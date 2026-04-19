#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(stringr)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
admin_path <- file.path(root_dir, "data", "admin_case_level.csv")
city_path <- file.path(root_dir, "data", "city_year_panel.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

CASE_NO_PATTERN <- "\\((\\d{4})\\)([^\u884c\u5211\u6c11[:space:]]+?)(\u884c\u521d|\u884c\u7ec8|\u884c\u518d|\u884c\u7533|\u884c\u5176\u4ed6|\u5211\u521d|\u6c11\u521d|\u884c)(?:\u5b57\u7b2c)?(\\d+)\u53f7?"

stars <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

parse_case_numbers <- function(case_no) {
  m <- str_match(case_no, CASE_NO_PATTERN)
  data.table(
    cn_year = suppressWarnings(as.integer(m[, 2])),
    court_code = m[, 3],
    procedure = m[, 4],
    seq = suppressWarnings(as.integer(m[, 5]))
  )
}

main <- function() {
  cases <- fread(admin_path)
  parsed <- parse_case_numbers(cases$case_no)
  cases[, c("cn_year", "court_code", "procedure", "seq") := parsed]
  cases <- cases[!is.na(seq) & !is.na(court_code) & !is.na(procedure) & seq <= 50000]

  cell <- cases[, .(n_obs = .N, m_max = max(seq)),
                by = .(court_code, cn_year, procedure)]
  cell[, k_hat := pmax(n_obs, ((n_obs + 1) / n_obs) * m_max - 1)]
  cell[, disclosure := pmin(1.0, n_obs / k_hat)]

  cases <- cell[cases, on = c("court_code", "cn_year", "procedure")]
  cases[, disclosure := pmin(1.0, pmax(0.05, disclosure))]
  cases[, ipw_weight := pmin(20, 1.0 / disclosure)]

  cy_weights <- cases[
    ,
    .(disclosure_weight = sum(ipw_weight)),
    by = .(province, city, year)
  ]

  city <- fread(city_path)
  panel <- city[cy_weights, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_id := .GRP, by = .(province, city)]

  outcomes <- list(
    list(key = "government_win_rate", label = "Gov.\\ Win Rate"),
    list(key = "appeal_rate", label = "Appeal Rate"),
    list(key = "admin_case_n", label = "Admin.\\ Cases")
  )

  fit <- function(outcome_col, weighted = FALSE) {
    rhs <- "treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n"
    f <- as.formula(sprintf("%s ~ %s | city_id + year", outcome_col, rhs))
    if (!weighted) {
      m <- feols(f, data = panel, cluster = ~ city_id)
    } else {
      m <- feols(f, data = panel, weights = panel[["disclosure_weight"]], cluster = ~ city_id)
    }
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "treatment"]
    list(
      estimate = row[["Estimate"]],
      se = row[["Std. Error"]],
      p_value = row[["Pr(>|t|)"]],
      n_obs = nobs(m),
      r2 = fitstat(m, "r2")[[1]]
    )
  }

  results <- list()
  for (spec in outcomes) {
    results[[paste0(spec$key, "_baseline")]] <- fit(spec$key, weighted = FALSE)
    results[[paste0(spec$key, "_disclosure")]] <- fit(spec$key, weighted = TRUE)
  }

  col_keys <- c(
    "government_win_rate_baseline", "government_win_rate_disclosure",
    "appeal_rate_baseline", "appeal_rate_disclosure",
    "admin_case_n_baseline", "admin_case_n_disclosure"
  )
  outcome_short <- c(
    "Government Win Rate", "Government Win Rate",
    "Appeal Rate", "Appeal Rate",
    "Administrative Cases", "Administrative Cases"
  )
  weighted_yes <- c("", "Yes", "", "Yes", "", "Yes")

  coef_row <- sapply(col_keys, function(k) {
    res <- results[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(col_keys, function(k) paste0("(", fmt_num(results[[k]]$se), ")"))
  obs_row <- sapply(col_keys, function(k) fmt_int(results[[k]]$n_obs))
  r2_row <- sapply(col_keys, function(k) fmt_num(results[[k]]$r2))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Disclosure-Weighted Robustness for the City-Year Administrative Estimates}",
    "\\label{tab:city_year_disclosure_weighted_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
    paste("Outcome &", paste(outcome_short, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Disclosure-inverse weight &", paste(weighted_yes, collapse = " & "), "\\\\"),
    paste("Controls (city-year) &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City Fixed Effects &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Year Fixed Effects &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Outcomes match the main city-year table.",
      "Even columns weight each city-year by its disclosure-corrected case count $\\sum_j 1/\\hat{p}_j$, where $\\hat{p}_j = n_j / \\hat{K}_j$ is the German-tank disclosure share for case $j$'s (court, year, procedure) cell with $\\hat{K}_j = (n_j+1)/n_j \\cdot m_j - 1$ (Liu, Wang, and Lyu 2023, \\textit{Journal of Public Economics}); per-case weights are clipped at 20.",
      "The disclosure correction enters as a regression weight only; the dependent variables match the baseline columns.",
      "City-year controls: log population, log GDP, log registered lawyers, log court caseload.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, "city_year_disclosure_weighted_appendix_table.tex")
  writeLines(lines, out_path)
  cat("Wrote", out_path, "\n")
}

main()
