#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
admin_path <- file.path(root_dir, "data", "output data", "admin_case_level.csv")
city_path <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

stars <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

CASE_NO_PATTERN <- "\\((\\d{4})\\)([^\u884c\u5211\u6c11[:space:]]+?)(\u884c\u521d|\u884c\u7ec8|\u884c\u518d|\u884c\u7533|\u884c\u5176\u4ed6|\u5211\u521d|\u6c11\u521d|\u884c)(?:\u5b57\u7b2c)?(\\d+)\u53f7?"

parse_case_numbers <- function(case_no) {
  m <- stringr::str_match(case_no, CASE_NO_PATTERN)
  data.table(
    cn_year = suppressWarnings(as.integer(m[, 2])),
    court_code = m[, 3],
    procedure = m[, 4],
    seq = suppressWarnings(as.integer(m[, 5]))
  )
}

main <- function() {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr", repos = "https://cloud.r-project.org")
  }

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

  cy_admin <- cases[
    ,
    .(
      government_win_rate_w = sum(government_win * ipw_weight) / sum(ipw_weight),
      appeal_rate_w = sum(appealed * ipw_weight) / sum(ipw_weight),
      admin_case_n_w = sum(ipw_weight),
      mean_disclosure = sum(ipw_weight) / sum(1.0 / disclosure * 1.0)
    ),
    by = .(province, city, year)
  ]

  city <- fread(city_path)
  panel <- city[cy_admin, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_id := .GRP, by = .(province, city)]

  outcomes <- list(
    list(
      key = "government_win_rate",
      label = "Government Win Rate",
      base = "government_win_rate",
      weighted = "government_win_rate_w"
    ),
    list(
      key = "appeal_rate",
      label = "Appeal Rate",
      base = "appeal_rate",
      weighted = "appeal_rate_w"
    ),
    list(
      key = "admin_case_n",
      label = "Administrative Case Numbers",
      base = "admin_case_n",
      weighted = "admin_case_n_w"
    )
  )

  fit <- function(outcome_col, weights = NULL) {
    rhs <- "treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n"
    f <- as.formula(sprintf("%s ~ %s | city_id + year", outcome_col, rhs))
    if (is.null(weights)) {
      m <- feols(f, data = panel, cluster = ~ city_id)
    } else {
      m <- feols(f, data = panel, weights = panel[[weights]], cluster = ~ city_id)
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
    results[[paste0(spec$key, "_baseline")]] <- fit(spec$base)
    results[[paste0(spec$key, "_disclosure")]] <- fit(spec$weighted, weights = "admin_case_n_w")
  }

  col_keys <- c(
    "government_win_rate_baseline", "government_win_rate_disclosure",
    "appeal_rate_baseline", "appeal_rate_disclosure",
    "admin_case_n_baseline", "admin_case_n_disclosure"
  )
  outcome_short <- c(
    "Gov.\\ Win Rate", "Gov.\\ Win Rate",
    "Appeal Rate", "Appeal Rate",
    "Admin.\\ Cases", "Admin.\\ Cases"
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
    paste("Disclosure-Inverse Weight &", paste(weighted_yes, collapse = " & "), "\\\\"),
    paste("City-Year Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on Treatment $\\times$ Post from city-year two-way fixed-effects regressions on the administrative-litigation panel.",
      "Odd-numbered columns reproduce the unweighted specification of the main table.",
      "Even-numbered columns weight each city-year by the sum of inverse disclosure probabilities of its underlying cases, where each disclosure probability is the German-tank estimate $n / \\hat{K}$ for the case's (court, year, procedure) cell with $\\hat{K} = (n+1)/n \\cdot m - 1$ following Liu, Wang, and Lyu (2023, \\textit{Journal of Public Economics}).",
      "Weights are clipped at 20 to prevent extreme cells from dominating.",
      "All specifications include city and year fixed effects, log population, log GDP, log registered lawyers, and log court caseload.",
      "Cluster-robust standard errors at the city level are in parentheses.",
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
