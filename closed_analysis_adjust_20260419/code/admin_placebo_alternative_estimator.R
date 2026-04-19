#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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

stars <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
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

build_city_year <- function(admin) {
  agg <- admin[
    ,
    .(
      withdraw_rate = mean(withdraw_case, na.rm = TRUE),
      end_rate = mean(end_case, na.rm = TRUE),
      share_expropriation = mean(cause_group == "expropriation"),
      share_land_planning = mean(cause_group == "land_planning"),
      share_public_security = mean(cause_group == "public_security"),
      share_enforcement = mean(cause_group == "enforcement"),
      share_permitting = mean(cause_group == "permitting_review"),
      share_labor_social = mean(cause_group == "labor_social"),
      n_cases = .N
    ),
    by = .(province, city, year)
  ]
  agg
}

main <- function() {
  admin <- fread(admin_path)
  city <- fread(city_path)
  agg <- build_city_year(admin)
  panel <- city[agg, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_id := .GRP, by = .(province, city)]
  panel[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  panel[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(year[treatment == 1L]), 10000L),
    by = city_id
  ]

  estimate <- function(outcome) {
    f <- as.formula(sprintf(
      "%s ~ treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n | city_id + year",
      outcome
    ))
    m <- feols(f, data = panel, cluster = ~ city_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "treatment"]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]],
         n_obs = nobs(m), r2 = fitstat(m, "r2")[[1]])
  }

  placebo_outcomes <- list(
    list(label = "Case withdrawal rate", col = "withdraw_rate"),
    list(label = "End-without-judgment rate", col = "end_rate"),
    list(label = "Share: expropriation", col = "share_expropriation"),
    list(label = "Share: land/planning", col = "share_land_planning"),
    list(label = "Share: public security", col = "share_public_security"),
    list(label = "Share: enforcement", col = "share_enforcement"),
    list(label = "Share: permitting", col = "share_permitting"),
    list(label = "Share: labor/social", col = "share_labor_social")
  )

  placebo_lines <- vapply(placebo_outcomes, function(spec) {
    res <- estimate(spec$col)
    paste(
      spec$label, "&",
      paste0(fmt_num(res$estimate), stars(res$p_value)), "&",
      paste0("(", fmt_num(res$se), ")"), "&",
      fmt_int(res$n_obs), "&",
      fmt_num(res$r2),
      "\\\\"
    )
  }, character(1))

  estimate_sunab <- function(outcome) {
    panel_sa <- copy(panel)
    panel_sa[ever_treated == 0L, first_treat_year := 10000L]
    f <- as.formula(sprintf(
      "%s ~ sunab(first_treat_year, year) + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n | city_id + year",
      outcome
    ))
    m <- feols(f, data = panel_sa, cluster = ~ city_id)
    agg <- summary(m, agg = "att")
    ct <- as.data.table(coeftable(agg), keep.rownames = "term")
    row <- ct[grepl("ATT|att", term)]
    if (nrow(row) == 0) row <- ct[1, ]
    list(estimate = row[["Estimate"]][1], se = row[["Std. Error"]][1],
         p_value = row[["Pr(>|t|)"]][1],
         n_obs = nobs(m), r2 = fitstat(m, "r2")[[1]])
  }

  sun_outcomes <- list(
    list(label = "Government Win Rate", col = "government_win_rate"),
    list(label = "Appeal Rate", col = "appeal_rate"),
    list(label = "Administrative Cases", col = "admin_case_n")
  )

  sun_lines <- vapply(sun_outcomes, function(spec) {
    res <- estimate_sunab(spec$col)
    paste(
      spec$label, "&",
      paste0(fmt_num(res$estimate), stars(res$p_value)), "&",
      paste0("(", fmt_num(res$se), ")"), "&",
      fmt_int(res$n_obs), "&",
      fmt_num(res$r2),
      "\\\\"
    )
  }, character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Placebo Outcomes and Alternative Staggered Difference-in-Differences Estimator}",
    "\\label{tab:admin_placebo_alternative_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Outcome & Coefficient & SE & Observations & $R^2$ \\\\",
    "\\midrule",
    "\\multicolumn{5}{l}{\\textit{Panel A. Process-margin placebo}} \\\\",
    placebo_lines[1:2],
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel B. Cause-mix stability placebo}} \\\\",
    placebo_lines[3:8],
    "\\addlinespace",
    "\\multicolumn{5}{l}{\\textit{Panel C. Sun and Abraham (2021) interaction-weighted ATT}} \\\\",
    sun_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Panel A outcomes are the within-city-year share of cases that are withdrawn (row 1) or that end without a judgment on the merits (row 2).",
      "Panel B outcomes are six cause-group shares of administrative cases at the city-year; the six shares are not independent because they sum to one minus the residual category that is omitted to avoid collinearity.",
      "Panel C re-estimates the three headline outcomes with the Sun and Abraham (2021) interaction-weighted estimator.",
      "City-year controls: log population, log GDP, log registered lawyers, log court caseload.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "admin_placebo_alternative_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
