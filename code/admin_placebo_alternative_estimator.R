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

stars <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
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
    row <- ct[1, ]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]],
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
    "\\centering",
    "\\caption{Placebo Outcomes and Alternative Staggered-DID Estimator}",
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
      "\\item \\textit{Notes:}",
      "Panel A reports the Treatment $\\times$ Post coefficient when the dependent variable is the within-city-year share of administrative cases that are withdrawn or that end without a judgment on the merits.",
      "If the headline win-rate effect were generated by strategic government settlement or pressure-to-withdraw rather than by stronger in-court representation, these process margins should respond to procurement.",
      "Panel B reports the same coefficient on six cause-group shares.",
      "Coefficients close to zero on these placebos indicate that procurement does not measurably reshape the composition of cases that reach a judgment.",
      "Panel C re-estimates the three headline outcomes with the Sun and Abraham (2021) interaction-weighted estimator, which aggregates cohort-specific event-study coefficients with weights that are robust to heterogeneous treatment effects under staggered adoption.",
      "All specifications include city-year controls for log population, log GDP, log registered lawyers, and log court caseload, plus city and year fixed effects.",
      "Cluster-robust standard errors at the city level are in parentheses.",
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
