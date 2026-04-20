#!/usr/bin/env Rscript
# back_of_envelope_substantive.R
#
# Computes the substantive-interpretation numbers required to
# translate the headline city-year procurement effects into
# concrete counts of cases and into percentile shifts in the
# pre-period outcome distribution. Outputs:
#   (a) An exported tex table with one row per outcome reporting
#       the headline coefficient, the implied per-city-year change
#       in the outcome, the implied total count change over treated
#       city-years, and the implied percentile shift in the
#       pre-period distribution.
#   (b) A plain-text dump of the same numbers under
#       output/tables/back_of_envelope_substantive.txt for direct
#       quotation in the manuscript.

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

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

twfe_coef <- function(panel, outcome) {
  rhs <- paste(c("treatment", preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]])
}

main <- function() {
  panel <- fread(city_path)
  panel[, city_id := .GRP, by = .(province, city)]
  treated_panel <- panel[treatment == 1L]
  treated_city_years <- nrow(treated_panel)
  mean_admin_n <- mean(panel$admin_case_n)
  mean_admin_n_treated <- mean(treated_panel$admin_case_n)

  outcomes <- list(
    list(label = "Government Win Rate", col = "government_win_rate",
         scale = "share", base_for_count = "admin"),
    list(label = "Appeal Rate", col = "appeal_rate",
         scale = "share", base_for_count = "admin"),
    list(label = "Administrative Cases", col = "admin_case_n",
         scale = "count", base_for_count = "self")
  )

  rows <- character(0)
  txt <- character(0)
  for (spec in outcomes) {
    res <- twfe_coef(panel, spec$col)
    pre_treated <- panel[treatment == 0L]
    p25 <- quantile(pre_treated[[spec$col]], 0.25, na.rm = TRUE)
    p75 <- quantile(pre_treated[[spec$col]], 0.75, na.rm = TRUE)
    p90 <- quantile(pre_treated[[spec$col]], 0.90, na.rm = TRUE)
    median_pre <- median(pre_treated[[spec$col]], na.rm = TRUE)
    if (spec$scale == "share") {
      per_city_year <- res$estimate * mean_admin_n_treated
      total_change <- res$estimate * sum(treated_panel$admin_case_n)
    } else {
      per_city_year <- res$estimate
      total_change <- res$estimate * treated_city_years
    }
    p_level <- mean(pre_treated[[spec$col]] <= median_pre + res$estimate, na.rm = TRUE)
    rows <- c(rows, paste(
      spec$label, "&",
      paste0(fmt_num(res$estimate), stars(res$p_value)), "&",
      fmt_num(per_city_year, 1), "&",
      fmt_int(total_change), "&",
      sprintf("%.0f$\\to$%.0f pctile",
              100 * mean(pre_treated[[spec$col]] <= median_pre, na.rm = TRUE),
              100 * p_level),
      "\\\\"
    ))
    txt <- c(txt,
             sprintf("%s: TWFE = %.4f (SE %.4f); per treated city-year change = %.2f units; total over %d treated city-years = %.0f units; pre-period quartiles (P25/P50/P75/P90) = (%.3f / %.3f / %.3f / %.3f); coefficient shifts the median pre-period city-year to the %.0fth percentile of the pre-period distribution.",
                     spec$label, res$estimate, res$se,
                     per_city_year, treated_city_years, total_change,
                     p25, median_pre, p75, p90,
                     100 * p_level))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Substantive Magnitude of the City-Year Procurement Effects}",
    "\\label{tab:back_of_envelope_substantive_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Outcome & TWFE coef. & Per treated city-year & Total over treated city-years & Pre-period percentile shift \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} For Government Win Rate and Appeal Rate the per-city-year change multiplies the rate coefficient by the average number of administrative cases per treated city-year (%.0f cases) and the total multiplies by the total caseload across treated city-years (%s cases over %d treated city-years).",
      "For Administrative Cases the per-city-year change is the coefficient itself, and the total multiplies it by the %d treated city-years.",
      "Pre-period percentile shifts are computed against the empirical distribution of the outcome across never-treated city-years, asking where the pre-period median moves to once the coefficient is added."
    ),
    mean_admin_n_treated,
    fmt_int(sum(treated_panel$admin_case_n)),
    treated_city_years,
    treated_city_years),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_tex <- file.path(table_dir, "back_of_envelope_substantive_appendix_table.tex")
  writeLines(lines, out_tex)
  cat("Wrote", out_tex, "\n")

  out_txt <- file.path(table_dir, "back_of_envelope_substantive.txt")
  writeLines(txt, out_txt)
  cat("Wrote", out_txt, "\n")
}

main()
