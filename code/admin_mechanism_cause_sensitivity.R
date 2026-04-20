#!/usr/bin/env Rscript
# admin_mechanism_cause_sensitivity.R
#
# Tests whether the procurement effect on government win rates is
# concentrated in politically sensitive case categories. Defines
# "high-sensitivity" as cause_group in {expropriation, land_planning}
# and "low-sensitivity" as {labor_social, permitting_review}, then:
#   (1) Reports the LPM ATT on government_win separately for the
#       high- and low-sensitivity subsamples (admin case-level).
#   (2) Estimates a pooled interaction model on the case panel:
#       government_win ~ did_treatment * sensitivity_high.
#   (3) Reports the equality-test p-value for the two subsample
#       coefficients.

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
input_file <- file.path(root_dir, "data", "admin_case_level.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

HIGH_GROUPS <- c("expropriation", "land_planning")
LOW_GROUPS <- c("labor_social", "permitting_review")

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
fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("$<0.001$")
  sprintf("%.3f", p)
}

main <- function() {
  dt <- fread(input_file)
  dt <- dt[!is.na(court_std) & court_std != ""]
  dt[, city_id := .GRP, by = .(province, city)]
  dt[, court_id := .GRP, by = court_std]
  dt[, sensitivity_high := as.integer(cause_group %in% HIGH_GROUPS)]
  dt[, sensitivity_low := as.integer(cause_group %in% LOW_GROUPS)]

  est <- function(sub) {
    m <- feols(
      government_win ~ did_treatment + government_has_lawyer + opponent_has_lawyer +
        plaintiff_is_entity | court_id + year + cause_group,
      data = sub, cluster = ~ city_id + court_id
    )
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "did_treatment"]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]], n_obs = nobs(m))
  }

  res_high <- est(dt[sensitivity_high == 1L])
  res_low <- est(dt[sensitivity_low == 1L])

  pooled_dt <- dt[sensitivity_high == 1L | sensitivity_low == 1L]
  m_int <- feols(
    government_win ~ did_treatment + did_treatment:sensitivity_high +
      sensitivity_high + government_has_lawyer + opponent_has_lawyer +
      plaintiff_is_entity | court_id + year + cause_group,
    data = pooled_dt, cluster = ~ city_id + court_id
  )
  ct_int <- as.data.table(coeftable(m_int), keep.rownames = "term")
  diff_row <- ct_int[term %in% c("did_treatment:sensitivity_high",
                                  "sensitivity_high:did_treatment")][1]
  diff_p <- diff_row[["Pr(>|t|)"]]

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")

  rows <- c(
    paste("Treatment $\\times$ Post &",
          fmt_cell(res_high), "&", fmt_cell(res_low), "\\\\"),
    paste("&", fmt_se(res_high), "&", fmt_se(res_low), "\\\\"),
    "\\addlinespace",
    paste("Observations &", fmt_int(res_high$n_obs), "&", fmt_int(res_low$n_obs), "\\\\")
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Procurement Effect on Government Win by Political Sensitivity of Cause Group}",
    "\\label{tab:admin_mechanism_cause_sensitivity}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    "Subsample & High sensitivity & Low sensitivity \\\\",
    " & (Expropriation, Land/Planning) & (Labor/Social, Permitting) \\\\",
    "\\midrule",
    rows,
    paste("Equality-test $p$ (cols.\\ 1 = 2) &",
          sprintf("\\multicolumn{2}{c}{%s}", fmt_p(diff_p)),
          "\\\\"),
    paste("Government / Opposing counsel controls &",
          paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Plaintiff entity control &",
          paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Court / Year / Cause-Group FE &",
          paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Linear-probability coefficients on Treatment $\\times$ Post with the government-win indicator as the outcome.",
      "Column 1 keeps administrative cases in the politically sensitive cause groups (expropriation and compensation; land and planning); column 2 keeps the lower-sensitivity service-delivery cause groups (labor and social security; permitting and review).",
      "The equality-test row reports the two-sided $p$-value for $H_0$: column 1 coefficient = column 2 coefficient, estimated from a pooled interaction specification with $\\text{did\\_treatment} \\times \\text{sensitivity\\_high}$ on cases in either group, with the same controls and fixed effects, clustered by city and court.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "admin_mechanism_cause_sensitivity_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
