#!/usr/bin/env Rscript
# admin_mechanism_selection_vs_quality.R
#
# Channel decomposition for the administrative-litigation effect.
# Estimates the LPM DID on government_win for three nested samples
# of the admin case file:
#   (a) all cases,
#   (b) cases that did not end in withdrawal (withdraw_case == 0),
#   (c) decisive cases (end_case == 0 AND withdraw_case == 0,
#       i.e. cases that progressed to a merits judgment).
# A coefficient that survives only in (a) is consistent with
# selection / withdrawal margin; coefficients that also survive in
# (b) and (c) are harder to attribute to selective dropping and are
# more consistent with genuine quality improvement.

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

read_admin <- function() {
  dt <- fread(input_file)
  dt <- dt[!is.na(court_std) & court_std != ""]
  dt[, city_id := .GRP, by = .(province, city)]
  dt[, court_id := .GRP, by = court_std]
  dt
}

est <- function(dt) {
  m <- feols(
    government_win ~ did_treatment + government_has_lawyer + opponent_has_lawyer +
      plaintiff_is_entity | court_id + year + cause_group,
    data = dt, cluster = ~ city_id + court_id
  )
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m),
       r2 = fitstat(m, "r2")[[1]])
}

main <- function() {
  dt <- read_admin()

  res_all <- est(dt)
  res_no_withdraw <- est(dt[withdraw_case == 0L])
  res_decisive <- est(dt[withdraw_case == 0L & end_case == 0L])

  share_withdraw <- mean(dt$withdraw_case == 1L)
  share_end <- mean(dt$end_case == 1L)
  share_decisive <- mean(dt$withdraw_case == 0L & dt$end_case == 0L)

  results <- list(All = res_all,
                  NoWithdraw = res_no_withdraw,
                  Decisive = res_decisive)
  col_labels <- c("All cases",
                  "Non-withdrawn cases",
                  "Decisive cases (merits judgment)")

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")

  coef_row <- sapply(results, fmt_cell)
  se_row <- sapply(results, fmt_se)
  obs_row <- sapply(results, function(r) fmt_int(r$n_obs))
  r2_row <- sapply(results, function(r) fmt_num(r$r2))

  share_cells <- c(
    sprintf("%.1f\\%%", 100 * mean(dt$government_win)),
    sprintf("%.1f\\%%", 100 * mean(dt[withdraw_case == 0L]$government_win)),
    sprintf("%.1f\\%%", 100 * mean(dt[withdraw_case == 0L & end_case == 0L]$government_win))
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Selection vs.\\ Quality: Procurement Effect on Government Win across Nested Case Samples}",
    "\\label{tab:admin_mechanism_selection_vs_quality}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    paste("Sample &", paste(col_labels, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Sample mean of government win &", paste(share_cells, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Government counsel control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Opposing counsel control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Plaintiff entity control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Court / Year / Cause-Group FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} Linear-probability coefficients on Treatment $\\times$ Post with the government-win indicator as the outcome.",
      "Column 1 uses all administrative cases; column 2 drops cases that ended in withdrawal (%.1f\\%% of all cases); column 3 further drops cases that ended without a merits judgment (column 3 retains %.1f\\%% of the original sample, the merits-decided subset).",
      "An effect that appears only in column 1 but not in columns 2--3 is consistent with selective withdrawal of cases the government would otherwise lose; an effect that survives in columns 2 and 3 is harder to attribute to that selection margin alone.",
      "Standard errors clustered by city and court.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ), 100 * share_withdraw, 100 * share_decisive),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "admin_mechanism_selection_vs_quality_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
