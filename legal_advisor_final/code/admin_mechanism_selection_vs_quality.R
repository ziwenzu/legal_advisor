#!/usr/bin/env Rscript
# admin_mechanism_selection_vs_quality.R
#
# Conditional comparisons across administrative-case samples.
# Estimates the LPM DID on government_win for three nested samples
# of the admin case file:
#   (a) all cases,
#   (b) cases that did not end in withdrawal (withdraw_case == 0),
#   (c) decisive cases (end_case == 0 AND withdraw_case == 0,
#       i.e. cases that progressed to a merits judgment).
# Withdrawal and procedural exit can respond to procurement. Comparisons
# across these samples therefore describe selected outcomes; they do not
# isolate litigation capacity or decompose the overall effect into channels.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# --- locale guard for portable UTF-8 reads (RStudio + Rscript safe) ---
local({
  utf8_locales <- c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")
  for (loc in utf8_locales) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG = loc, LC_ALL = loc, LC_CTYPE = loc, LC_COLLATE = loc)
      break
    }
  }
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
setFixest_nthreads(4)

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
  dt <- fread(input_file, encoding = "UTF-8")
  dt <- dt[!is.na(court_std) & court_std != ""]
  dt[, city_id := .GRP, by = .(province, city)]
  dt[, court_id := .GRP, by = court_std]
  dt
}

est <- function(dt, outcome = "government_win") {
  f <- as.formula(sprintf(
    "%s ~ did_treatment + government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity | court_id + year + cause_group",
    outcome
  ))
  m <- feols(f, data = dt, cluster = ~ city_id + court_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m),
       r2 = fitstat(m, "r2")[[1]])
}

main <- function() {
  dt <- read_admin()

  # Panel A: government_win across nested case samples
  res_all <- est(dt, "government_win")
  res_no_withdraw <- est(dt[withdraw_case == 0L], "government_win")
  res_decisive <- est(dt[withdraw_case == 0L & end_case == 0L], "government_win")

  # Panel B: changes in recorded case dispositions. These indicators can
  # overlap and do not identify plaintiffs' reasons for ending a case.
  res_withdraw <- est(dt, "withdraw_case")
  res_end      <- est(dt, "end_case")
  res_appeal   <- est(dt, "appealed")

  share_withdraw <- mean(dt$withdraw_case == 1L)
  share_end <- mean(dt$end_case == 1L)
  share_decisive <- mean(dt$withdraw_case == 0L & dt$end_case == 0L)

  results <- list(All = res_all,
                  NoWithdraw = res_no_withdraw,
                  Decisive = res_decisive)
  col_labels <- c("All cases",
                  "Non-withdrawn cases",
                  "Merits cases")
  results_b <- list(Withdraw = res_withdraw,
                    End = res_end,
                    Appeal = res_appeal)
  panel_b_labels <- c("Withdrawal",
                      "Procedural end",
                      "Appeal")

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")

  coef_row <- sapply(results, fmt_cell)
  se_row <- sapply(results, fmt_se)
  obs_row <- sapply(results, function(r) fmt_int(r$n_obs))
  r2_row <- sapply(results, function(r) fmt_num(r$r2))

  coef_row_b <- sapply(results_b, fmt_cell)
  se_row_b <- sapply(results_b, fmt_se)
  obs_row_b <- sapply(results_b, function(r) fmt_int(r$n_obs))
  r2_row_b <- sapply(results_b, function(r) fmt_num(r$r2))

  share_cells <- c(
    sprintf("%.1f\\%%", 100 * mean(dt$government_win)),
    sprintf("%.1f\\%%", 100 * mean(dt[withdraw_case == 0L]$government_win)),
    sprintf("%.1f\\%%", 100 * mean(dt[withdraw_case == 0L & end_case == 0L]$government_win))
  )
  share_cells_b <- c(
    sprintf("%.1f\\%%", 100 * mean(dt$withdraw_case)),
    sprintf("%.1f\\%%", 100 * mean(dt$end_case)),
    sprintf("%.1f\\%%", 100 * mean(dt$appealed))
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Administrative Outcomes across Case Samples and Dispositions}",
    "\\label{tab:admin_mechanism_selection_vs_quality}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "\\midrule",
    "\\multicolumn{4}{l}{\\textit{Panel A. Outcome = Government Win across nested case samples}} \\\\",
    "\\addlinespace",
    paste("Sample &", paste(col_labels, collapse = " & "), "\\\\"),
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    paste("Sample mean of government win &", paste(share_cells, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    "\\midrule",
    "\\multicolumn{4}{l}{\\textit{Panel B. Outcome = case-margin indicators (all cases)}} \\\\",
    "\\addlinespace",
    paste("Outcome &", paste(panel_b_labels, collapse = " & "), "\\\\"),
    paste("Treatment $\\times$ Post &", paste(coef_row_b, collapse = " & "), "\\\\"),
    paste("&", paste(se_row_b, collapse = " & "), "\\\\"),
    paste("Sample mean of outcome &", paste(share_cells_b, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_row_b, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row_b, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Government counsel control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Opposing counsel control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Plaintiff entity control &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Court / Year / Cause-Group FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} Linear-probability coefficients on Treatment $\\times$ Post.",
      "Panel A compares all cases, non-withdrawn cases, and cases with neither withdrawal nor procedural termination (%.1f\\%% of the sample). These are conditional comparisons: procurement may change which cases reach judgment, so differences across columns do not isolate capacity or decompose mechanisms.",
      "Panel B reports withdrawal, procedural termination, and appeal indicators; withdrawal and procedural termination are mutually exclusive. These outcomes do not reveal why plaintiffs exit.",
      "Controls include government counsel, opposing counsel, and plaintiff entity status, which may themselves respond to procurement.",
      "Standard errors clustered by city and court.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ), 100 * share_decisive),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "admin_mechanism_selection_vs_quality_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
