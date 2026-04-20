#!/usr/bin/env Rscript
# document_mechanism_reasoning_decomposition.R
#
# Document-style decomposition. The two existing outcomes are
#   - legal_reasoning_share = reasoning_length / total_length
#   - log_legal_reasoning_length_chars = log(reasoning_length + 1)
# We back out reasoning_length and total_length per row,
#   reasoning_length = exp(log_legal_reasoning_length_chars) - 1
#   total_length     = reasoning_length / legal_reasoning_share
# (left as NA when the share is zero or undefined), then construct
#   log_total_length = log(total_length + 1)
#   log_other_length = log(other_length + 1) where other = total - reasoning.
# The script then runs the standard Winner x Post DID on three
# decomposed outcomes and on the total length, so a reader can see
# whether the reasoning-share effect comes from longer reasoning
# sections, longer non-reasoning sections, or both.

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
input_file <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
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

main <- function() {
  dt <- fread(input_file)
  dt[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  dt[, cause_side_fe := sprintf("%s__%s", cause, side)]
  dt[, lawyer_practice_years_obs := as.integer(!is.na(lawyer_practice_years))]
  practice_mean <- mean(dt$lawyer_practice_years, na.rm = TRUE)
  practice_sd <- sd(dt$lawyer_practice_years, na.rm = TRUE)
  dt[, lawyer_practice_years_std := (lawyer_practice_years - practice_mean) / practice_sd]
  dt[is.na(lawyer_practice_years_std), lawyer_practice_years_std := 0]
  dt[, year_gender_fe := sprintf("%s__%d", year, lawyer_gender)]

  dt[, reasoning_length := pmax(0, exp(log_legal_reasoning_length_chars) - 1)]
  dt[, total_length := fifelse(legal_reasoning_share > 0,
                               reasoning_length / legal_reasoning_share,
                               NA_real_)]
  dt[, other_length := pmax(0, total_length - reasoning_length)]
  dt[, log_total_length := log(total_length + 1)]
  dt[, log_other_length := log(other_length + 1)]

  fit <- function(outcome, sample_filter = NULL) {
    work <- dt[!is.na(get(outcome))]
    if (!is.null(sample_filter)) work <- work[eval(sample_filter)]
    rhs <- "did_treatment + opponent_has_lawyer + plaintiff_party_is_entity + defendant_party_is_entity + lawyer_practice_years_std + lawyer_practice_years_obs"
    f <- as.formula(sprintf(
      "%s ~ %s | firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe",
      outcome, rhs
    ))
    m <- feols(f, data = work, cluster = ~ firm_id + stack_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "did_treatment"]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]], n_obs = nobs(m),
         r2 = fitstat(m, "r2")[[1]])
  }

  res_share <- fit("legal_reasoning_share")
  res_log_reason <- fit("log_legal_reasoning_length_chars")
  res_log_total <- fit("log_total_length")
  res_log_other <- fit("log_other_length")

  results <- list(res_share, res_log_reason, res_log_total, res_log_other)
  outcome_labels <- c("Reasoning Share",
                      "log(Reasoning Length + 1)",
                      "log(Total Length + 1)",
                      "log(Non-Reasoning Length + 1)")

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")
  coef_row <- sapply(results, fmt_cell)
  se_row <- sapply(results, fmt_se)
  obs_row <- sapply(results, function(r) fmt_int(r$n_obs))
  r2_row <- sapply(results, function(r) fmt_num(r$r2))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Decomposing the Reasoning-Share Effect into Reasoning vs Non-Reasoning Length}",
    "\\label{tab:document_mechanism_reasoning_decomposition}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) \\\\",
    paste("Outcome &", paste(outcome_labels, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Lawyer Controls &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Firm / Stack$\\times$Year / Cause$\\times$Side / Court / Year$\\times$Gender FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Document-level DID coefficients on Winner $\\times$ Post.",
      "Reasoning length is recovered as $\\exp(\\texttt{log\\_legal\\_reasoning\\_length\\_chars}) - 1$, total length as reasoning length divided by \\texttt{legal\\_reasoning\\_share} (left undefined when the share is zero), and non-reasoning length as the difference; both are re-expressed as $\\log(x + 1)$.",
      "Comparing columns 2--4 with column 1 separates whether the share decline reflects shorter reasoning sections, longer non-reasoning sections, or a change in document length composition.",
      "Same case and lawyer controls and same fixed effects as the main document-level table.",
      "Standard errors clustered by firm and stack.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "document_mechanism_reasoning_decomposition_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
