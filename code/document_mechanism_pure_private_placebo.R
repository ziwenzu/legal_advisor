#!/usr/bin/env Rscript
# document_mechanism_pure_private_placebo.R
#
# Channel test: re-estimates the document-level Winner x Post DID
# on the subsample of *pure private* civil cases, defined as
# documents with both plaintiff_party_is_entity == 0 and
# defendant_party_is_entity == 0 (individuals on both sides).
# These cases have no organisational party that the procurement
# winner could be carrying over from the government-counsel role,
# so a Winner x Post effect of the same sign as the main table
# would be consistent with the firm-capture story (H3).

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

  pure_private <- dt[plaintiff_party_is_entity == 0L &
                       defendant_party_is_entity == 0L]
  pure_decisive <- pure_private[case_decisive == 1L]
  pure_fee <- pure_decisive[!is.na(case_win_rate_fee)]

  fit <- function(data, outcome) {
    work <- data[!is.na(get(outcome))]
    rhs <- "did_treatment + opponent_has_lawyer + lawyer_practice_years_std + lawyer_practice_years_obs"
    f <- as.formula(sprintf("%s ~ %s | firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe", outcome, rhs))
    m <- feols(f, data = work, cluster = ~ firm_id + stack_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "did_treatment"]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]], n_obs = nobs(m),
         r2 = fitstat(m, "r2")[[1]])
  }

  res_share <- fit(pure_private, "legal_reasoning_share")
  res_length <- fit(pure_private, "log_legal_reasoning_length_chars")
  res_win <- fit(pure_decisive, "case_win_binary")
  res_fee <- fit(pure_fee, "case_win_rate_fee")

  results <- list(res_share, res_length, res_win, res_fee)
  outcome_labels <- c("Reasoning Share",
                      "log(Reasoning Length + 1)",
                      "Case Win Binary",
                      "Case Fee Win Rate")

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
    "\\caption{Document-Level Winner $\\times$ Post on the Pure-Private Civil-Case Subsample}",
    "\\label{tab:document_mechanism_pure_private_placebo}",
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
    paste("Case / Lawyer Controls &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Firm / Stack$\\times$Year / Cause$\\times$Side / Court / Year$\\times$Gender FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Document-level DID coefficients on Winner $\\times$ Post estimated only on civil documents in which both the plaintiff and the defendant party are individuals (\\texttt{plaintiff\\_party\\_is\\_entity} = 0 and \\texttt{defendant\\_party\\_is\\_entity} = 0).",
      "Such cases have no organisational party that could mechanically benefit from the firm's government-counsel role, so a coefficient of the same sign and order of magnitude as in the full-document table is consistent with a firm-level skill or routine that travels across case types.",
      "Outcomes follow the main document-level table; case controls cover opposing-side representation; lawyer controls cover standardized practice years and a missing-practice-years indicator.",
      "Standard errors clustered by firm and stack.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "document_mechanism_pure_private_placebo_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
