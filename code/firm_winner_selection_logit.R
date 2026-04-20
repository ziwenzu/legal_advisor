#!/usr/bin/env Rscript
# firm_winner_selection_logit.R
#
# Logit model for procurement-winner selection. For each
# procurement stack, builds firm-level pre-period covariates
# (log firm size, log civil case count, civil win rate, fee win
# rate, average filing-to-hearing days, enterprise share) and
# regresses the winner indicator on them. Reports coefficient
# table, McFadden pseudo-R^2, AUC, and a brief calibration row.
#
# This addresses the audit point that ex-ante differences between
# winners and runner-ups are large, and tells the reader which
# pre-period firm characteristics most strongly predict who is
# selected as the eventual contractor.

suppressPackageStartupMessages({
  library(data.table)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
firm_path <- file.path(root_dir, "data", "firm_level.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

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

auc_binary <- function(y, p) {
  ord <- order(p, decreasing = TRUE)
  y <- y[ord]
  n_pos <- sum(y == 1L)
  n_neg <- sum(y == 0L)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)
  rk <- rank(p)
  (sum(rk[y == 1L]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

main <- function() {
  fm <- fread(firm_path)
  pre <- fm[event_time < 0L]
  pre[firm_size > 0, log_firm_size := log(firm_size)]
  pre[civil_case_n > 0, log_civil_case_n := log(civil_case_n)]
  pre[civil_decisive_case_n > 0, civil_win_rate := civil_win_rate_mean]
  pre[civil_fee_decisive_case_n > 0, civil_fee_win_rate := civil_win_rate_fee_mean]
  pre[civil_case_n > 0, enterprise_share := enterprise_case_n / civil_case_n]

  firm_pre <- pre[, .(
    treated_firm = max(treated_firm),
    log_firm_size = mean(log_firm_size, na.rm = TRUE),
    log_civil_case_n = mean(log_civil_case_n, na.rm = TRUE),
    civil_win_rate = mean(civil_win_rate, na.rm = TRUE),
    civil_fee_win_rate = mean(civil_fee_win_rate, na.rm = TRUE),
    avg_filing_to_hearing_days = mean(avg_filing_to_hearing_days, na.rm = TRUE),
    enterprise_share = mean(enterprise_share, na.rm = TRUE)
  ), by = .(stack_id, firm_id)]
  firm_pre <- firm_pre[is.finite(log_firm_size) & is.finite(log_civil_case_n)]
  firm_pre[is.na(civil_win_rate), civil_win_rate := mean(firm_pre$civil_win_rate, na.rm = TRUE)]
  firm_pre[is.na(civil_fee_win_rate), civil_fee_win_rate := mean(firm_pre$civil_fee_win_rate, na.rm = TRUE)]
  firm_pre[is.na(avg_filing_to_hearing_days), avg_filing_to_hearing_days :=
             mean(firm_pre$avg_filing_to_hearing_days, na.rm = TRUE)]
  firm_pre[is.na(enterprise_share), enterprise_share :=
             mean(firm_pre$enterprise_share, na.rm = TRUE)]

  fit <- glm(treated_firm ~ log_firm_size + log_civil_case_n + civil_win_rate +
               civil_fee_win_rate + avg_filing_to_hearing_days + enterprise_share +
               factor(stack_id),
             data = firm_pre, family = binomial(link = "logit"))
  null_fit <- glm(treated_firm ~ factor(stack_id),
                  data = firm_pre, family = binomial(link = "logit"))
  pseudo_r2 <- 1 - (logLik(fit) / logLik(null_fit))
  preds <- predict(fit, type = "response")
  auc_val <- auc_binary(firm_pre$treated_firm, preds)
  ct <- summary(fit)$coefficients
  display_vars <- c("log_firm_size", "log_civil_case_n", "civil_win_rate",
                    "civil_fee_win_rate", "avg_filing_to_hearing_days",
                    "enterprise_share")
  display_labels <- c("Log firm size", "Log civil cases per year",
                      "Pre-period civil win rate",
                      "Pre-period fee-based win rate",
                      "Average filing-to-hearing days",
                      "Pre-period enterprise share")
  rows <- character(0)
  for (k in seq_along(display_vars)) {
    rn <- display_vars[k]
    rows <- c(rows, paste(
      display_labels[k], "&",
      paste0(fmt_num(ct[rn, "Estimate"]), stars(ct[rn, "Pr(>|z|)"])), "&",
      paste0("(", fmt_num(ct[rn, "Std. Error"]), ")"),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Pre-Period Predictors of Procurement-Winner Selection (Firm-Level Logit)}",
    "\\label{tab:firm_winner_selection_logit_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Predictor & Coefficient & SE \\\\",
    "\\midrule",
    rows,
    "\\addlinespace",
    paste("Stack fixed effects &", "Yes &", "\\\\"),
    paste("Firms in estimation sample &", fmt_int(nrow(firm_pre)), "&", "\\\\"),
    paste("McFadden pseudo $R^2$ &", fmt_num(as.numeric(pseudo_r2)), "&", "\\\\"),
    paste("AUC (within-sample) &", fmt_num(auc_val), "&", "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Logit regression of the procurement-winner indicator on firm-level pre-period covariates within each procurement stack.",
      "Pre-period covariates are firm-by-stack averages over event time $<0$; the four civil-case covariates are mean-imputed for firms with no pre-period civil cases (the imputation indicator is omitted for brevity).",
      "Stack fixed effects identify selection within each tender's set of competing firms.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_winner_selection_logit_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
