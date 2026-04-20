#!/usr/bin/env Rscript
# firm_sutva_same_court_spillover.R
#
# SUTVA test for the firm/document analysis. For each loser
# (control) firm in a procurement stack, identifies whether that
# firm shares a court_match_key with the stack winner in the
# pre-period civil documents. The conjecture is that if the
# winner's effect spills over within a court, control firms
# operating in the same court should be displaced and show
# negative effects on civil_win_rate_mean and civil_case_n at
# the firm-year level.
#
# We collapse the document panel to firm-year-court cells, mark
# each (stack_id, firm_id, court_match_key) cell with whether the
# firm is a winner, a loser-in-same-court, or a loser-in-other-court,
# and run a stacked DID with these three categories as the
# treatment indicator (loser-in-other-court is the baseline).

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
doc_path <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
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
  doc <- fread(doc_path)
  doc <- doc[!is.na(court_match_key)]

  winner_courts <- unique(doc[treated_firm == 1L,
                              .(stack_id, court_match_key)])
  winner_courts[, winner_court := 1L]
  doc <- merge(doc, winner_courts, by = c("stack_id", "court_match_key"),
               all.x = TRUE)
  doc[, winner_court := fifelse(is.na(winner_court), 0L, winner_court)]
  doc[, control_in_winner_court := as.integer(treated_firm == 0L & winner_court == 1L)]
  doc[, control_other_court := as.integer(treated_firm == 0L & winner_court == 0L)]

  cell <- doc[, .(
    n_cases = .N,
    n_decisive = sum(case_decisive),
    n_win = sum(case_win_binary, na.rm = TRUE),
    legal_reasoning_share = mean(legal_reasoning_share, na.rm = TRUE),
    log_reasoning_length = mean(log_legal_reasoning_length_chars, na.rm = TRUE),
    treated_firm = max(treated_firm),
    control_in_winner_court = max(control_in_winner_court),
    control_other_court = max(control_other_court),
    event_year = first(event_year),
    post = max(post),
    did_treatment = max(did_treatment)
  ), by = .(stack_id, firm_id, year)]
  cell[, civil_win_rate := fifelse(n_decisive > 0, n_win / n_decisive, NA_real_)]
  cell[, log_civil_case_n := log(n_cases + 1)]
  cell[, post_within_winner_court := control_in_winner_court * post]
  cell[, post_winner := treated_firm * post]
  cell[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  cell[, stack_firm_fe := sprintf("%s__%s", stack_id, firm_id)]

  fit <- function(outcome, sample_filter = NULL) {
    sub <- if (is.null(sample_filter)) cell else cell[eval(sample_filter)]
    sub <- sub[!is.na(get(outcome))]
    f <- as.formula(sprintf(
      "%s ~ post_winner + post_within_winner_court | stack_firm_fe + stack_year_fe",
      outcome
    ))
    m <- feols(f, data = sub, cluster = ~ stack_id + firm_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    list(
      winner = ct[term == "post_winner"],
      same_court = ct[term == "post_within_winner_court"],
      n_obs = nobs(m),
      r2 = fitstat(m, "r2")[[1]]
    )
  }

  outcomes <- list(
    list(label = "Civil Win Rate (decisive)", col = "civil_win_rate", filter = quote(n_decisive > 0)),
    list(label = "log(Civil Cases + 1)", col = "log_civil_case_n", filter = quote(n_cases > 0)),
    list(label = "Reasoning Share", col = "legal_reasoning_share", filter = quote(n_cases > 0)),
    list(label = "log Reasoning Length", col = "log_reasoning_length", filter = quote(n_cases > 0))
  )

  fmt_cell <- function(r) paste0(fmt_num(r[["Estimate"]]),
                                  stars(r[["Pr(>|t|)"]]))
  fmt_se <- function(r) paste0("(", fmt_num(r[["Std. Error"]]), ")")

  rows <- character(0)
  for (spec in outcomes) {
    res <- fit(spec$col, spec$filter)
    rows <- c(rows, paste(
      spec$label, "&",
      fmt_cell(res$winner), "&", fmt_se(res$winner), "&",
      fmt_cell(res$same_court), "&", fmt_se(res$same_court), "&",
      fmt_int(res$n_obs), "&", fmt_num(res$r2),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{SUTVA Diagnostic: Within-Court Spillovers from Procurement Winners to Loser Firms in the Same Court}",
    "\\label{tab:firm_sutva_same_court_spillover}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Winner $\\times$ Post} & \\multicolumn{2}{c}{Loser-in-Winner-Court $\\times$ Post} & & \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Outcome & Estimate & SE & Estimate & SE & Observations & $R^2$ \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Stacked firm-year regressions on outcomes aggregated from the document-level civil-case file; observation unit is (stack, firm, year).",
      "Each row reports two coefficients from the same regression: the level effect on procurement winners (Winner $\\times$ Post) and a separate level effect on the loser firms in the same stack that had pre-procurement civil cases in the eventual winner's court (Loser-in-Winner-Court $\\times$ Post).",
      "The omitted category is loser firms whose civil documents are entirely in courts where the winner did not appear; baseline level differences across firms and years are absorbed by stack-by-firm and stack-by-year fixed effects.",
      "A negative Loser-in-Winner-Court coefficient is consistent with a within-court substitution / SUTVA violation; an insignificant coefficient supports SUTVA at the court level.",
      "Standard errors clustered by stack and firm.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_sutva_same_court_spillover_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
