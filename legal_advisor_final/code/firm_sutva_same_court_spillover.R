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
# each (award_id, firm_id, court_match_key) cell with whether the
# firm is a winner, a loser-in-same-court, or a loser-in-other-court,
# and run a stacked DID with these three categories as the
# treatment indicator (loser-in-other-court is the baseline).

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
doc_path <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
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

main <- function() {
  doc <- fread(doc_path, encoding = "UTF-8")
  doc <- doc[!is.na(court_match_key)]

  # Freeze court exposure at the award. Both the winner's court footprint and
  # each loser's overlap must come from pre-award histories, not later cases.
  pre_doc <- doc[event_time < 0L]
  winner_courts <- unique(pre_doc[treated_firm == 1L,
                              .(award_id, court_match_key)])
  winner_courts[, winner_court := 1L]
  pre_doc <- merge(pre_doc, winner_courts,
                   by = c("award_id", "court_match_key"), all.x = TRUE)
  pre_doc[, winner_court := fifelse(is.na(winner_court), 0L, winner_court)]
  # Without an observed pre-award court history, absence of overlap is unknown.
  exposure <- pre_doc[, .(
    control_in_winner_court = as.integer(first(treated_firm) == 0L &&
                                          any(winner_court == 1L))
  ), by = .(award_id, firm_id)]
  observed_winner_stacks <- unique(winner_courts$award_id)
  doc <- merge(doc[award_id %in% observed_winner_stacks], exposure,
               by = c("award_id", "firm_id"), all = FALSE)
  stopifnot(doc[, all(uniqueN(control_in_winner_court) == 1L),
                by = .(award_id, firm_id)]$V1)

  cell <- doc[, .(
    n_cases = .N,
    n_decisive = sum(case_decisive),
    n_win = sum(case_win_binary, na.rm = TRUE),
    legal_reasoning_share = mean(legal_reasoning_share, na.rm = TRUE),
    log_reasoning_length = mean(log_legal_reasoning_length_chars, na.rm = TRUE),
    treated_firm = max(treated_firm),
    control_in_winner_court = max(control_in_winner_court),
    event_year = first(event_year),
    post = max(post),
    did_treatment = max(did_treatment)
  ), by = .(award_id, firm_id, year)]
  cell[, civil_win_rate := fifelse(n_decisive > 0, n_win / n_decisive, NA_real_)]
  cell[, log_civil_case_n := log(n_cases + 1)]
  cell[, post_within_winner_court := control_in_winner_court * post]
  cell[, post_winner := treated_firm * post]
  cell[, stack_year_fe := sprintf("%s__%s", award_id, year)]
  cell[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]

  fit <- function(outcome, sample_filter = NULL) {
    sub <- if (is.null(sample_filter)) cell else cell[eval(sample_filter)]
    sub <- sub[!is.na(get(outcome))]
    f <- as.formula(sprintf(
      "%s ~ post_winner + post_within_winner_court | stack_firm_fe + stack_year_fe",
      outcome
    ))
    m <- feols(f, data = sub, cluster = ~ award_id + firm_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    list(
      winner = ct[term == "post_winner"],
      same_court = ct[term == "post_within_winner_court"],
      n_obs = nobs(m),
      r2 = fitstat(m, "r2")[[1]],
      collinear_terms = paste(m$collin.var, collapse = ";"),
      comparison_without_overlap = uniqueN(sub[treated_firm == 0L & control_in_winner_court == 0L, .(award_id, firm_id)]),
      comparison_with_overlap = uniqueN(sub[treated_firm == 0L & control_in_winner_court == 1L, .(award_id, firm_id)])
    )
  }

  outcomes <- list(
    list(label = "Civil Win Rate (decisive)", col = "civil_win_rate", filter = quote(n_decisive > 0)),
    list(label = "log(Civil Cases + 1)", col = "log_civil_case_n", filter = quote(n_cases > 0)),
    list(label = "Reasoning Share", col = "legal_reasoning_share", filter = quote(n_cases > 0)),
    list(label = "log Reasoning Length", col = "log_reasoning_length", filter = quote(n_cases > 0))
  )

  fmt_cell <- function(r) {
    if (!nrow(r)) return("\\textit{N/A}")
    paste0(fmt_num(r[["Estimate"]]), stars(r[["Pr(>|t|)"]]))
  }
  fmt_se <- function(r) {
    if (!nrow(r)) return("--")
    paste0("(", fmt_num(r[["Std. Error"]]), ")")
  }

  rows <- character(0)
  support <- list()
  for (spec in outcomes) {
    res <- fit(spec$col, spec$filter)
    support[[spec$col]] <- data.table(
      outcome = spec$col, n_obs = res$n_obs,
      comparison_without_overlap = res$comparison_without_overlap,
      comparison_with_overlap = res$comparison_with_overlap,
      overlap_estimation_status = if (nrow(res$same_court)) "estimated" else "not_estimable_collinear",
      collinear_terms = res$collinear_terms)
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
    "\\caption{SUTVA Diagnostic: Pre-Award Court Overlap among Winners and Comparison Firms}",
    "\\label{tab:firm_sutva_same_court_spillover}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Winner $\\times$ Post} & \\multicolumn{2}{c}{\\shortstack{Comparison with winner-court\\\\overlap $\\times$ Post}} & & \\\\",
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
      "Both the winner's courts and each comparison firm's overlap are defined using only pre-award civil cases and held fixed across years; firms without an observed pre-award court history and stacks without a pre-award winner history are excluded.",
      "The formula includes winner-by-post and comparison-overlap-by-post terms, with stack-by-firm and stack-by-year fixed effects. Comparison firms are sampled firms, not verified unsuccessful bidders.",
      "N/A denotes a requested interaction omitted because of collinearity; it is neither an estimated zero nor evidence of no interference.",
      if (all(vapply(support, function(x) x$comparison_without_overlap == 0L, logical(1)))) "Every observed comparison firm in these estimation samples has pre-award winner-court overlap. The winner coefficient is therefore relative to these observed comparison firms; a separate overlap contrast is not identified." else "Where comparison firms without overlap are absent, the winner coefficient is relative to the observed comparison firms and a separate overlap contrast is not identified.",
      "Court overlap may capture shared local shocks as well as spillovers; an imprecise or unavailable comparison does not establish the absence of interference.",
      "Standard errors clustered by stack and firm.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_sutva_same_court_spillover_table.tex")
  writeLines(lines, out)
  fwrite(rbindlist(support), file.path(table_dir, "firm_sutva_same_court_spillover_support.csv"))
  cat("Wrote", out, "\n")
}

main()
