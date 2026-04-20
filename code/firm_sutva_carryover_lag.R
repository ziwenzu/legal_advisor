#!/usr/bin/env Rscript
# firm_sutva_carryover_lag.R
#
# Tests whether the firm-level Winner x Post effect persists across
# adjacent post-treatment years or is concentrated in the
# announcement / contract year. Adds the one-year lag of
# did_treatment to the firm-year stacked DID for the three main
# outcomes.

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
firm_path <- file.path(root_dir, "data", "firm_level.csv")
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
  fm <- fread(firm_path)
  setorder(fm, stack_id, firm_id, year)
  fm[, did_treatment_lag1 := shift(did_treatment, 1L, type = "lag"),
     by = .(stack_id, firm_id)]
  fm[, stack_firm_fe := sprintf("%s__%s", stack_id, firm_id)]
  fm[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  fm[, event_time_window := fifelse(event_time < -5, NA_real_,
                            fifelse(event_time > 5, NA_real_, event_time))]

  outcomes <- list(
    list(label = "Civil Win Rate", col = "civil_win_rate_mean", filter = quote(civil_decisive_case_n > 0)),
    list(label = "Average Hearing Days", col = "avg_filing_to_hearing_days", filter = quote(civil_case_n > 0)),
    list(label = "Civil Fee Win Rate", col = "civil_win_rate_fee_mean", filter = quote(civil_fee_decisive_case_n > 0))
  )

  fmt_cell <- function(r) paste0(fmt_num(r[["Estimate"]]),
                                  stars(r[["Pr(>|t|)"]]))
  fmt_se <- function(r) paste0("(", fmt_num(r[["Std. Error"]]), ")")

  fit <- function(spec) {
    sub <- fm[!is.na(get(spec$col)) & !is.na(event_time_window) &
              !is.na(did_treatment_lag1) & eval(spec$filter)]
    f <- as.formula(sprintf("%s ~ did_treatment + did_treatment_lag1 | stack_firm_fe + stack_year_fe", spec$col))
    m <- feols(f, data = sub, cluster = ~ stack_id + firm_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    list(
      base = ct[term == "did_treatment"],
      lag = ct[term == "did_treatment_lag1"],
      n_obs = nobs(m),
      r2 = fitstat(m, "r2")[[1]]
    )
  }

  rows <- character(0)
  for (spec in outcomes) {
    res <- fit(spec)
    rows <- c(rows, paste(
      spec$label, "&",
      fmt_cell(res$base), "&", fmt_se(res$base), "&",
      fmt_cell(res$lag), "&", fmt_se(res$lag), "&",
      fmt_int(res$n_obs), "&", fmt_num(res$r2),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Carry-over and Persistence Test for the Firm-Year Stacked DID}",
    "\\label{tab:firm_sutva_carryover_lag}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Winner $\\times$ Post (current year)} & \\multicolumn{2}{c}{Winner $\\times$ Post (one-year lag)} & & \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Outcome & Estimate & SE & Estimate & SE & Observations & $R^2$ \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Stacked firm-year regressions with the contemporaneous and one-year-lagged Winner $\\times$ Post indicators on the right-hand side, with stack-by-firm and stack-by-year fixed effects, on the event-time window $[-5, 5]$, clustered by stack and firm.",
      "If the procurement effect is announcement-driven, the contemporaneous coefficient should dominate; if the effect builds up through accumulating procurement experience, the lagged coefficient should be sizeable in the same direction.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_sutva_carryover_lag_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
