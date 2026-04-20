#!/usr/bin/env Rscript
# firm_fect_carryover_exit.R
#
# For cities with multiple procurement contracts (i.e. those with
# 2 or 3 stacks in the firm-year panel), the first contractor
# eventually loses the contract and is replaced by a new winner.
# This script restricts the firm-year panel to such "exit" cities,
# constructs a binary treatment that turns on while the firm holds
# the contract and switches off after a successor stack starts in
# the same city, and runs the fect package's
#  (i) period-wise ATT relative to treatment exit (treat == 0
#      after exit), and
#  (ii) the carry-over test, which probes whether the post-exit
#       outcomes still reflect the prior treatment.

suppressPackageStartupMessages({
  library(data.table)
  library(fect)
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
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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

  city_stacks <- unique(fm[, .(province, city, stack_id, event_year)])
  contracts_per_city <- city_stacks[, .(n_stacks = uniqueN(stack_id)),
                                     by = .(province, city)]
  exit_cities <- contracts_per_city[n_stacks >= 2L, .(province, city, n_stacks)]
  if (nrow(exit_cities) == 0L) {
    cat("No multi-contract cities; nothing to do.\n")
    return(invisible(NULL))
  }

  fm_exit <- merge(fm, exit_cities[, .(province, city)],
                   by = c("province", "city"))
  fm_exit[, contract_active := as.integer(treated_firm == 1L &
                                           event_time >= 0L)]
  contract_periods <- city_stacks[order(event_year), .(
    stack_start = event_year[1],
    stack_next = if (.N >= 2) event_year[2] else NA_integer_
  ), by = .(province, city)]
  fm_exit <- merge(fm_exit, contract_periods, by = c("province", "city"),
                   all.x = TRUE)
  fm_exit[, lost_contract := as.integer(treated_firm == 1L &
                                         !is.na(stack_next) &
                                         event_year == stack_start &
                                         year >= stack_next)]
  fm_exit[, treat := as.integer(contract_active == 1L & lost_contract == 0L)]

  fm_exit[, unit_id := sprintf("%s__%s", stack_id, firm_id)]
  fm_exit[, time_id := year]

  outcomes <- list(
    list(label = "civil_win_rate_mean", filter = quote(civil_decisive_case_n > 0)),
    list(label = "avg_filing_to_hearing_days", filter = quote(civil_case_n > 0)),
    list(label = "civil_win_rate_fee_mean", filter = quote(civil_fee_decisive_case_n > 0))
  )

  rows <- character(0)
  for (spec in outcomes) {
    sub <- as.data.frame(fm_exit[!is.na(get(spec$label)) & eval(spec$filter)])
    if (nrow(sub) < 30) next
    fit <- tryCatch(
      fect(formula = as.formula(paste(spec$label, "~ treat")),
           data = sub,
           index = c("unit_id", "time_id"),
           force = "two-way",
           method = "fe",
           se = TRUE,
           nboots = 200L,
           parallel = FALSE,
           seed = 42L,
           CV = FALSE),
      error = function(e) {cat("fect error for", spec$label, ":", conditionMessage(e), "\n"); NULL}
    )
    if (is.null(fit)) next
    att_avg <- fit$att.avg
    att_se <- if (!is.null(fit$est.att.avg)) fit$est.att.avg[1, "S.E."] else NA_real_
    fig_path <- file.path(figure_dir,
                          sprintf("firm_fect_exit_%s.pdf", spec$label))
    p_obj <- tryCatch(
      plot(fit, type = "exit",
           main = paste("Period-wise ATT relative to exit:", spec$label)),
      error = function(e) {cat("fect plot exit error for", spec$label, ":", conditionMessage(e), "\n"); NULL}
    )
    if (is.null(p_obj) || (!inherits(p_obj, "ggplot") && !inherits(p_obj, "gtable"))) {
      p_obj <- tryCatch(
        plot(fit, type = "gap",
             main = paste("Event-time gap (fect):", spec$label)),
        error = function(e) NULL
      )
    }
    if (!is.null(p_obj)) {
      pdf(fig_path, width = 7.0, height = 4.6, family = "serif")
      tryCatch(print(p_obj), error = function(e) {
        cat("print plot error for", spec$label, ":", conditionMessage(e), "\n")
      })
      dev.off()
      cat("Wrote", fig_path, "\n")
    } else {
      cat("Skipped plot for", spec$label, "\n")
    }
    co_test <- tryCatch(
      fect(formula = as.formula(paste(spec$label, "~ treat")),
           data = sub,
           index = c("unit_id", "time_id"),
           force = "two-way",
           method = "fe",
           se = TRUE,
           nboots = 200L,
           parallel = FALSE,
           seed = 42L,
           CV = FALSE,
           carryoverTest = TRUE,
           carryover.period = c(1, 2)),
      error = function(e) NULL
    )
    co_p <- if (!is.null(co_test) && !is.null(co_test$test.carryover$p.value))
      co_test$test.carryover$p.value[1] else NA_real_
    rows <- c(rows, paste(
      spec$label, "&",
      fmt_num(att_avg), "&", fmt_num(att_se), "&",
      fmt_num(co_p),
      "\\\\"
    ))
  }
  if (length(rows) == 0L) rows <- "\\multicolumn{4}{c}{Insufficient sample for fect estimation} \\\\"

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Period-wise ATT Relative to Treatment Exit and Carry-over Test (fect) for Multi-Contract Cities}",
    "\\label{tab:firm_fect_carryover_exit}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Outcome & Avg.\\ ATT & SE & Carry-over $p$ \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} Estimated with the \\texttt{fect} R package on the firm-year subsample restricted to cities with at least two procurement contracts (%d cities, %d firm-year observations).",
      "Treatment turns on while the firm holds an active contract and turns off in the year a successor stack begins in the same city; pre- and post-exit periods are absorbed by two-way fixed effects and 200 bootstrap replications.",
      "Avg.\\ ATT is the average treatment effect on the treated across post-exit periods; the carry-over $p$-value tests whether outcomes in the first two post-exit years still differ from the no-effect prediction (small $p$ signals carry-over).",
      "Figures \\texttt{firm\\_fect\\_exit\\_*.pdf} plot the period-wise ATT relative to exit for each outcome."
    ), nrow(exit_cities), nrow(fm_exit)),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_fect_carryover_exit_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
