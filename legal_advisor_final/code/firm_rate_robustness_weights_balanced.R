#!/usr/bin/env Rscript
# firm_rate_robustness_weights_balanced.R  (H4 robustness)
#
# Rate outcomes (civil win rate, fee-based win rate, filing-to-hearing days)
# are undefined for firm-years without decisive cases, and winners' caseloads
# grow after the award, so the set of firm-years with defined rates changes
# with treatment. This script re-estimates the firm-year stacked DID
#   (1) baseline (unweighted, all firm-years with a defined rate);
#   (2) weighted by the number of cases behind each rate;
#   (3) on the balanced set of firms with a defined rate in every year of the
#       event window;
#   (4) with the rate set to the firm's pre-period mean when undefined
#       (an assumption-dependent no-change scenario for silent years).
#
# Output: output/tables/firm_rate_robustness_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

fy <- fread(file.path(dirs$data, "firm_level.csv"), encoding = "UTF-8")
fy[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]
fy[, stack_year_fe := sprintf("%s__%s", award_id, year)]
fy[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
fy <- fy[!is.na(et)]

outcomes <- c("civil_win_rate_mean", "civil_win_rate_fee_mean", "avg_filing_to_hearing_days")
weights_var <- c("civil_decisive_case_n", "civil_fee_decisive_case_n", "civil_case_n")
labels <- c("Civil win rate", "Fee-based win rate", "Filing-to-hearing days")

run <- function(o, w, k) {
  d <- copy(fy)
  d[, wgt := get(w)]
  # Match the main table: an outcome is defined only with a positive case
  # denominator. A stored hearing average in a zero-case year is not an outcome.
  d[is.na(wgt) | wgt <= 0, (o) := NA_real_]
  if (k == 1) {
    d <- d[!is.na(get(o))]
    m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", o)), data = d, cluster = ~ award_id + firm_id)
  } else if (k == 2) {
    d <- d[!is.na(get(o)) & wgt > 0]
    m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", o)), data = d, weights = ~ wgt, cluster = ~ award_id + firm_id)
  } else if (k == 3) {
    d[, n_defined := sum(!is.na(get(o))), by = stack_firm_fe]
    d[, n_years := .N, by = stack_firm_fe]
    d <- d[n_defined == n_years]
    m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", o)), data = d, cluster = ~ award_id + firm_id)
  } else {
    d[, pre_mean := mean(get(o)[event_time < 0], na.rm = TRUE), by = stack_firm_fe]
    d[, y_imp := fifelse(is.na(get(o)), pre_mean, get(o))]
    d <- d[!is.na(y_imp)]
    m <- feols(y_imp ~ did_treatment | stack_firm_fe + stack_year_fe, data = d, cluster = ~ award_id + firm_id)
  }
  r <- pull_term(m, "did_treatment"); r$n_firms <- uniqueN(d$stack_firm_fe); r
}
res <- lapply(seq_along(outcomes), function(i) lapply(1:4, function(k) run(outcomes[i], weights_var[i], k)))

header <- c(" & (1) & (2) & (3) & (4) \\\\",
            "Specification & Baseline & Case-weighted & Balanced firms & Pre-mean imputation \\\\")
body <- unlist(lapply(seq_along(outcomes), function(i) {
  r <- res[[i]]
  c(sprintf("\\multicolumn{5}{l}{\\textit{Outcome: %s}} \\\\", labels[i]),
    paste("Winner $\\times$ Post &", paste(sapply(r, function(x) coef_cell(x$estimate, x$se, x$p_value)[1]), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(r, function(x) coef_cell(x$estimate, x$se, x$p_value)[2]), collapse = " & "), "\\\\"),
    paste("Observations &", paste(sapply(r, function(x) fmt_int(x$n_obs)), collapse = " & "), "\\\\"),
    paste("Firm-stacks &", paste(sapply(r, function(x) fmt_int(x$n_firms)), collapse = " & "), "\\\\"),
    "\\addlinespace")
}))
body <- c(body, "Stack $\\times$ Firm FE & Yes & Yes & Yes & Yes \\\\", "Stack $\\times$ Year FE & Yes & Yes & Yes & Yes \\\\")
note <- paste(
  "Firm-year stacked DID for the rate outcomes under alternative treatments of firm-years without a defined rate (event window $[-5, 5]$).",
  "Baseline samples require positive case denominators, as in the main estimates; column 2 weights by those denominators and therefore changes the weighting of firm-years.",
  "Column 3 restricts to firms with an outcome in every observed year of the event window; column 4 assigns the pre-award mean to undefined outcomes as a no-change scenario. These assumptions address composition differently and do not yield bounds.",
  "Standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "firm_rate_robustness_table.tex"),
                "Firm-Side Rate Outcomes: Weighting and Sample Composition",
                "tab:firm_rate_robustness", "lcccc", header, body, note)
