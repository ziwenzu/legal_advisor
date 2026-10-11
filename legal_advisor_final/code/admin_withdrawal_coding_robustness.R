#!/usr/bin/env Rscript
# admin_withdrawal_coding_robustness.R  (H1 / H3 robustness)
#
# The government-win indicator includes withdrawals already coded as state wins.
# This script shows how the case-level estimate depends on withdrawal coding.
# The supplement's reason labels are generated from existing outcome indicators,
# not independently observed motives or remedies:
#   (1) baseline government-win indicator;
#   (2) strict coding: no withdrawal counts as a government win;
#   (3) withdrawals dropped from the sample;
#   (4) plaintiff win as the outcome (mirror image);
#   (5)-(7) withdrawal crossed with state win, plaintiff win, or neither.
#
# Output: output/tables/admin_withdrawal_coding_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

adm <- fread(file.path(dirs$data, "admin_case_level.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "admin_case_supplement.csv"), encoding = "UTF-8")
adm <- merge(adm, sup, by = "case_no")
adm <- adm[!is.na(court_std) & court_std != ""]
adm[, city_id := .GRP, by = .(province, city)]
adm[, court_id := .GRP, by = court_std]
adm[, gov_win_strict := as.integer(government_win == 1L & withdraw_case == 0L)]
adm[, withdraw_voluntary := as.integer(withdrawal_reason == "voluntary")]
adm[, withdraw_agency_changed := as.integer(withdrawal_reason == "agency_changed_act")]
adm[, withdraw_other := as.integer(withdrawal_reason == "other")]

est <- function(outcome, d = adm) {
  if (uniqueN(d[[outcome]], na.rm=TRUE) < 2L) {
    cat("Descriptive only: constant outcome", outcome, "\n")
    return(list(estimate=NA_real_, se=NA_real_, p_value=NA_real_, n_obs=nrow(d), r2=NA_real_, mean=mean(d[[outcome]], na.rm=TRUE)))
  }
  m <- feols(as.formula(sprintf("%s ~ did_treatment + government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity | court_id + year + cause_group", outcome)),
             data = d, cluster = ~ city_id + court_id)
  r <- pull_term(m, "did_treatment")
  r$mean <- mean(d[[outcome]])
  r
}

res <- list(
  est("government_win"),
  est("gov_win_strict"),
  est("government_win", adm[withdraw_case == 0L]),
  est("plaintiff_win"),
  est("withdraw_voluntary"),
  est("withdraw_agency_changed"),
  est("withdraw_other")
)
labels <- c("Baseline win", "Non-withdrawal win", "Withdrawals excluded",
            "Plaintiff win", "State-win exit", "Plaintiff-win exit", "Other exit")
header <- c(
  " & (1) & (2) & (3) & (4) \\\\"
)
panel_rows <- function(idx) {
  pad <- function(x) paste(c(x, rep("", 4L - length(x))), collapse = " & ")
  rr <- res[idx]
  c(
    paste("Outcome &", pad(labels[idx]), "\\\\"),
    paste("Treatment $\\times$ Post &", pad(sapply(rr, function(r) if (is.na(r$estimate)) "" else coef_cell(r$estimate, r$se, r$p_value)[1])), "\\\\"),
    paste("&", pad(sapply(rr, function(r) if (is.na(r$estimate)) "" else coef_cell(r$estimate, r$se, r$p_value)[2])), "\\\\"),
    paste("Sample mean &", pad(sapply(rr, function(r) fmt_num(r$mean))), "\\\\"),
    paste("Observations &", pad(sapply(rr, function(r) fmt_int(r$n_obs))), "\\\\")
  )
}
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. Alternative outcome definitions and samples}} \\\\",
  panel_rows(1:4),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Withdrawals by recorded outcome}} \\\\",
  panel_rows(5:7),
  "\\addlinespace",
  paste("Case controls &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
  paste("Court / Year / Cause-group FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\")
)
note <- paste(
  "Panel A uses the existing government-win indicator (column 1), sets it to zero for every withdrawal (column 2), excludes withdrawals (column 3), or uses plaintiff win (column 4). These definitions change the outcome or selected sample; column 2 is not a causal lower bound.",
  "Panel B crosses withdrawal with the existing state-win, plaintiff-win, and residual outcomes. The supplement labels these voluntary exit, agency revision, and other exit by construction; those labels are not independent evidence of motives or relief.",
  "Constant outcomes are reported descriptively with no estimated coefficient. Controls: government counsel, opposing counsel, and plaintiff entity status; these can respond to procurement. Standard errors clustered by city and court."
)
write_tpt_table(file.path(dirs$tables, "admin_withdrawal_coding_table.tex"),
                "Coding of Withdrawals and the State-Side Estimate",
                "tab:admin_withdrawal_coding", "lcccc", header, body, note)
