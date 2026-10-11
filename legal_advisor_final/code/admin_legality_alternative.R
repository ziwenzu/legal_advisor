#!/usr/bin/env Rscript
# admin_legality_alternative.R  (ex-ante legality alternative vs capacity/deterrence)
#
# If procurement mainly improves the lawfulness of administrative acts (advisors
# review decisions before they are issued), the government-win gain should be
# confined to acts taken after the contract, and challenges to the government's
# normative documents should succeed less often. If the gain runs through
# litigation capacity and deterrence, it should appear for acts taken before
# the contract as well, and it should be accompanied by more withdrawals.
#   Panel A. Treatment x Post split by whether the contested act post-dates the contract.
#   Panel B. Normative-document review (ALL Art. 53): request rate; unlawful findings among reviews.
#   Panel C. Agency head appears in court (performing legality).
#
# Output: output/tables/admin_legality_alternative_table.tex

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
adm[, decisive := as.integer(withdraw_case == 0L & end_case == 0L)]
# Acts decided before the contract exist only in the first post-award years
# (the limitation period is short), so the act-timing comparison is made within
# the early post-award window (event time 0-1); later post-award cases enter as
# a separate term so that the comparison is not confounded by effect dynamics.
adm[, early_post := as.integer(post == 1L & event_time <= 1L)]
adm[, did_early_after := did_treatment * early_post * act_after_contract]
adm[, did_early_before := did_treatment * early_post * (1L - act_after_contract)]
adm[, did_later := did_treatment * (1L - early_post)]

ctrl <- "government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity"
fe <- "court_id + year + cause_group"

split_est <- function(outcome, d) {
  m <- feols(as.formula(sprintf("%s ~ did_early_after + did_early_before + did_later + %s | %s", outcome, ctrl, fe)),
             data = d, cluster = ~ city_id + court_id)
  list(after = pull_term(m, "did_early_after"), before = pull_term(m, "did_early_before"),
       later = pull_term(m, "did_later"),
       eq_p = diff_test(m, "did_early_after", "did_early_before")$p_value, n = nobs(m))
}
pa <- list(split_est("government_win", adm), split_est("government_win", adm[decisive == 1L]),
           split_est("withdraw_case", adm), split_est("appealed", adm))
pa_labels <- c("Gov.\\ win (all)", "Gov.\\ win (merits)", "Withdrawal", "Appeal")

simple_est <- function(outcome, d) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment + %s | %s", outcome, ctrl, fe)), data = d, cluster = ~ city_id + court_id)
  r <- pull_term(m, "did_treatment"); r$mean <- mean(d[[outcome]], na.rm = TRUE); r
}
pb <- list(simple_est("normative_doc_review", adm),
           simple_est("normative_doc_unlawful", adm[normative_doc_review == 1L]),
           simple_est("head_appeared", adm))
pb_labels <- c("Review requested", "Document found unlawful", "Agency head appeared")

cellrow <- function(lst, key, which) sapply(lst, function(r) coef_cell(r[[key]]$estimate, r[[key]]$se, r[[key]]$p_value)[which])
header <- c(" & (1) & (2) & (3) & (4) \\\\")
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. Treatment $\\times$ Post by timing of the contested act}} \\\\",
  paste("Outcome &", paste(pa_labels, collapse = " & "), "\\\\"),
  "\\addlinespace",
  paste("Years 0--1, act decided after the contract &", paste(cellrow(pa, "after", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(pa, "after", 2), collapse = " & "), "\\\\"),
  paste("Years 0--1, act decided before the contract &", paste(cellrow(pa, "before", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(pa, "before", 2), collapse = " & "), "\\\\"),
  paste("Years 2 and later (all acts) &", paste(cellrow(pa, "later", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(pa, "later", 2), collapse = " & "), "\\\\"),
  paste("Equality test $p$ &", paste(sapply(pa, function(r) fmt_p(r$eq_p)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(pa, function(r) fmt_int(r$n)), collapse = " & "), "\\\\"),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Normative-document review and the head's appearance}} \\\\",
  paste("Outcome &", paste(pb_labels, collapse = " & "), "& \\\\"),
  "\\addlinespace",
  paste("Treatment $\\times$ Post &", paste(sapply(pb, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "& \\\\"),
  paste("&", paste(sapply(pb, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "& \\\\"),
  paste("Sample mean &", paste(sapply(pb, function(r) fmt_num(r$mean)), collapse = " & "), "& \\\\"),
  paste("Observations &", paste(sapply(pb, function(r) fmt_int(r$n_obs)), collapse = " & "), "& \\\\"),
  "\\addlinespace",
  "Case controls & Yes & Yes & Yes & Yes \\\\",
  "Court / Year / Cause-group FE & Yes & Yes & Yes & Yes \\\\"
)
share_after <- adm[treated_city == 1L & early_post == 1L, mean(act_after_contract)]
note <- paste(
  "Tests of the ex-ante legality alternative.",
  sprintf("Panel A splits Treatment $\\times$ Post in the first two post-award years by whether the contested administrative act was decided after the city's contract began (%s of those cases) or before it, with later post-award years entered separately; a legality channel predicts gains only for acts taken under the advisor, a capacity or deterrence channel predicts gains for both.", fmt_pct(share_after)),
  "Panel B reports DID estimates for whether the plaintiff requested incidental review of a normative document (ALL Art.~53), whether a reviewed document was found unlawful (among reviewed cases), and whether the head of the defendant agency appeared in court.",
  "Case controls: government counsel, opposing counsel, plaintiff entity. Standard errors clustered by city and court."
)
write_tpt_table(file.path(dirs$tables, "admin_legality_alternative_table.tex"),
                "Ex-Ante Legality versus Litigation Capacity",
                "tab:admin_legality_alternative", "lcccc", header, body, note)
