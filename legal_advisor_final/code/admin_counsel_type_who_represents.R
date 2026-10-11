#!/usr/bin/env Rscript
# admin_counsel_type_who_represents.R  (H2, treatment on the treated)
#
# Among cases in adopting cities after the award, compares outcomes by who
# represented the government: in-house staff only, another law firm, or the
# contracted firm. Two specifications:
#   Panel A. Post-award cases in adopting cities: outcome ~ counsel type
#            (reference: in-house only) with court, year and cause-group FE.
#   Panel B. Full case panel: Treatment x Post interacted with counsel type,
#            so the procurement effect is split into the part carried by
#            cases the contracted firm argued and the part carried by all
#            other cases (which includes deterrence and in-house learning).
# Outcomes: government win, merits-decided government win, withdrawal, appeal.
#
# Output: output/tables/admin_counsel_type_table.tex

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
adm[, contracted := as.integer(gov_counsel_type == "contracted_firm")]
adm[, other_counsel := as.integer(gov_counsel_type == "other_counsel")]
adm[, decisive := as.integer(withdraw_case == 0L & end_case == 0L)]
adm[, did_contracted := did_treatment * contracted]
adm[, did_other := did_treatment * (1L - contracted)]

outcomes <- c("government_win", "government_win", "withdraw_case", "appealed")
samples <- list(quote(TRUE), quote(decisive == 1L), quote(TRUE), quote(TRUE))
labels <- c("Gov.\\ win (all)", "Gov.\\ win (merits)", "Withdrawal", "Appeal")

post_dt <- adm[treated_city == 1L & post == 1L]
panelA <- lapply(seq_along(outcomes), function(k) {
  d <- post_dt[eval(samples[[k]])]
  m <- feols(as.formula(sprintf("%s ~ contracted + other_counsel + plaintiff_is_entity + opponent_has_lawyer | court_id + year + cause_group", outcomes[k])),
             data = d, cluster = ~ city_id + court_id)
  list(contracted = pull_term(m, "contracted"), other = pull_term(m, "other_counsel"),
       n = nobs(m), mean_inhouse = d[gov_counsel_type == "in_house_only", mean(get(outcomes[k]))])
})
panelB <- lapply(seq_along(outcomes), function(k) {
  d <- adm[eval(samples[[k]])]
  m <- feols(as.formula(sprintf("%s ~ did_contracted + did_other + plaintiff_is_entity + opponent_has_lawyer | court_id + year + cause_group", outcomes[k])),
             data = d, cluster = ~ city_id + court_id)
  eq <- diff_test(m, "did_contracted", "did_other")$p_value
  list(contracted = pull_term(m, "did_contracted"), other = pull_term(m, "did_other"), n = nobs(m), eq_p = eq)
})

cellrow <- function(lst, key, which) sapply(lst, function(r) coef_cell(r[[key]]$estimate, r[[key]]$se, r[[key]]$p_value)[which])
header <- c(
  " & (1) & (2) & (3) & (4) \\\\",
  paste("Outcome &", paste(labels, collapse = " & "), "\\\\")
)
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. Post-award cases in adopting cities; reference: government represented by in-house staff only}} \\\\",
  "\\addlinespace",
  paste("Contracted firm represents government &", paste(cellrow(panelA, "contracted", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(panelA, "contracted", 2), collapse = " & "), "\\\\"),
  paste("Other law firm represents government &", paste(cellrow(panelA, "other", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(panelA, "other", 2), collapse = " & "), "\\\\"),
  paste("Mean, in-house only &", paste(sapply(panelA, function(r) fmt_num(r$mean_inhouse)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(panelA, function(r) fmt_int(r$n)), collapse = " & "), "\\\\"),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Full case panel; Treatment $\\times$ Post split by who argued the case}} \\\\",
  "\\addlinespace",
  paste("Treatment $\\times$ Post $\\times$ contracted firm &", paste(cellrow(panelB, "contracted", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(panelB, "contracted", 2), collapse = " & "), "\\\\"),
  paste("Treatment $\\times$ Post $\\times$ other representation &", paste(cellrow(panelB, "other", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cellrow(panelB, "other", 2), collapse = " & "), "\\\\"),
  paste("Equality test $p$ &", paste(sapply(panelB, function(r) fmt_p(r$eq_p)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(panelB, function(r) fmt_int(r$n)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "Case controls & Yes & Yes & Yes & Yes \\\\",
  "Court / Year / Cause-group FE & Yes & Yes & Yes & Yes \\\\"
)
note <- paste(
  "Who represents the state (H2, treatment on the treated).",
  "Panel A restricts to cases decided in adopting cities after the award and compares outcomes by the government's representation, with in-house representation as the reference category.",
  "Panel B estimates the DID on the full case panel with Treatment $\\times$ Post split into cases argued by the contracted firm and all other post-award cases; the latter carries deterrence and any in-house learning, so the equality test asks whether the contracted firm's cases differ from the rest of the post-award docket.",
  "Column 2 restricts to merits-decided cases.",
  "Case controls: plaintiff entity, opposing counsel. Standard errors clustered by city and court."
)
write_tpt_table(file.path(dirs$tables, "admin_counsel_type_table.tex"),
                "Who Represents the State: Outcomes by the Government's Counsel",
                "tab:admin_counsel_type", "lcccc", header, body, note)
