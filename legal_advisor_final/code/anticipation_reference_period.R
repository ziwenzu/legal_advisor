#!/usr/bin/env Rscript
# anticipation_reference_period.R  (anticipation check)
#
# Tenders are announced months before awards, so cities and firms may adjust
# in the year before the award year. The standard event studies use t = -1 as
# the reference period and therefore cannot detect adjustment in that year.
# This script re-estimates the city-year and firm-year event studies with
# t = -2 as the reference period and reports the t = -1 coefficient (the
# anticipation margin), the post-period average, and the joint test of the
# remaining leads (t = -5 ... -3).
#
# Output: output/tables/anticipation_check_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

# ---- city-year ---------------------------------------------------------------
cy <- fread(file.path(dirs$data, "city_year_panel.csv"), encoding = "UTF-8")
cy[, city_id := .GRP, by = .(province, city)]
cy[, first_treat := if (any(treatment == 1L)) min(year[treatment == 1L]) else 0, by = city_id]
cy[, ever_treated := as.integer(first_treat > 0)]
cy[, rel := fifelse(ever_treated == 1L, pmin(pmax(year - first_treat, -5), 5), -2)]
ctrl <- function(o) if (o == "government_win_rate") "log_population_10k + log_gdp + log_registered_lawyers + log_published_document_n" else "log_population_10k + log_gdp + log_registered_lawyers"
city_es <- function(o) {
  m <- feols(as.formula(sprintf("%s ~ i(rel, ever_treated, ref = -2) + %s | city_id + year", o, ctrl(o))), data = cy, cluster = ~ city_id)
  lead1 <- pull_term(m, "rel::-1:ever_treated")
  post_terms <- sprintf("rel::%d:ever_treated", 0:5)
  b <- coef(m)[post_terms]; V <- vcov(m)[post_terms, post_terms]
  w <- rep(1 / length(post_terms), length(post_terms))
  post_est <- sum(w * b); post_se <- sqrt(as.numeric(t(w) %*% V %*% w))
  pre <- joint_pretest(m, "ever_treated", "rel", c(-5, -4, -3))
  list(lead1 = lead1, post_est = post_est, post_se = post_se, post_p = 2 * pnorm(-abs(post_est / post_se)), pre_p = pre$p_value, n = nobs(m))
}
rc <- lapply(c("government_win_rate", "appeal_rate", "log_admin_case_n"), city_es)
labels_c <- c("Government win rate", "Appeal rate", "Log(1 + administrative cases)")

# ---- firm-year ---------------------------------------------------------------
fy <- fread(file.path(dirs$data, "firm_level.csv"), encoding = "UTF-8")
fy[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]
fy[, stack_year_fe := sprintf("%s__%s", award_id, year)]
fy[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
fy <- fy[!is.na(et)]
fy[, enterprise_share := fifelse(civil_case_n > 0, enterprise_case_n / civil_case_n, NA_real_)]
firm_es <- function(o) {
  d <- fy[!is.na(get(o))]
  # Match the main-table denominator: stored hearing averages in zero-case
  # firm-years are not defined litigation outcomes.
  if (o == "avg_filing_to_hearing_days") d <- d[!is.na(civil_case_n) & civil_case_n > 0]
  m <- feols(as.formula(sprintf("%s ~ i(et, treated_firm, ref = -2) | stack_firm_fe + stack_year_fe", o)), data = d, cluster = ~ award_id + firm_id)
  lead1 <- pull_term(m, "et::-1:treated_firm")
  post_terms <- sprintf("et::%d:treated_firm", 0:5)
  post_terms <- intersect(post_terms, names(coef(m)))
  b <- coef(m)[post_terms]; V <- vcov(m)[post_terms, post_terms]
  w <- rep(1 / length(post_terms), length(post_terms))
  post_est <- sum(w * b); post_se <- sqrt(as.numeric(t(w) %*% V %*% w))
  pre <- joint_pretest(m, "treated_firm", "et", c(-5, -4, -3))
  list(lead1 = lead1, post_est = post_est, post_se = post_se, post_p = 2 * pnorm(-abs(post_est / post_se)), pre_p = pre$p_value, n = nobs(m))
}
rf <- lapply(c("civil_win_rate_mean", "avg_filing_to_hearing_days", "enterprise_share"), firm_es)
labels_f <- c("Civil win rate", "Filing-to-hearing days", "Enterprise share")

rows <- function(res, labs) unlist(lapply(seq_along(res), function(i) {
  r <- res[[i]]
  c(paste(labs[i], "&", coef_cell(r$lead1$estimate, r$lead1$se, r$lead1$p_value)[1], "&",
          coef_cell(r$post_est, r$post_se, r$post_p)[1], "&", fmt_p(r$pre_p), "&", fmt_int(r$n), "\\\\"),
    paste("&", coef_cell(r$lead1$estimate, r$lead1$se, r$lead1$p_value)[2], "&",
          coef_cell(r$post_est, r$post_se, r$post_p)[2], "& & \\\\"))
}))
header <- c(" & $t=-1$ coefficient & Post-period average & Leads $t\\le-3$ joint $p$ & Observations \\\\")
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. City-year TWFE event studies (reference period $t=-2$)}} \\\\",
  rows(rc, labels_c),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Firm-year stacked event studies (reference period $t=-2$)}} \\\\",
  rows(rf, labels_f)
)
note <- paste(
  "Anticipation check. Reference period $t=-2$ allows a coefficient for the year before the award.",
  "The post-period average is the equally weighted mean of the $t=0$ to $t=5$ coefficients.",
  "Panel A estimates TWFE event studies on all cities, including never-adopters, with city controls and city and year fixed effects. It is not a cohort-specific comparison using only untreated controls.",
  "Panel B includes stack $\\times$ firm and stack $\\times$ year fixed effects. Hearing-time outcomes require positive civil caseloads, as in the main table.",
  "Standard errors clustered by city (Panel A) and by stack and firm (Panel B)."
)
write_tpt_table(file.path(dirs$tables, "anticipation_check_table.tex"),
                "Anticipation: Effects in the Year Before the Award",
                "tab:anticipation_check", "lcccc", header, body, note)
