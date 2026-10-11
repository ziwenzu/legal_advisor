#!/usr/bin/env Rscript
# firm_caseload_expansion.R  (H4: demand-side rents)
#
# Firm-year stacked DID on the contracted firm's caseload and organizational
# growth: log civil caseload, log firm size (lawyers), branch offices, and the
# enterprise share (reported again for completeness), with the standard
# [-5, 5] window and stack x firm / stack x year fixed effects. A count model
# (Poisson pseudo-maximum likelihood) is reported for the caseload as a check
# on the log(1 + x) transformation.
#
# Outputs: output/tables/firm_caseload_expansion_table.tex
#          output/tables/firm_client_counts_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

fy <- fread(file.path(dirs$data, "firm_level.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "firm_year_supplement.csv"), encoding = "UTF-8")
fy <- merge(fy, sup, by = c("award_id", "firm_id", "year"))
fy[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]
fy[, stack_year_fe := sprintf("%s__%s", award_id, year)]
fy[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
fy <- fy[!is.na(et)]
fy[, log_civil_cases := log1p(civil_case_n)]
fy[, log_enterprise_cases := log1p(enterprise_case_n)]
fy[, log_personal_cases := log1p(personal_case_n)]
fy[, log_firm_size := fifelse(!is.na(firm_size) & firm_size > 0, log(firm_size), NA_real_)]
fy[, enterprise_share := fifelse(civil_case_n > 0, enterprise_case_n / civil_case_n, NA_real_)]

est <- function(outcome, poisson = FALSE) {
  d <- fy[!is.na(get(outcome))]
  f <- as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome))
  m <- if (poisson) fepois(f, data = d, cluster = ~ award_id + firm_id) else feols(f, data = d, cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment")
  if (poisson) r$r2 <- NA_real_
  r$pre_mean <- fy[treated_firm == 1L & event_time < 0 & !is.na(get(outcome)), mean(get(outcome))]
  r
}
res <- list(est("log_civil_cases"), est("civil_case_n", poisson = TRUE), est("log_firm_size"),
            est("branch_office_n"), est("enterprise_share"))
labels <- c("Log(1 + civil cases)", "Civil cases (PPML)", "Log firm size", "Branch offices", "Enterprise share")
header <- c(" & (1) & (2) & (3) & (4) & (5) \\\\", paste("Outcome &", paste(labels, collapse = " & "), "\\\\"))
body <- c(
  paste("Winner $\\times$ Post &", paste(sapply(res, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(res, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Winner pre-award mean &", paste(sapply(res, function(r) fmt_num(r$pre_mean)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(res, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  paste("$R^2$ &", paste(sapply(res, function(r) fmt_num(r$r2)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "Stack $\\times$ Firm FE & Yes & Yes & Yes & Yes & Yes \\\\",
  "Stack $\\times$ Year FE & Yes & Yes & Yes & Yes & Yes \\\\"
)
note <- paste(
  "Demand-side rents (H4): the contracted firm's caseload and organizational growth relative to sampled comparison firms in the same event, event window $[-5, 5]$.",
  "Column 2 is a Poisson pseudo-maximum-likelihood estimate on the raw count; its coefficient is a semi-elasticity.",
  "Firm size is the number of licensed lawyers; branch offices counts active branches in that year.",
  "Standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "firm_caseload_expansion_table.tex"),
                "Caseload and Expansion of the Contracted Firm",
                "tab:firm_caseload_expansion", "lccccc", header, body, note)

# Absolute counts distinguish expansion of enterprise litigation from a share
# change caused only by a loss of individual-client cases. Zero counts remain.
client_res <- list(est("log_enterprise_cases"), est("log_personal_cases"))
client_header <- c(" & (1) & (2) \\\\",
                   "Client type & Enterprise & Individual \\\\")
client_body <- c(
  paste("Winner $\\times$ Post &", paste(sapply(client_res, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(client_res, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Winner pre-award mean &", paste(sapply(client_res, function(r) fmt_num(r$pre_mean)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(client_res, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "Stack $\\times$ Firm FE & Yes & Yes \\\\",
  "Stack $\\times$ Year FE & Yes & Yes \\\\"
)
client_note <- paste(
  "Firm-year stacked DID for log(1 + cases) by client type, retaining zero counts in the event window $[-5, 5]$.",
  "The sample comprises winners and comparison firms sampled for the same event; cases measure recorded civil litigation, not unique clients or revenue.",
  "Pre-award means use the transformed outcome; standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "firm_client_counts_table.tex"),
                "Civil Caseload by Client Type",
                "tab:firm_client_counts", "lcc", client_header, client_body, client_note)
