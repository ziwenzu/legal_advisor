#!/usr/bin/env Rscript
# firm_criminal_defense.R  (H4, portability into criminal defense)
#
# Does the contracted firm's advantage extend to criminal defense, where
# outcomes turn on relationships with police, procurators and courts?
#   Panel A. Firm-year stacked DID: criminal share of the docket, probation
#            rate, mean sentence-to-recommendation ratio.
#   Panel B. Document-level DID on the criminal document file: probation
#            (LPM) and the sentence ratio, with charge-group, court-level and
#            firm / stack x year fixed effects.
# Event study for the probation rate.
#
# Outputs: output/tables/firm_criminal_defense_table.tex
#          output/figures/firm_criminal_probation_event_study.pdf

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
fy[, criminal_share := fifelse(criminal_case_n + civil_case_n >= 10, criminal_case_n / (criminal_case_n + civil_case_n), NA_real_)]
fy[, log_criminal_cases := log1p(criminal_case_n)]

estA <- function(outcome) {
  d <- fy[!is.na(get(outcome))]
  m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome)), data = d, cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment")
  r$pre_mean <- fy[treated_firm == 1L & event_time < 0 & !is.na(get(outcome)), mean(get(outcome))]
  r
}
resA <- list(estA("log_criminal_cases"), estA("criminal_share"), estA("criminal_probation_rate"), estA("criminal_sentence_ratio_mean"))
labelsA <- c("Log(1 + criminal cases)", "Criminal share of docket", "Probation rate", "Sentence / recommendation")

crim <- fread(file.path(dirs$data, "criminal_document_level.csv"), encoding = "UTF-8")
crim[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
crim <- crim[!is.na(et) & sentenced == 1L]
crim[, did_treatment := as.integer(treated_firm == 1L & event_time >= 0)]
crim[, stack_year_fe := sprintf("%s__%s", award_id, year)]
crim[, log_rec := log(prosecutor_recommended_months)]
estB <- function(outcome) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment + log_rec + defendant_is_entity | firm_id + stack_year_fe + charge_group + court_level", outcome)),
             data = crim, cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment")
  r$pre_mean <- crim[treated_firm == 1L & event_time < 0, mean(get(outcome))]
  r
}
resB <- list(estB("probation"), estB("sentence_ratio"))
labelsB <- c("Probation (LPM)", "Sentence / recommendation")

# Event study: firm-year probation rate
m_es <- feols(criminal_probation_rate ~ i(et, treated_firm, ref = -1) | stack_firm_fe + stack_year_fe,
              data = fy[!is.na(criminal_probation_rate)], cluster = ~ award_id + firm_id)
es_dt <- extract_event_dt(m_es)
pre <- joint_pretest(m_es, "treated_firm", "et", c(-5, -4, -3, -2))
plot_event_study(es_dt, "Probation Rate in Criminal Defense", "Years Since the Contract",
                 resA[[3]]$estimate, resA[[3]]$se, pre$p_value,
                 file.path(dirs$figures, "firm_criminal_probation_event_study.pdf"))

header <- c(" & (1) & (2) & (3) & (4) \\\\")
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. Firm-year stacked DID}} \\\\",
  paste("Outcome &", paste(labelsA, collapse = " & "), "\\\\"),
  paste("Winner $\\times$ Post &", paste(sapply(resA, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(resA, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Winner pre-award mean &", paste(sapply(resA, function(r) fmt_num(r$pre_mean)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(resA, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Document-level DID, sentenced defendants}} \\\\",
  paste("Outcome &", paste(labelsB, collapse = " & "), "& & \\\\"),
  paste("Winner $\\times$ Post &", paste(sapply(resB, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "& & \\\\"),
  paste("&", paste(sapply(resB, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "& & \\\\"),
  paste("Winner pre-award mean &", paste(sapply(resB, function(r) fmt_num(r$pre_mean)), collapse = " & "), "& & \\\\"),
  paste("Observations &", paste(sapply(resB, function(r) fmt_int(r$n_obs)), collapse = " & "), "& & \\\\"),
  "\\addlinespace",
  "Stack $\\times$ Firm FE (A) / Firm FE (B) & Yes & Yes & Yes & Yes \\\\",
  "Stack $\\times$ Year FE & Yes & Yes & Yes & Yes \\\\",
  "Charge-group and court-level FE (B) & Yes & Yes & & \\\\"
)
note <- paste(
  "Portability of the contracted firm's advantage into criminal defense (H4).",
  "Panel A uses the firm-year panel of winners and sampled comparison firms (event window $[-5, 5]$); the criminal share is the firm's criminal cases over its criminal plus civil cases, defined for firm-years with at least ten cases.",
  "Panel B uses the criminal document file for sentenced defendants, controlling for the log of the prosecutor's recommended term and whether the defendant is an entity.",
  sprintf("The probation-rate event study (Figure~\\ref{fig:firm_criminal_probation_es}) has a joint pre-period $p$-value of %s.", fmt_p(pre$p_value)),
  "Standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "firm_criminal_defense_table.tex"),
                "Criminal Defense Outcomes of the Contracted Firm",
                "tab:firm_criminal_defense", "lcccc", header, body, note)
