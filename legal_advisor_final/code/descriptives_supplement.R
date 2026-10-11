#!/usr/bin/env Rscript
# descriptives_supplement.R
#
# Descriptive statistics for the supplementary variables and panels:
#   Panel A. Tender attributes (266 tenders).
#   Panel B. Administrative case supplement (representation, act timing,
#            withdrawal reason, head appearance, normative-document review).
#   Panel C. Firm-year supplement (administrative representation by side,
#            criminal defense, branches, political positions).
#   Panel D. Criminal document file and lawyer-year panel.
# Also a table of the government's representation by event time (who
# represents the state as the contract matures).
#
# Outputs: output/tables/summary_statistics_supplement_table.tex
#          output/tables/admin_representation_by_event_time_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

stat_row <- function(label, x, digits = 3) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(paste(label, "& 0 & & & & \\\\"))
  big <- max(abs(x), na.rm = TRUE) >= 1000
  f <- function(v) if (big) fmt_int(v) else fmt_num(v, digits)
  paste(label, "&", fmt_int(length(x)), "&", f(mean(x)), "&", f(sd(x)), "&", f(min(x)), "&", f(max(x)), "\\\\")
}

tender <- fread(file.path(dirs$data, "city_tender_attributes.csv"), encoding = "UTF-8")
adm <- fread(file.path(dirs$data, "admin_case_level.csv"), encoding = "UTF-8",
             select = c("case_no", "treated_city", "post", "event_time", "government_has_lawyer", "withdraw_case"))
sup <- fread(file.path(dirs$data, "admin_case_supplement.csv"), encoding = "UTF-8")
adm <- merge(adm, sup, by = "case_no")
fy <- fread(file.path(dirs$data, "firm_year_supplement.csv"), encoding = "UTF-8")
crim <- fread(file.path(dirs$data, "criminal_document_level.csv"), encoding = "UTF-8")
lw <- fread(file.path(dirs$data, "lawyer_year_panel.csv"), encoding = "UTF-8")

adm[, contracted := as.integer(gov_counsel_type == "contracted_firm")]
adm[, other_firm := as.integer(gov_counsel_type == "other_counsel")]
adm[, in_house := as.integer(gov_counsel_type == "in_house_only")]
adm[, wd_voluntary := fifelse(withdraw_case == 1L, as.integer(withdrawal_reason == "voluntary"), NA_integer_)]
adm[, wd_agency := fifelse(withdraw_case == 1L, as.integer(withdrawal_reason == "agency_changed_act"), NA_integer_)]

body <- c(
  "\\multicolumn{6}{l}{\\textit{Panel A. Procurement tenders}} \\\\",
  stat_row("Contract value (yuan)", tender$contract_value_yuan),
  stat_row("Budget ceiling (yuan)", tender$budget_yuan),
  stat_row("Bodies covered", tender$contract_scope_n_bodies, 2),
  stat_row("Bidders per tender", tender$n_bidders, 2),
  stat_row("Contract term (years)", tender$contract_term_years, 2),
  stat_row("Public tender", as.integer(tender$procurement_method == "public_tender")),
  stat_row("Competitive consultation", as.integer(tender$procurement_method == "competitive_consultation")),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel B. Administrative cases (supplement)}} \\\\",
  stat_row("Government: in-house staff only", adm$in_house),
  stat_row("Government: other law firm", adm$other_firm),
  stat_row("Government: contracted firm", adm$contracted),
  stat_row("Contracted firm, post-award cases in adopting cities", adm[treated_city == 1L & post == 1L]$contracted),
  stat_row("Years from contested act to filing", adm$act_lag_years, 2),
  stat_row("Act decided after contract (post-award cases)", adm[treated_city == 1L & post == 1L]$act_after_contract),
  stat_row("Withdrawal: voluntary (among withdrawals)", adm$wd_voluntary),
  stat_row("Withdrawal: agency changed act (among withdrawals)", adm$wd_agency),
  stat_row("Agency head appeared", adm$head_appeared),
  stat_row("Normative-document review requested", adm$normative_doc_review),
  stat_row("Document found unlawful (among reviews)", adm$normative_doc_unlawful),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel C. Firm-years (supplement)}} \\\\",
  stat_row("Admin.\\ cases for plaintiffs", fy$admin_plaintiff_case_n, 2),
  stat_row("Admin.\\ cases for government", fy$admin_gov_case_n, 2),
  stat_row("Criminal cases", fy$criminal_case_n, 2),
  stat_row("Probation rate (firm-years with sentences)", fy$criminal_probation_rate),
  stat_row("Sentence / recommendation", fy$criminal_sentence_ratio_mean),
  stat_row("Branch offices", fy$branch_office_n, 2),
  stat_row("Senior lawyers tracked", fy$senior_lawyer_panel_n, 2),
  stat_row("Share of senior lawyers with a position", fy$senior_political_share),
  "\\addlinespace",
  "\\multicolumn{6}{l}{\\textit{Panel D. Criminal documents and lawyer-years}} \\\\",
  stat_row("Sentenced", crim$sentenced),
  stat_row("Prosecutor's recommended term (months)", crim$prosecutor_recommended_months, 1),
  stat_row("Sentence (months)", crim$sentence_months, 1),
  stat_row("Probation (sentenced)", crim$probation),
  stat_row("Lawyer: party member", lw$lawyer_ccp),
  stat_row("Lawyer: practice years", lw$practice_years, 1),
  stat_row("Lawyer: congress / CPPCC seat", lw$npc_cppcc_member),
  stat_row("Lawyer: bar association office", lw$bar_assoc_office),
  stat_row("Lawyer: outstanding-lawyer award (year)", lw$outstanding_lawyer_award)
)
header <- c("Variable & N & Mean & SD & Min & Max \\\\")
note <- paste(
  "Descriptive statistics for the supplementary files (README section 5.5).",
  sprintf("Tenders: %s; administrative cases: %s; firm-years: %s; criminal documents: %s; lawyer-years: %s (%s lawyers).",
          fmt_int(nrow(tender)), fmt_int(nrow(adm)), fmt_int(nrow(fy)), fmt_int(nrow(crim)), fmt_int(nrow(lw)), fmt_int(uniqueN(lw$lawyer_id))),
  "Each row's $N$ counts non-missing values."
)
write_tpt_table(file.path(dirs$tables, "summary_statistics_supplement_table.tex"),
                "Descriptive Statistics: Supplementary Variables and Panels",
                "tab:summary_statistics_supplement", "lccccc", header, body, note, size = "\\scriptsize")

# Representation by event time (adopting cities)
rep_dt <- adm[treated_city == 1L & event_time >= -3 & event_time <= 5,
              .(N = .N, in_house = mean(in_house), other = mean(other_firm), contracted = mean(contracted),
                winner_firm = mean(gov_counsel_winner_firm)), by = event_time][order(event_time)]
header2 <- c("Years since award & Cases & In-house only & Other law firm & Contracted firm & Winner firm (any period) \\\\")
body2 <- sapply(seq_len(nrow(rep_dt)), function(i) {
  r <- rep_dt[i]
  paste(r$event_time, "&", fmt_int(r$N), "&", fmt_num(r$in_house), "&", fmt_num(r$other), "&",
        fmt_num(r$contracted), "&", fmt_num(r$winner_firm), "\\\\")
})
note2 <- paste(
  "Who represents the government in administrative cases in adopting cities, by year relative to the award.",
  "``Contracted firm'' is the firm holding the city's contract in that year; ``winner firm (any period)'' also counts appearances by the eventual winner before the award, the prior-exposure margin used in the triple-difference design."
)
write_tpt_table(file.path(dirs$tables, "admin_representation_by_event_time_table.tex"),
                "The Government's Representation by Years Since the Award",
                "tab:admin_representation_by_event_time", "lccccc", header2, body2, note2)
