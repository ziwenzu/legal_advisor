#!/usr/bin/env Rscript
# lawyer_political_incorporation.R  (H7: supporting political comparisons)
#
# Are the contracted firm's lawyers drawn into representative and professional
# bodies after the award?
#   Panel A. Firm-year stacked DID on the share of the firm's senior lawyers
#            holding an NPC or CPPCC seat, any public or professional
#            position, and a bar association office separately.
#   Panel B. Lawyer-year stacked DID (LPM) with lawyer and stack x year fixed
#            effects: NPC or CPPCC identity; any public or professional position;
#            bar office;
#            outstanding-lawyer award in the year.
# Event study for the firm-year share.
#
# Outputs: output/tables/lawyer_political_incorporation_table.tex
#          output/figures/lawyer_political_position_event_study.pdf

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)
lw <- fread(file.path(dirs$data, "lawyer_year_panel.csv"), encoding = "UTF-8")
lw[, formal_position_any := as.integer(npc_deputy == 1L | cppcc_member == 1L)]
stopifnot(all(lw$formal_position_any == lw$npc_cppcc_member))
formal_shares <- lw[, .(senior_formal_position_share = mean(formal_position_any)),
                   by = .(award_id, firm_id, year)]

fy <- fread(file.path(dirs$data, "firm_level.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "firm_year_supplement.csv"), encoding = "UTF-8")
fy <- merge(fy, sup, by = c("award_id", "firm_id", "year"))
if("senior_formal_position_share" %in% names(fy)) fy[,senior_formal_position_share:=NULL]
fy <- merge(fy, formal_shares, by = c("award_id", "firm_id", "year"), all.x = TRUE)
fy[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]
fy[, stack_year_fe := sprintf("%s__%s", award_id, year)]
fy[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
fy <- fy[!is.na(et)]
fy[, bar_share := senior_bar_office_n / senior_lawyer_panel_n]

estA <- function(outcome) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome)), data = fy, cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment"); r$pre_mean <- fy[treated_firm == 1L & event_time < 0, mean(get(outcome))]; r
}
resA <- list(estA("senior_formal_position_share"), estA("senior_political_share"), estA("bar_share"))
labelsA <- c("NPC / CPPCC identity", "Any position incl. bar", "Bar association office")

lw[, et := fifelse(event_time < -5 | event_time > 5, NA_real_, as.numeric(event_time))]
lw <- lw[!is.na(et)]
lw[, stack_year_fe := sprintf("%s__%s", award_id, year)]
lw[, lawyer_stack_fe := sprintf("%s__%s", award_id, lawyer_id)]
estB <- function(outcome) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment | lawyer_stack_fe + stack_year_fe", outcome)), data = lw, cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment"); r$pre_mean <- lw[treated_firm == 1L & event_time < 0, mean(get(outcome))]; r
}
resB <- list(estB("formal_position_any"), estB("any_political_position"), estB("bar_assoc_office"), estB("outstanding_lawyer_award"))
labelsB <- c("NPC / CPPCC identity", "Any position incl. bar", "Bar association office", "Outstanding-lawyer award")
# Party membership modifies the combined representative-identity outcome.
m_het <- feols(formal_position_any ~ did_treatment + did_treatment:lawyer_ccp | lawyer_stack_fe + stack_year_fe, data = lw, cluster = ~ award_id + firm_id)
het <- pull_term(m_het, "did_treatment:lawyer_ccp")

m_es <- feols(senior_formal_position_share ~ i(et, treated_firm, ref = -1) | stack_firm_fe + stack_year_fe, data = fy, cluster = ~ award_id + firm_id)
es_dt <- extract_event_dt(m_es)
ct_es <- as.data.table(coeftable(m_es), keep.rownames="term")
es_dt[, term := sprintf("et::%s:treated_firm",event_time)]
es_dt[, se := ct_es[["Std. Error"]][match(term,ct_es$term)]]
es_dt[, p_value := ct_es[["Pr(>|t|)"]][match(term,ct_es$term)]]
es_dt[, term := NULL]
pre <- joint_pretest(m_es, "treated_firm", "et", c(-5, -4, -3, -2))
plot_event_study(es_dt, "Share of Senior Lawyers Holding an NPC or CPPCC Seat", "Years Since the Contract",
                 resA[[1]]$estimate, resA[[1]]$se, pre$p_value,
                 file.path(dirs$figures, "lawyer_formal_position_event_study.pdf"))
file.copy(file.path(dirs$figures, "lawyer_formal_position_event_study.pdf"),
          file.path(dirs$figures, "lawyer_political_position_event_study.pdf"), overwrite = TRUE)
formal_results <- rbindlist(list(
  rbindlist(lapply(seq_along(resA), function(i) data.table(panel="firm_year",component=labelsA[i],outcome=c("senior_formal_position_share","senior_political_share","bar_share")[i],as.data.table(resA[[i]])))),
  rbindlist(lapply(seq_along(resB), function(i) data.table(panel="lawyer_year",component=labelsB[i],outcome=c("formal_position_any","any_political_position","bar_assoc_office","outstanding_lawyer_award")[i],as.data.table(resB[[i]]))))
),fill=TRUE)
fwrite(formal_results,file.path(dirs$tables,"lawyer_political_incorporation_results.csv"))
fwrite(es_dt,file.path(dirs$tables,"lawyer_formal_position_event_coefficients.csv"))

header <- c(" & (1) & (2) & (3) & (4) \\\\")
body <- c(
  "\\multicolumn{5}{l}{\\textit{Panel A. Firm-year stacked DID: share of the firm's senior lawyers}} \\\\",
  paste("Outcome &", paste(labelsA, collapse = " & "), "& \\\\"),
  paste("Winner $\\times$ Post &", paste(sapply(resA, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "& \\\\"),
  paste("&", paste(sapply(resA, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "& \\\\"),
  paste("Winner pre-award mean &", paste(sapply(resA, function(r) fmt_num(r$pre_mean)), collapse = " & "), "& \\\\"),
  paste("Observations &", paste(sapply(resA, function(r) fmt_int(r$n_obs)), collapse = " & "), "& \\\\"),
  "\\midrule",
  "\\multicolumn{5}{l}{\\textit{Panel B. Lawyer-year stacked DID (linear probability)}} \\\\",
  paste("Outcome &", paste(labelsB, collapse = " & "), "\\\\"),
  paste("Winner $\\times$ Post &", paste(sapply(resB, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(resB, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Winner pre-award mean &", paste(sapply(resB, function(r) fmt_num(r$pre_mean)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(resB, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  paste("Winner $\\times$ Post $\\times$ party member (col.\\ 1) &", coef_cell(het$estimate, het$se, het$p_value)[1], "& & & \\\\"),
  paste("&", coef_cell(het$estimate, het$se, het$p_value)[2], "& & & \\\\"),
  "\\addlinespace",
  "Unit FE (stack $\\times$ firm / stack $\\times$ lawyer) & Yes & Yes & Yes & Yes \\\\",
  "Stack $\\times$ Year FE & Yes & Yes & Yes & Yes \\\\"
)
note <- paste(
  "Supporting political comparisons for H7. Formal political identity is an NPC deputy or CPPCC member, counting dual holders once. Panel A uses the shares of the firm's senior lawyers (lawyers with at least five practice years in the tracked panel) with formal identity, any public or professional position, and a bar association office separately.",
  "Panel B uses the lawyer-year panel with lawyer-by-stack and stack-by-year fixed effects. Formal identity excludes bar offices; the broader any-position outcome includes them. Representative seats have finite terms and may be renewed. The award is a yearly indicator.",
  "The last row of Panel B interacts the formal-identity treatment effect with party membership.",
  sprintf("The firm-year event study (Figure~\\ref{fig:lawyer_political_position_es}) has a joint pre-period $p$-value of %s.", fmt_p(pre$p_value)),
  "Standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "lawyer_political_incorporation_table.tex"),
                "Political Incorporation of the Contracted Firm's Lawyers",
                "tab:lawyer_political_incorporation", "lcccc", header, body, note)
