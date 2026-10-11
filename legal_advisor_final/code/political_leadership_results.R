#!/usr/bin/env Rscript
# NPC deputies and CPPCC members: holdings, annual acquisition, and award selection.
args <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", args[1])), ".."))
source(file.path(root, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root)
fy <- fread(file.path(dirs$data, "firm_level.csv"))
fy <- merge(fy, fread(file.path(dirs$data, "firm_year_supplement.csv")), by=c("award_id","firm_id","year"))
lw <- fread(file.path(dirs$data, "lawyer_position_year.csv"))
terms <- fread(file.path(dirs$data, "lawyer_position_terms.csv"))

# Office spells have an inclusive appointment year and an exclusive end year.
# A holder of either continuing office is outside the combined acquisition risk set.
active <- terms[position_type %in% c("npc","cppcc") & appointment_year<2021 & actual_end_year>2010,
  .(year=seq.int(max(2010L,appointment_year),min(2020L,actual_end_year-1L))),
  by=.(lawyer_id,firm_id,position_id,position_type,appointment_year,actual_end_year,is_renewal)]
annual <- active[, .(formal_from_terms=1L,
  formal_continuing=as.integer(any(appointment_year<year)),
  formal_renewal=as.integer(any(appointment_year==year & is_renewal==1L)),
  formal_term_entry=as.integer(any(appointment_year==year & is_renewal==0L))),
  by=.(lawyer_id,year)]
lw <- merge(lw,annual,by=c("lawyer_id","year"),all.x=TRUE)
for (v in c("formal_from_terms","formal_continuing","formal_renewal","formal_term_entry"))
  set(lw,which(is.na(lw[[v]])),v,0L)
lw[, formal_position_any:=as.integer(npc_deputy==1L | cppcc_member==1L)]
stopifnot(all(lw$formal_position_any==lw$formal_from_terms))
lw[, formal_position_at_risk:=as.integer(formal_continuing==0L & formal_renewal==0L)]
lw[, formal_position_new_appointment:=formal_term_entry*formal_position_at_risk]
# An expired office may be acquired again; uninterrupted same-office renewals are excluded from nonrenewal appointments.
lw[, npc_at_risk:=as.integer(!(npc_deputy==1L & npc_term_start_year<year) & npc_renewal==0L)]
lw[is.na(npc_at_risk),npc_at_risk:=1L]

lead_annual <- lw[leadership_role==1L, .(
  leadership_formal_position_n=sum(formal_position_any),
  leadership_formal_position_any=as.integer(any(formal_position_any==1L)),
  leadership_formal_new_any=as.integer(any(formal_position_new_appointment==1L)),
  leadership_formal_position_share=mean(formal_position_any)),by=.(award_id,firm_id,year)]
existing_fields <- intersect(setdiff(names(lead_annual),c("award_id","firm_id","year")),names(fy))
if(length(existing_fields)) fy[,(existing_fields):=NULL]
fy <- merge(fy,lead_annual,by=c("award_id","firm_id","year"),all.x=TRUE)
stopifnot(all(fy$leadership_formal_position_any==fy$leadership_political_any))
fy[, `:=`(stack_firm_fe=paste(award_id,firm_id,sep="__"), stack_year_fe=paste(award_id,year,sep="__"), et=as.numeric(event_time))]
lw[, `:=`(lawyer_stack_fe=paste(award_id,lawyer_id,sep="__"), stack_year_fe=paste(award_id,year,sep="__"), et=as.numeric(event_time))]
fy <- fy[et>=-5 & et<=5]
lw <- lw[et>=-5 & et<=5]
leaders <- lw[leadership_role==1L & practice_years>=5]
risk <- leaders[formal_position_at_risk==1L]
cohort <- lw[baseline_leader==1L & practice_years>=5 & formal_position_at_risk==1L]
specs <- list(
  list(outcome="leadership_formal_position_any", data=fy, unit="stack_firm_fe", label="Leadership includes an NPC deputy or CPPCC member"),
  list(outcome="leadership_formal_new_any", data=fy, unit="stack_firm_fe", label="Leadership includes a nonrenewal NPC or CPPCC appointment"),
  list(outcome="formal_position_any", data=leaders, unit="lawyer_stack_fe", label="Leader holds an NPC or CPPCC seat"),
  list(outcome="formal_position_new_appointment", data=risk, unit="lawyer_stack_fe", label="Leader receives a nonrenewal NPC or CPPCC appointment"),
  list(outcome="formal_position_new_appointment", data=cohort, unit="lawyer_stack_fe", label="Baseline leader receives a nonrenewal NPC or CPPCC appointment"),
  list(outcome="formal_position_new_appointment", data=risk[is_director==1L], unit="lawyer_stack_fe", label="Director receives a nonrenewal NPC or CPPCC appointment"),
  list(outcome="formal_position_new_appointment", data=risk[is_director==0L & is_partner==1L], unit="lawyer_stack_fe", label="Other partner receives a nonrenewal NPC or CPPCC appointment")
)
estimate_specs <- function(specifications) rbindlist(lapply(specifications,function(s) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment | %s + stack_year_fe",s$outcome,s$unit)),data=s$data,cluster=~award_id+firm_id)
  x <- pull_term(m,"did_treatment")
  data.table(component=s$label,outcome=s$outcome,estimate=x$estimate,se=x$se,p_value=x$p_value,n_obs=x$n_obs)
}))
results <- estimate_specs(specs)
results[,p_holm:=p.adjust(p_value,method="holm")]
fwrite(results,file.path(dirs$tables,"political_leadership_results.csv"))
body <- unlist(lapply(seq_len(nrow(results)),function(i) {
  r <- results[i]
  c(paste(r$component,"&",coef_cell(r$estimate,r$se,r$p_value)[1],"&",fmt_p(r$p_holm),"&",fmt_int(r$n_obs),"\\\\"),
    paste("&",coef_cell(r$estimate,r$se,r$p_value)[2],"& & \\\\"))
}))
write_tpt_table(file.path(dirs$tables,"political_leadership_table.tex"),"NPC and CPPCC Representation among Firm Leaders","tab:political_leadership","lccc",
  "Outcome & Winner $\\times$ Post & Holm p & Observations \\\\",body,
  "Formal political identity is the union of an NPC deputy and a CPPCC member; bar association posts are separate professional offices. A person holding both offices is counted once. The first two outcomes indicate whether any leader holds an office or receives a nonrenewal representative appointment in that year. Director is included in the leadership and partner indicators; the final row contains other partners. The annual appointment sample excludes anyone with either continuing office or either current renewal, using complete office spells. Nonrenewal appointments include later re-entry and transfers to the other office type; the measure is not first-ever acquisition of political identity. The baseline cohort is defined in 2010. Unit and stack-by-year effects; standard errors clustered by stack and firm. The seven declared tests form one Holm family; the firm-level share is reported separately.")
event_results <- list()
event_cells <- list()
for (i in seq_along(specs)) {
  s <- specs[[i]]
  m <- feols(as.formula(sprintf("%s ~ i(et, treated_firm, ref=-1) | %s + stack_year_fe",s$outcome,s$unit)),data=s$data,cluster=~award_id+firm_id)
  pre <- joint_pretest(m,"treated_firm","et",c(-5,-4,-3,-2))
  event_results[[i]] <- data.table(component=s$label,outcome=s$outcome,p_value=pre$p_value)
  cells <- extract_event_dt(m)
  native <- as.data.table(coeftable(m),keep.rownames="term")[grepl("^et::-?[0-9]+:treated_firm$",term)]
  native[,event_time:=as.integer(sub("^et::(-?[0-9]+):treated_firm$","\\1",term))]
  cells <- merge(cells,native[,.(event_time,se=`Std. Error`,p_value=`Pr(>|t|)`)],by="event_time",all.x=TRUE,sort=TRUE)
  cells[,`:=`(component=s$label,outcome=s$outcome)]
  event_cells[[i]] <- cells
  if (i %in% c(1L,4L)) {
    canonical <- if(i==1L) "leadership_formal_position_event_study.pdf" else "leader_formal_acquisition_event_study.pdf"
    legacy <- if(i==1L) "leadership_cppcc_event_study.pdf" else "leader_cppcc_acquisition_event_study.pdf"
    plot_event_study(extract_event_dt(m),s$label,"Years Since the Contract",results$estimate[i],results$se[i],pre$p_value,file.path(dirs$figures,canonical))
    file.copy(file.path(dirs$figures,canonical),file.path(dirs$figures,legacy),overwrite=TRUE)
  }
}
fwrite(rbindlist(event_results),file.path(dirs$tables,"political_leadership_pretrends.csv"))
fwrite(rbindlist(event_cells),file.path(dirs$tables,"political_leadership_event_coefficients.csv"))
# Restrict annual current leaders at risk to pre-award leader IDs;
# the other comparison omits the award calendar year.
pre_leaders <- leaders[et==-1L,.(award_id,lawyer_id)]
checks <- list(pre_award_leaders=merge(risk,pre_leaders,by=c("award_id","lawyer_id")),omit_award_year=risk[et!=0])
robustness <- rbindlist(lapply(names(checks),function(label) {
  m <- feols(formal_position_new_appointment ~ did_treatment | lawyer_stack_fe + stack_year_fe,data=checks[[label]],cluster=~award_id+firm_id)
  x <- pull_term(m,"did_treatment")
  data.table(specification=label,outcome="formal_position_new_appointment",estimate=x$estimate,se=x$se,p_value=x$p_value,n_obs=x$n_obs)
}))
robustness[,p_holm:=p.adjust(p_value,method="holm")]
fwrite(robustness,file.path(dirs$tables,"political_leadership_robustness.csv"))
# Subtype estimates describe which representative body contributes to the combined result.
subtypes <- list(
  list(outcome="leadership_cppcc_any",data=fy,unit="stack_firm_fe",label="Leadership includes a CPPCC member"),
  list(outcome="leadership_npc_any",data=fy,unit="stack_firm_fe",label="Leadership includes an NPC deputy"),
  list(outcome="cppcc_member",data=leaders,unit="lawyer_stack_fe",label="Leader holds a CPPCC seat"),
  list(outcome="npc_deputy",data=leaders,unit="lawyer_stack_fe",label="Leader holds an NPC seat"),
  list(outcome="cppcc_new_appointment",data=leaders[cppcc_at_risk==1L & cppcc_renewal==0L],unit="lawyer_stack_fe",label="Leader receives a nonrenewal CPPCC appointment"),
  list(outcome="npc_new_appointment",data=leaders[npc_at_risk==1L],unit="lawyer_stack_fe",label="Leader receives a nonrenewal NPC appointment")
)
subtype_results <- estimate_specs(subtypes)
subtype_results[,p_holm:=p.adjust(p_value,method="holm")]
fwrite(subtype_results,file.path(dirs$tables,"political_leadership_subtypes.csv"))
# The share answers a different question from whether a firm has any representative.
# Retain its estimate and pre-period test, including a result near zero.
share_model <- feols(leadership_formal_position_share ~ did_treatment | stack_firm_fe + stack_year_fe,data=fy,cluster=~award_id+firm_id)
share_result <- pull_term(share_model,"did_treatment")
share_event <- feols(leadership_formal_position_share ~ i(et,treated_firm,ref=-1) | stack_firm_fe + stack_year_fe,data=fy,cluster=~award_id+firm_id)
share_pre <- joint_pretest(share_event,"treated_firm","et",c(-5,-4,-3,-2))
share_row <- data.table(component="Share of leaders holding an NPC or CPPCC seat",outcome="leadership_formal_position_share",as.data.table(share_result),pre_p_value=share_pre$p_value)
fwrite(share_row,file.path(dirs$tables,"political_leadership_share_results.csv"))
write_tpt_table(file.path(dirs$tables,"political_leadership_share_table.tex"),"Share of Firm Leaders Holding an NPC or CPPCC Seat","tab:political_leadership_share","lcc",
  "Outcome & Winner $\\times$ Post & Pre-period joint p \\\\",
  c(paste("Formal-identity share &",coef_cell(share_result$estimate,share_result$se,share_result$p_value)[1],"&",fmt_p(share_pre$p_value),"\\\\"),paste("&",coef_cell(share_result$estimate,share_result$se,share_result$p_value)[2],"& \\\\"),paste("Observations &",fmt_int(share_result$n_obs),"& \\\\")),
  "The denominator is the full current leadership roster; the numerator counts people with an NPC or CPPCC seat once. This share differs from the binary presence and annual acquisition outcomes. Firm-by-stack and stack-by-year fixed effects; standard errors clustered by stack and firm. The joint pre-period test covers years -5 through -2 relative to year -1.")
fwrite(data.table(component=share_row$component,outcome=share_row$outcome,p_value=share_pre$p_value),file.path(dirs$tables,"political_leadership_auxiliary_pretrends.csv"))
# One row per comparison firm, using only the year immediately before the award.
pre <- fy[year==event_year-1L]
pre[,`:=`(pre_leadership_formal_position=leadership_formal_position_any,log_size=log1p(firm_size),log_civil=log1p(civil_case_n))]
selection <- feols(treated_firm ~ pre_leadership_formal_position + log_size + log_civil | award_id,data=pre,cluster=~award_id)
sel <- pull_term(selection,"pre_leadership_formal_position")
fwrite(data.table(estimate=sel$estimate,se=sel$se,p_value=sel$p_value,n_obs=sel$n_obs),file.path(dirs$tables,"pre_award_political_selection.csv"))
write_tpt_table(file.path(dirs$tables,"pre_award_political_selection_table.tex"),"Pre-Award NPC or CPPCC Identity and Award Selection","tab:pre_award_political_selection","lc",
  " & Award winner \\\\",c(paste("Pre-award leadership NPC or CPPCC identity &",coef_cell(sel$estimate,sel$se,sel$p_value)[1],"\\\\"),paste("&",coef_cell(sel$estimate,sel$se,sel$p_value)[2],"\\\\"),paste("Observations &",fmt_int(sel$n_obs),"\\\\")),
  "Identity is measured at the end of the preceding calendar year; post-award information is excluded. Stack fixed effects and pre-award firm size and civil volume controls are included. This is a predictive association, not a causal effect of acquiring political identity. Comparators are sampled firms rather than verified unsuccessful bidders.")
desc <- lw[,.(lawyer_years=.N,leaders=sum(leadership_role),formal_position_holders=sum(formal_position_any),cppcc_members=sum(cppcc_member),npc_deputies=sum(npc_deputy),both_offices=sum(cppcc_member==1L & npc_deputy==1L),new_formal_position=sum(formal_position_new_appointment),new_cppcc=sum(cppcc_new_appointment),new_npc=sum(npc_new_appointment),renewed_cppcc=sum(cppcc_renewal),renewed_npc=sum(npc_renewal)),by=year]
fwrite(desc,file.path(dirs$tables,"political_identity_by_year.csv"))
tenure <- terms[,.(spells=.N,mean_served_years=mean(actual_end_year-appointment_year),midterm_share=mean(appointment_year>body_term_start_year),renewal_share=mean(is_renewal)),by=position_type]
fwrite(tenure,file.path(dirs$tables,"political_term_descriptives.csv"))
