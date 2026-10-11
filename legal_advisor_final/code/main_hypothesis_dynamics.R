#!/usr/bin/env Rscript
# Annual estimates and joint pre-period diagnostics for the main outcome groups.
suppressPackageStartupMessages({library(data.table);library(fixest);library(did)})
arg <- grep("^--file=",commandArgs(trailingOnly=FALSE),value=TRUE)
root_dir <- if(length(arg)) normalizePath(file.path(dirname(normalizePath(sub("^--file=","",arg[1]))),"..")) else normalizePath(getwd())
source(file.path(root_dir,"code/analysis_helpers.R"))
dirs <- sb_dirs(root_dir)
setFixest_nthreads(4)
case_rows <- list(); new_pre <- list()
adm <- fread(file.path(dirs$data,"admin_case_level.csv"),encoding="UTF-8")
adm <- adm[!is.na(court_std) & court_std!=""]
adm[,city_id:=.GRP,by=.(province,city)]
adm[,court_id:=.GRP,by=court_std]
stopifnot(all(is.finite(adm[treated_city==1,event_time])))
adm[,et_bin:=fifelse(treated_city==0,-1,pmin(pmax(event_time,-5),5))]
specs <- data.table(
 endpoint_id=c("H2_case_counsel","H2_merits_win","H3_withdrawal","H3_termination","H3_case_petition"),
 hypothesis=c("H2","H2","H3","H3","H3"),
 outcome=c("government_has_lawyer","government_win","withdraw_case","end_case","petitioned"),
 component=c("Government has counsel","Government wins in merits cases","Withdrawal","Procedural termination","Case petition"),
 expected_direction=c(1,1,1,1,-1),merits_only=c(FALSE,TRUE,FALSE,FALSE,FALSE),
 controls=c("plaintiff_is_entity + opponent_has_lawyer",rep("government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity",4)))
for(j in seq_len(nrow(specs))) {
 s <- specs[j]
 d <- if(s$merits_only) adm[withdraw_case==0L & end_case==0L] else adm
 f <- as.formula(sprintf("%s ~ i(et_bin,treated_city,ref=-1) + %s | court_id + year + cause_group",s$outcome,s$controls))
 m <- feols(f,data=d,cluster=~city_id+court_id,notes=FALSE)
 ct <- as.data.table(coeftable(m),keep.rownames="term")[grepl("^et_bin::-?[0-9]+:treated_city$",term)]
 ci <- confint(m,level=.95)
 z <- data.table(endpoint_id=s$endpoint_id,hypothesis=s$hypothesis,outcome=s$outcome,component=s$component,
  expected_direction=s$expected_direction,event_time=as.integer(sub("^et_bin::(-?[0-9]+):treated_city$","\\1",ct$term)),
  term=ct$term,estimate=ct$Estimate,se=ct[["Std. Error"]],p_value=ct[["Pr(>|t|)"]],
  ci_lo=ci[ct$term,1],ci_hi=ci[ct$term,2],is_ref=FALSE,n_obs=nobs(m),
  specification=paste(deparse(f),collapse=" "),source_script="main_hypothesis_dynamics.R")
 z <- rbind(z,data.table(endpoint_id=s$endpoint_id,hypothesis=s$hypothesis,outcome=s$outcome,component=s$component,
  expected_direction=s$expected_direction,event_time=-1L,estimate=0,se=0,p_value=NA_real_,ci_lo=0,ci_hi=0,is_ref=TRUE),fill=TRUE)
 setorder(z,event_time)
 case_rows[[s$endpoint_id]] <- z
 w <- joint_pretest(m,"treated_city","et_bin",c(-5,-4,-3,-2))
 new_pre[[s$endpoint_id]] <- data.table(endpoint_id=s$endpoint_id,hypothesis=s$hypothesis,outcome=s$outcome,
  component=s$component,pre_periods="-5,-4,-3,-2",reference_period=-1L,statistic=w$stat,p_value=w$p_value,
  n_obs=nobs(m),inference="Original city/court clustered Wald test; outer event years binned")
 avg <- feols(as.formula(sprintf("%s ~ did_treatment + %s | court_id + year + cause_group",s$outcome,s$controls)),data=d,cluster=~city_id+court_id,notes=FALSE)
 av <- pull_term(avg,"did_treatment")
 plot_event_study(z,s$component,"Years Since First Recorded Award",av$estimate,av$se,w$p_value,
  file.path(dirs$figures,paste0(tolower(s$endpoint_id),"_event_study.pdf")))
}
fwrite(rbindlist(case_rows,fill=TRUE),file.path(dirs$tables,"main_case_event_coefficients.csv"))
rm(adm,d,m,avg);gc(FALSE)

# Reuse original city estimator functions without running their export entry point.
ce <- new.env(parent=globalenv())
for(expr in parse(file.path(root_dir,"code/city_year_cs_twfe_figures_tables.R"))) {
 if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&length(expr)==3L&&is.call(expr[[3]])&&identical(expr[[3]][[1]],as.name("function"))) eval(expr,ce)
}
ce$NEVER_TREATED_SENTINEL <- 0
cy <- ce$read_city_panel(file.path(dirs$data,"city_year_panel.csv"))
cs <- ce$estimate_cs(copy(cy),"petition_rate")
pet_cs <- ce$extract_cs_event(cs)
pet_cs[,`:=`(endpoint_id="H3_city_petition_CS",hypothesis="H3",outcome="petition_rate",component="City petition rate: CS",
 expected_direction=-1L,is_ref=FALSE,source_script="main_hypothesis_dynamics.R")]
pw <- ce$extract_cs_pretest(cs)
new_pre[["H3_city_petition_CS"]] <- data.table(endpoint_id="H3_city_petition_CS",hypothesis="H3",outcome="petition_rate",
 component="City petition rate: CS",pre_periods=paste(cs$agg_dynamic$egt[cs$agg_dynamic$egt<0],collapse=","),
 reference_period=NA_integer_,statistic=pw$stat,p_value=pw$p_value,n_obs=nrow(cy),
 inference="All available negative event periods; varying base; analytical influence-function covariance")
cy[,et_bin:=fifelse(ever_treated==0,-1,pmin(pmax(rel_time,-5),5))]
pm <- feols(petition_rate ~ i(et_bin,ever_treated,ref=-1) + log_population_10k + log_gdp + log_registered_lawyers | city_id+year,data=cy,cluster=~city_id)
ct <- as.data.table(coeftable(pm),keep.rownames="term")[grepl("^et_bin::-?[0-9]+:ever_treated$",term)]
ci <- confint(pm)
pet_twfe <- data.table(endpoint_id="H3_city_petition_TWFE",hypothesis="H3",outcome="petition_rate",component="City petition rate: TWFE",
 expected_direction=-1L,event_time=as.integer(sub("^et_bin::(-?[0-9]+):ever_treated$","\\1",ct$term)),estimate=ct$Estimate,
 se=ct[["Std. Error"]],p_value=ct[["Pr(>|t|)"]],ci_lo=ci[ct$term,1],ci_hi=ci[ct$term,2],is_ref=FALSE,source_script="main_hypothesis_dynamics.R")
tw <- joint_pretest(pm,"ever_treated","et_bin",c(-5,-4,-3,-2))
new_pre[["H3_city_petition_TWFE"]] <- data.table(endpoint_id="H3_city_petition_TWFE",hypothesis="H3",outcome="petition_rate",
 component="City petition rate: TWFE",pre_periods="-5,-4,-3,-2",reference_period=-1L,statistic=tw$stat,p_value=tw$p_value,
 n_obs=nobs(pm),inference="Original city clustered Wald test; outer event years binned")
fwrite(rbindlist(new_pre,fill=TRUE),file.path(dirs$tables,"main_dynamic_joint_pretests.csv"))
fwrite(rbindlist(list(pet_cs,pet_twfe),fill=TRUE),file.path(dirs$tables,"city_petition_event_coefficients.csv"))
pet_avg <- ce$extract_cs_coef(cs)
ce$EVENT_PLOT_LAYOUT <- list()
ce$plot_event_study(ce$build_plot_data(pet_cs),"City Petition Rate","Petition Rate",pet_avg$estimate,pet_avg$se,pw$p_value,
 file.path(dirs$figures,"city_petition_rate_event_study.pdf"))

# Explicitly retain one trajectory per declared annual comparison.
all <- list(); put <- function(z,id,h,label,direction,source) {
 z <- copy(z)
 z[,`:=`(endpoint_id=id,hypothesis=h,component=label,expected_direction=direction,source_script=source)]
 needed <- c("endpoint_id","hypothesis","outcome","component","expected_direction","event_time","estimate","se","p_value","ci_lo","ci_hi","source_script")
 stopifnot(all(needed %in% names(z)))
 all[[id]] <<- z[,..needed]
}
native_city <- fread(file.path(dirs$tables,"city_cs_event_coefficients.csv"))
for(o in c("government_win_rate","appeal_rate")) put(native_city[outcome==o],paste0("H1_CS_",o),"H1",o,if(o=="appeal_rate") -1 else 1,"city_year_cs_twfe_figures_tables.R")
native_firm <- fread(file.path(dirs$tables,"firm_event_coefficients_and_support.csv"))
for(o in unique(native_firm$outcome)) put(native_firm[outcome==o],paste0("H4_",o),"H4",o,if(o=="avg_filing_to_hearing_days") -1 else 1,"firm_level_stacked_did_fixest.R")
native_diag <- fread(file.path(dirs$tables,"hypothesis_diagnostics_event_coefficients.csv"))
ids <- c("diag_H4_log_civil_cases","diag_H6_log_admin_plaintiff","diag_H6_any_plaintiff_case","diag_H6_log_admin_gov",
 "diag_H5_pure_private_case_win_binary","diag_H5_pure_private_case_win_rate_fee","diag_H3_nonlocal_combined_exit")
for(id in ids) {
 z <- native_diag[endpoint_id==id]
 stopifnot(nrow(z)>0,uniqueN(z$outcome)==1L)
 direction <- if(unique(z$outcome) %in% c("log_admin_plaintiff","any_plaintiff_case")) -1 else 1
 put(z,id,unique(z$hypothesis),id,direction,unique(z$source_script))
}
side <- fread(file.path(dirs$tables,"firm_side_event_coefficients.csv"))
put(side,"H6_plaintiff_share","H6","Plaintiff-side share",-1,"firm_side_switching.R")
political <- fread(file.path(dirs$tables,"political_leadership_event_coefficients.csv"))
for(label in unique(political$component)) {
 z <- political[component==label]
 put(z,paste0("H7_",match(label,unique(political$component))),"H7",label,1,"political_leadership_results.R")
}
for(id in names(case_rows)) {
 z <- case_rows[[id]]
 put(z,id,unique(z$hypothesis),unique(z$component),unique(z$expected_direction),"main_hypothesis_dynamics.R")
}
put(pet_cs,"H3_city_petition_CS","H3","City petition rate: CS",-1,"main_hypothesis_dynamics.R")
put(pet_twfe,"H3_city_petition_TWFE","H3","City petition rate: TWFE",-1,"main_hypothesis_dynamics.R")
combined <- rbindlist(all,fill=TRUE)
stopifnot(uniqueN(combined$endpoint_id)==29L,!anyDuplicated(combined[,.(endpoint_id,event_time)]))
combined[,`:=`(significant_at_005=is.finite(p_value)&p_value<.05,direction_supported=sign(estimate)==expected_direction)]
early <- combined[event_time %in% 0:3]
stopifnot(nrow(early)==116L,early[,all(sort(event_time)==0:3),by=endpoint_id]$V1)
early[,p_holm_within_hypothesis:=p.adjust(p_value,method="holm"),by=hypothesis]
early[,assessment:=fifelse(!is.finite(p_value),"UNAVAILABLE",fifelse(significant_at_005&direction_supported,"DIRECTION_AND_P_SUPPORTED",fifelse(direction_supported,"DIRECTION_ONLY_NOT_SIGNIFICANT","OPPOSITE_DIRECTION")))]
fwrite(combined,file.path(dirs$tables,"main_hypothesis_event_coefficients.csv"))
fwrite(early,file.path(dirs$tables,"main_hypothesis_first_four_periods.csv"))
summary <- early[,.(periods=.N,supported=sum(assessment=="DIRECTION_AND_P_SUPPORTED"),max_p=max(p_value),all_supported=all(assessment=="DIRECTION_AND_P_SUPPORTED")),by=.(endpoint_id,hypothesis,component)]
fwrite(summary,file.path(dirs$tables,"main_hypothesis_first_four_summary.csv"))
display_labels <- c(diag_H4_log_civil_cases="Civil case volume",diag_H6_log_admin_plaintiff="Plaintiff-side administrative volume",
 diag_H6_any_plaintiff_case="Any plaintiff-side case",diag_H6_log_admin_gov="Government-side administrative volume",
 diag_H5_pure_private_case_win_binary="Private-party civil wins",diag_H5_pure_private_case_win_rate_fee="Private-party fee success",
 diag_H3_nonlocal_combined_exit="Non-local exit difference",civil_win_rate_mean="Civil win rate",
 civil_win_rate_fee_mean="Civil fee success",avg_filing_to_hearing_days="Time to hearing",log_firm_size="Firm size",enterprise_share="Enterprise-client share",
 government_win_rate="City government wins: CS",appeal_rate="City appeal rate: CS")
format_p <- function(x) if(!is.finite(x)) "NA" else if(x<.001) "$<0.001$" else sprintf("%.3f",x)
row_end <- paste0(" ",intToUtf8(c(92L,92L)))
tex_rows <- vapply(summary$endpoint_id,function(id) {
 z <- early[endpoint_id==id][order(event_time)]
 label <- z$component[1]
 if(label %in% names(display_labels)) label <- unname(display_labels[label])
 paste0(sprintf("%s: %s & %s & %s & %s & %s & %d/4",z$hypothesis[1],label,
  format_p(z$p_value[1]),format_p(z$p_value[2]),format_p(z$p_value[3]),format_p(z$p_value[4]),sum(z$assessment=="DIRECTION_AND_P_SUPPORTED")),row_end)
},character(1))
writeLines(c("\\begingroup\\footnotesize", "\\setlength{\\tabcolsep}{4pt}",
 "\\begin{longtable}{p{0.47\\linewidth}rrrrr}",
 "\\caption{Annual Effects in the First Four Award Periods}\\label{tab:main_early_dynamic}\\\\",
 paste0("\\toprule Outcome and comparison & $p_0$ & $p_1$ & $p_2$ & $p_3$ & Supported",row_end),
 "\\midrule\\endfirsthead", paste0("\\toprule Outcome and comparison & $p_0$ & $p_1$ & $p_2$ & $p_3$ & Supported",row_end),
 "\\midrule\\endhead",tex_rows,"\\bottomrule\\end{longtable}",
 "\\noindent Reported model p values use the original estimator and covariance. Supported means the declared direction and $p<0.05$. Zero denotes the award calendar year. Annual Holm adjustments, coefficients and intervals are retained in the accompanying CSV. Later periods remain separately reported.",
 "\\endgroup"),file.path(dirs$tables,"main_hypothesis_first_four_summary_table.tex"))
cat("Declared annual comparisons:",nrow(summary),"; early period coefficients:",nrow(early),"; supported:",sum(early$assessment=="DIRECTION_AND_P_SUPPORTED"),"\n")
