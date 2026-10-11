#!/usr/bin/env Rscript
# Joint pre-period diagnostics for additional outcome specifications.
suppressPackageStartupMessages({library(data.table);library(fixest)})
setFixest_nthreads(4)
get_root_dir <- function() {
 arg <- grep("^--file=",commandArgs(trailingOnly=FALSE),value=TRUE)
 if(!length(arg)) return(normalizePath(getwd()))
 normalizePath(file.path(dirname(normalizePath(sub("^--file=","",arg[1]))),".."))
}
root_dir <- get_root_dir()
table_dir <- file.path(root_dir,"output/tables")
dir.create(table_dir,recursive=TRUE,showWarnings=FALSE)
group <- Sys.getenv("HYPOTHESIS_DIAGNOSTICS_GROUP",unset="all")
stopifnot(group %in% c("all","firm","document","overlap","court_fee","admin"))
pre_rows <- list();event_rows <- list()
load_functions <- function(name) {
 e <- new.env(parent=globalenv())
 for(expr in parse(file.path(root_dir,"code",name))) {
  if(is.call(expr)&&identical(expr[[1]],as.name("<-"))&&length(expr)==3L&&
    ((is.call(expr[[3]])&&identical(expr[[3]][[1]],as.name("function")))||
     identical(expr[[2]],as.name("control_rhs")))) eval(expr,envir=e)
 }
 e
}
add_diagnostic <- function(m,data,outcome,endpoint_id,hypothesis,variant,source_script,
                           time_var="et",treat_var="treated_firm",scope="main_related",
                           cluster_label="award_id + firm_id") {
 pre_names <- sprintf("%s::%s:%s",time_var,c(-5,-4,-3,-2),treat_var)
 available_pre <- intersect(pre_names,names(coef(m)))
 missing_pre <- setdiff(pre_names,available_pre)
 collinear_pre <- intersect(missing_pre,m$collin.var)
 pretest_status <- "estimable"
 pretest_reason <- ""
 test <- list(stat=NA_real_,p=NA_real_,df1=NA_real_,df2=NA_real_)
 if(length(missing_pre)) {
  # A subset Wald test would change the declared four-lead hypothesis.
  # Keep that test unavailable and retain every model coefficient unchanged.
  pretest_status <- if(length(collinear_pre)==length(missing_pre))
    "not_estimable_collinear_leads" else "not_estimable_missing_leads"
  pretest_reason <- paste("Unavailable declared lead terms:",paste(missing_pre,collapse="; "))
 } else {
  escaped <- gsub("([][{}()+*^$|\\?.])","\\\1",pre_names)
  keep <- paste0("^(",paste(escaped,collapse="|"),")$")
  candidate_test <- tryCatch(suppressMessages(wald(m,keep=keep,print=FALSE)),
    error=function(e) structure(list(message=conditionMessage(e)),class="pretest_error"))
  if(inherits(candidate_test,"pretest_error")) {
   pretest_status <- "not_estimable_wald_error"
   pretest_reason <- candidate_test$message
  } else if(!is.list(candidate_test)||length(candidate_test$p)!=1L||
            length(candidate_test$stat)!=1L||!is.finite(candidate_test$p)||
            !is.finite(candidate_test$stat)) {
   pretest_status <- "not_estimable_wald_result"
   pretest_reason <- "Native Wald test did not return a finite statistic and p value."
  } else test <- candidate_test
 }
 form <- paste(deparse(formula(m)),collapse=" ")
 pre_rows[[length(pre_rows)+1L]] <<- data.table(endpoint_id,hypothesis,outcome,variant,
   component=treat_var,source_script,scope,record_type="expanded_hypothesis_pretest",
   n_pre=length(pre_names),n_pre_available=length(available_pre),
   n_pre_missing=length(missing_pre),pretest_status,pretest_reason,
   missing_pre_terms=paste(missing_pre,collapse=";"),
   collinear_pre_terms=paste(collinear_pre,collapse=";"),
   stat=unname(test$stat),p_value=unname(test$p),
   df1=unname(test$df1),df2=unname(test$df2),n_obs=nobs(m),
   reference_period=-1,event_min=-5,event_max=5,formula=form,clusters=cluster_label)
 ct <- as.data.table(coeftable(m),keep.rownames="term")
 pattern <- sprintf("^%s::(-?[0-9]+):%s$",time_var,treat_var)
 ev <- ct[grepl(pattern,term)]
 ev[,event_time:=as.integer(sub(pattern,"\\1",term))]
 pc <- intersect(c("Pr(>|t|)","Pr(>|z|)"),names(ev))[1]
 ci <- confint(m,level=.95)
 out <- data.table(event_time=ev$event_time,term=ev$term,estimate=ev$Estimate,
   se=ev[["Std. Error"]],p_value=ev[[pc]],ci_lo=ci[ev$term,1],ci_hi=ci[ev$term,2],is_ref=FALSE,
   coefficient_status="estimated")
 unavailable_time <- setdiff(setdiff(-5:5,-1L),out$event_time)
 if(length(unavailable_time)) {
  missing_terms <- sprintf("%s::%s:%s",time_var,unavailable_time,treat_var)
  out <- rbind(out,data.table(event_time=unavailable_time,term=missing_terms,
    estimate=NA_real_,se=NA_real_,p_value=NA_real_,ci_lo=NA_real_,ci_hi=NA_real_,is_ref=FALSE,
    coefficient_status=ifelse(missing_terms %in% m$collin.var,"collinear","not_estimated_unsupported")))
 }
 out <- rbind(out,data.table(event_time=-1L,term=NA_character_,estimate=0,se=0,
   p_value=NA_real_,ci_lo=0,ci_hi=0,is_ref=TRUE,coefficient_status="reference_normalization"))
 used <- data[obs(m)]
 if("firm_id" %in% names(used)) {
  support <- used[,.(treated_obs=sum(get(treat_var)!=0),control_obs=sum(get(treat_var)==0),
    total_obs=.N,treated_firms=uniqueN(firm_id[get(treat_var)!=0]),
    control_firms=uniqueN(firm_id[get(treat_var)==0]),total_stacks=uniqueN(award_id)),
    by=.(event_time=get(time_var))]
 } else {
  support <- used[,.(treated_obs=sum(get(treat_var)!=0),control_obs=sum(get(treat_var)==0),
    total_obs=.N,treated_cities=uniqueN(city_id[get(treat_var)!=0]),
    control_cities=uniqueN(city_id[get(treat_var)==0]),total_cities=uniqueN(city_id)),
    by=.(event_time=get(time_var))]
 }
 out <- merge(out,support,by="event_time",all.x=TRUE,sort=TRUE)
 out[,`:=`(endpoint_id=endpoint_id,hypothesis=hypothesis,outcome=outcome,variant=variant,
   component=treat_var,source_script=source_script,scope=scope,n_obs=nobs(m),
   pretest_status=pretest_status)]
 event_rows[[length(event_rows)+1L]] <<- out
 cat(endpoint_id,"pre p",format(test$p,digits=12),"status",pretest_status,
   "available leads",length(available_pre),"of",length(pre_names),"N",nobs(m),"\n")
}

if(group %in% c("all","firm")) {
 fy <- fread(file.path(root_dir,"data/firm_level.csv"),encoding="UTF-8")
 sup <- fread(file.path(root_dir,"data/firm_year_supplement.csv"),encoding="UTF-8")
 fy <- merge(fy,sup,by=c("award_id","firm_id","year"))
 fy[,`:=`(stack_firm_fe=sprintf("%s__%s",award_id,firm_id),
          stack_year_fe=sprintf("%s__%s",award_id,year),
          et=fifelse(event_time < -5 | event_time > 5,NA_real_,as.numeric(event_time)))]
 fy <- fy[!is.na(et)]
 fy[,`:=`(log_civil_cases=log1p(civil_case_n),log_admin_plaintiff=log1p(admin_plaintiff_case_n),
          log_admin_gov=log1p(admin_gov_case_n),any_plaintiff_case=as.integer(admin_plaintiff_case_n>0))]
 for(o in c("log_civil_cases","civil_case_n","log_admin_plaintiff","any_plaintiff_case","log_admin_gov")) {
  d <- fy[!is.na(get(o))]
  f <- as.formula(sprintf("%s ~ i(et,treated_firm,ref=-1) | stack_firm_fe + stack_year_fe",o))
  m <- if(o=="civil_case_n") fepois(f,data=d,cluster=~award_id+firm_id) else
       feols(f,data=d,cluster=~award_id+firm_id)
  h <- if(o %in% c("log_civil_cases","civil_case_n")) "H4" else "H6"
  source <- if(h=="H4") "firm_caseload_expansion.R" else "firm_side_switching.R"
  variant <- if(o=="civil_case_n") "PPML_count" else "OLS"
  add_diagnostic(m,d,o,paste0("diag_",h,"_",o),h,variant,source)
 }
 rm(fy,sup,d,m);gc(FALSE)
}

if(group %in% c("all","document")) {
 # Practice-years standardization precedes the pure-private sample filter.
 dp <- fread(file.path(root_dir,"data/document_level_winner_vs_loser.csv"),encoding="UTF-8")
 dp[,`:=`(stack_year_fe=sprintf("%s__%s",award_id,year),cause_side_fe=sprintf("%s__%s",cause,side),
  lawyer_practice_years_obs=as.integer(!is.na(lawyer_practice_years)),
  year_gender_fe=sprintf("%s__%d",year,lawyer_gender),
  year_ccp_fe=sprintf("%s__%d",year,as.integer(lawyer_ccp==1L)),
  year_edu_fe=sprintf("%s__%d",year,as.integer(lawyer_edu>=4L)),
  et=fifelse(event_time < -5 | event_time > 5,NA_real_,as.numeric(event_time)))]
 practice_mean <- mean(dp$lawyer_practice_years,na.rm=TRUE)
 practice_sd <- sd(dp$lawyer_practice_years,na.rm=TRUE)
 dp[,lawyer_practice_years_std:=(lawyer_practice_years-practice_mean)/practice_sd]
 dp[is.na(lawyer_practice_years_std),lawyer_practice_years_std:=0]
 for(o in c("case_win_binary","case_win_rate_fee")) {
  d <- dp[plaintiff_party_is_entity==0L & defendant_party_is_entity==0L &
    case_decisive==1L & !is.na(get(o)) & !is.na(et)]
  f <- as.formula(sprintf(paste0("%s ~ i(et,treated_firm,ref=-1) + opponent_has_lawyer + ",
   "lawyer_practice_years_std + lawyer_practice_years_obs | firm_id + stack_year_fe + ",
   "cause_side_fe + court + year_gender_fe + year_ccp_fe + year_edu_fe"),o))
  m <- feols(f,data=d,cluster=~firm_id+award_id)
  add_diagnostic(m,d,o,paste0("diag_H5_pure_private_",o),"H5","pure_private",
    "document_mechanism_pure_private_placebo.R",cluster_label="firm_id + award_id")
 }
 rm(dp,d,m);gc(FALSE)
 de <- load_functions("document_level_ddd_fixest.R")
 dp <- de$read_document_panel(file.path(root_dir,"data/document_level_winner_vs_loser.csv"))
 dp[,et:=fifelse(event_time < -5 | event_time > 5,NA_real_,as.numeric(event_time))]
 for(o in c("case_win_binary","case_win_rate_fee")) {
  d <- dp[ddd_support_row==1L & case_decisive==1L & !is.na(get(o)) & !is.na(et)]
  # Baseline winner leads condition on no pre-award government exposure;
  # differential leads are retained for the exposed group in the same model.
  rhs <- paste("i(et,treated_firm,ref=-1)","prior_admin_gov_exposure","treated_by_prior",
     "post_by_prior","i(et,treated_by_prior,ref=-1)",de$control_rhs,sep=" + ")
  f <- as.formula(sprintf(paste0("%s ~ %s | firm_id + stack_year_fe + court_year_fe + ",
     "cause_side_fe + year_gender_fe + year_ccp_fe + year_edu_fe"),o,rhs))
  m <- feols(f,data=d,cluster=~firm_id+court_key)
  add_diagnostic(m,d,o,paste0("diag_H5_no_pre_gov_court_",o),"H5","DDD_baseline",
    "document_level_ddd_fixest.R",cluster_label="firm_id + court_key")
 }
 rm(dp,d,m);gc(FALSE)
}

if(group %in% c("all","overlap")) {
 doc <- fread(file.path(root_dir,"data/document_level_winner_vs_loser.csv"),encoding="UTF-8")
 doc <- doc[!is.na(court_match_key)]
 pre_doc <- doc[event_time<0L]
 winner_courts <- unique(pre_doc[treated_firm==1L,.(award_id,court_match_key)])
 winner_courts[,winner_court:=1L]
 pre_doc <- merge(pre_doc,winner_courts,by=c("award_id","court_match_key"),all.x=TRUE)
 pre_doc[,winner_court:=fifelse(is.na(winner_court),0L,winner_court)]
 exposure <- pre_doc[,.(control_in_winner_court=as.integer(first(treated_firm)==0L &&
   any(winner_court==1L))),by=.(award_id,firm_id)]
 doc <- merge(doc[award_id %in% unique(winner_courts$award_id)],exposure,
   by=c("award_id","firm_id"),all=FALSE)
 cell <- doc[,.(n_cases=.N,n_decisive=sum(case_decisive),n_win=sum(case_win_binary,na.rm=TRUE),
   treated_firm=max(treated_firm),control_in_winner_court=max(control_in_winner_court),
   event_year=first(event_year)),by=.(award_id,firm_id,year)]
 cell[,`:=`(civil_win_rate=fifelse(n_decisive>0,n_win/n_decisive,NA_real_),
   log_civil_case_n=log1p(n_cases),et=as.numeric(year-event_year),
   stack_firm_fe=sprintf("%s__%s",award_id,firm_id),stack_year_fe=sprintf("%s__%s",award_id,year))]
 for(o in c("civil_win_rate","log_civil_case_n")) {
  d <- cell[et>=-5 & et<=5 & !is.na(get(o))]
  if(o=="civil_win_rate") d <- d[n_decisive>0] else d <- d[n_cases>0]
  f <- as.formula(sprintf(paste0("%s ~ i(et,treated_firm,ref=-1) + ",
    "i(et,control_in_winner_court,ref=-1) | stack_firm_fe + stack_year_fe"),o))
  m <- feols(f,data=d,cluster=~award_id+firm_id)
  h <- if(o=="civil_win_rate") "H5" else "H4"
  add_diagnostic(m,d,o,paste0("diag_",h,"_overlap_winner_",o),h,"court_overlap_winner",
    "firm_sutva_same_court_spillover.R")
  add_diagnostic(m,d,o,paste0("diag_overlap_loser_",o),h,"court_overlap_loser",
    "firm_sutva_same_court_spillover.R",treat_var="control_in_winner_court",scope="interference_diagnostic")
 }
 rm(doc,pre_doc,cell,d,m);gc(FALSE)
}

if(group %in% c("all","court_fee")) {
 de <- load_functions("document_level_did_fixest.R")
 dp <- de$read_document_panel(file.path(root_dir,"data/document_level_winner_vs_loser.csv"))
 dp <- dp[case_decisive==1L & !is.na(case_win_rate_fee) & !is.na(event_time_window)]
 m <- de$estimate_event_model(dp,"case_win_rate_fee",
   sample_filter=quote(case_decisive==1L & !is.na(case_win_rate_fee)),fe_variant="court_year")
 add_diagnostic(m,dp,"case_win_rate_fee","diag_H5_court_year_fee","H5","court_year_fee",
   "document_level_did_fixest.R",time_var="event_time_window",cluster_label="firm_id + court")
}

if(group %in% c("all","admin")) {
 ae <- load_functions("admin_mechanism_selection_vs_quality.R")
 ae$input_file <- file.path(root_dir,"data/admin_case_level.csv")
 ad <- ae$read_admin()
 stopifnot(all(ad$withdraw_case+ad$end_case <= 1L))
 ad[,exit_without_merits:=withdraw_case+end_case]
 interactions <- list()
 for(o in c("withdraw_case","end_case","exit_without_merits")) {
  f <- as.formula(sprintf(paste0("%s ~ did_treatment * non_local_plaintiff + ",
    "government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity | court_id + year + cause_group"),o))
  m <- feols(f,data=ad,cluster=~city_id+court_id)
  ct <- as.data.table(coeftable(m),keep.rownames="term")
  interaction <- ct[term=="did_treatment:non_local_plaintiff"]
  baseline <- ct[term=="did_treatment"]
  interactions[[o]] <- data.table(outcome=o,term=interaction$term,
    estimate=interaction$Estimate,se=interaction[["Std. Error"]],p_value=interaction[["Pr(>|t|)"]],
    local_estimate=baseline$Estimate,local_se=baseline[["Std. Error"]],local_p_value=baseline[["Pr(>|t|)"]],
    n_obs=nobs(m),formula=paste(deparse(f),collapse=" "),clusters="city_id + court_id",
    hypothesis="H3",family="three_nonlocal_exit_modifiers",
    gate_scope=if(o=="exit_without_merits") "combined_exit_modifier" else "component_modifier_diagnostic")
 }
 nonlocal <- rbindlist(interactions)
 nonlocal[,p_holm:=p.adjust(p_value,method="holm",n=3L)]
 fwrite(nonlocal,file.path(table_dir,"h3_nonlocal_exit_interactions.csv"))
 # Never-treated cities remain in the comparison sample. Their zero exposure
 # gives no event coefficient, so they use the reference coordinate.
 ad[,et:=fifelse(treated_city==0L,-1,
   fifelse(event_time < -5 | event_time > 5,NA_real_,as.numeric(event_time)))]
 ad[,treated_city_nonlocal:=treated_city*non_local_plaintiff]
 d <- ad[!is.na(et)]
 m <- feols(exit_without_merits ~ i(et,treated_city,ref=-1) + non_local_plaintiff +
   i(et,treated_city_nonlocal,ref=-1) + government_has_lawyer + opponent_has_lawyer +
   plaintiff_is_entity | court_id + year + cause_group,data=d,cluster=~city_id+court_id)
 add_diagnostic(m,d,"exit_without_merits","diag_H3_nonlocal_combined_exit","H3","nonlocal_modifier",
   "admin_mechanism_selection_vs_quality.R",treat_var="treated_city_nonlocal",cluster_label="city_id + court_id")
 fmt <- function(x) sprintf("%.3f",x)
 fmt_p <- function(x) if(!is.finite(x)) "NA (not estimable)" else if(x<.001) "$<0.001$" else sprintf("%.3f",x)
 labels <- c(withdraw_case="Withdrawal",end_case="Procedural termination",exit_without_merits="Either exit before merits")
 body <- vapply(seq_len(nrow(nonlocal)),function(j) {
  r <- nonlocal[j]
  paste(labels[r$outcome],"&",fmt(r$estimate),"&",paste0("(",fmt(r$se),")"),"&",
    fmt_p(r$p_value),"&",fmt_p(r$p_holm),"\\\\")
 },character(1))
 pre_p <- pre_rows[[length(pre_rows)]]$p_value
 lines <- c("\\begin{table}[!htbp]","\\setlength{\\abovecaptionskip}{0pt}","\\centering",
  "\\caption{Additional Change in Case Exit among Nonlocal Plaintiffs}",
  "\\label{tab:h3_nonlocal_exit_interactions}","\\begin{threeparttable}","\\footnotesize",
  "\\begin{tabular}{lcccc}","\\toprule",
  "Outcome & Nonlocal DID difference & SE & Raw $p$ & Holm $p$ \\\\","\\midrule",body,
  "\\bottomrule","\\end{tabular}","\\begin{tablenotes}[flushleft]","\\footnotesize",
  paste0("\\item \\textit{Note:} Each row estimates Treatment $\\times$ Post $\\times$ Nonlocal plaintiff, with the treatment main effect and nonlocal indicator. Withdrawal and procedural termination are mutually exclusive; either exit is their sum. Component and combined differences are reported separately; a positive combined difference does not imply that both component differences are positive. Controls include government and opposing counsel and plaintiff entity status, with court, year and cause-group fixed effects; standard errors are clustered by city and court. Holm correction uses the three displayed interactions. The four pre-period nonlocal interaction leads for the combined exit have joint $p$ = ",
    fmt_p(pre_p),"; the event model also includes the baseline treatment leads. These are conditional comparisons and do not identify plaintiffs' motives."),
  "\\end{tablenotes}","\\end{threeparttable}","\\end{table}")
 writeLines(lines,file.path(table_dir,"h3_nonlocal_exit_interactions_table.tex"))
 print(nonlocal[,.(outcome,estimate,se,p_value,p_holm,gate_scope)])
}

suffix <- if(group=="all") "" else paste0("_",group)
fwrite(rbindlist(pre_rows,fill=TRUE),file.path(table_dir,paste0("hypothesis_diagnostics_pretests",suffix,".csv")))
fwrite(rbindlist(event_rows,fill=TRUE),file.path(table_dir,paste0("hypothesis_diagnostics_event_coefficients",suffix,".csv")))
