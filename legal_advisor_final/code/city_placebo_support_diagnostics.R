#!/usr/bin/env Rscript
# Additional random-assignment diagnostics with the original sample and controls.
suppressPackageStartupMessages({library(data.table);library(fixest);library(did)})
local({
  for (loc in c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG=loc, LC_ALL=loc, LC_CTYPE=loc, LC_COLLATE=loc)
      break
    }
  }
})
arg <- grep("^--file=",commandArgs(trailingOnly=FALSE),value=TRUE)
root_dir <- if(length(arg)) normalizePath(file.path(dirname(normalizePath(sub("^--file=","",arg[1]))),"..")) else normalizePath(getwd())
table_dir <- file.path(root_dir,"output/tables")
dir.create(table_dir,recursive=TRUE,showWarnings=FALSE)
setFixest_nthreads(4)

# Load function definitions only; keep the original analysis entry points intact.
load_functions <- function(name) {
  e <- new.env(parent=globalenv())
  for(expr in parse(file.path(root_dir,"code",name))) {
    if(is.call(expr) && identical(expr[[1]],as.name("<-")) && length(expr)==3L &&
       is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name("function"))) eval(expr,e)
  }
  e
}
original <- load_functions("admin_random_fake_treatment_placebo.R")
original$PLACEBO_SEED <- 2026L
original$city_path <- file.path(root_dir,"data/city_year_panel.csv")
# Retain the native object returned by the unchanged original estimator call.
original$att_gt <- function(...) {
  value <- did::att_gt(...)
  original$last_native_mp <- value
  value
}
city_functions <- load_functions("city_year_cs_twfe_figures_tables.R")
panel <- original$read_panel()
assigned <- original$assign_fake_treatment(copy(panel))[ever_treated==0L]
stopifnot(all(assigned$treatment==0L),uniqueN(assigned$city_id)==65L,
          uniqueN(assigned[fake_treated_indicator==1L,city_id])==46L,
          uniqueN(assigned[fake_treated_indicator==0L,city_id])==19L)
assignment <- unique(assigned[,.(city_id,province,city,fake_treated_indicator,
                                original_fake_year=fake_first_treat_year)])
assignment[,uniform2017_fake_year:=fifelse(fake_treated_indicator==1L,2017,0)]
fwrite(assignment,file.path(table_dir,"city_placebo_support_assignment.csv"))
common_date <- copy(assigned)
common_date[fake_treated_indicator==1L,`:=`(fake_first_treat_year=2017,
                                         fake_event_year=2017,
                                         fake_treatment=as.integer(year>=2017))]

pre_rows <- list(); mean_rows <- list(); original_rows <- list()
cov_rows <- list(); if_rows <- list(); cell_rows <- list(); support_rows <- list(); event_rows <- list()
export_covariance <- function(V,keys,endpoint_id,scope) {
  ind <- expand.grid(i=seq_len(nrow(V)),j=seq_len(ncol(V)))
  data.table(endpoint_id=endpoint_id,scope=scope,
             contrast_i=keys[ind$i],contrast_j=keys[ind$j],
             covariance=V[cbind(ind$i,ind$j)])
}
export_if <- function(IF,keys,ids,endpoint_id,scope) {
  z <- as.data.table(as.matrix(IF));setnames(z,keys)
  stopifnot(nrow(z)==length(ids))
  z[,city_id:=ids]
  z <- melt(z,id.vars="city_id",variable.name="contrast",value.name="influence_function",variable.factor=FALSE)
  z[,`:=`(endpoint_id=endpoint_id,scope=scope)]
  z[]
}
run_design <- function(data,design,outcome) {
  endpoint_id <- paste0("SUPP_",design,"_",outcome)
  # The original function resets seed 2026, uses reg/varying, city clustering,
  # and 1000 bootstrap draws. Additional exports do not alter that call.
  mean_result <- original$estimate_cs_placebo(data,outcome)
  mp <- original$last_native_mp
  units <- unique(data[,.(city_id,group=fake_first_treat_year)])
  cohort <- units[,.(cohort_cities=.N),by=group][order(group)]
  cohort[,`:=`(endpoint_id=endpoint_id,design=design,outcome=outcome,
               comparison_cities=units[group==0,.N],first_calendar_year=min(data$year),
               last_calendar_year=max(data$year))]
  support_rows[[length(support_rows)+1L]] <<- cohort
  ids <- unique(mp$DIDparams$data[[mp$DIDparams$idname]])
  ix <- which(mp$group>mp$t & !is.na(mp$se))
  keys <- paste0("g",mp$group[ix],"_t",mp$t[ix])
  V <- as.matrix(mp$V_analytical[ix,ix,drop=FALSE])/mp$n
  beta <- mp$att[ix]
  W <- as.numeric(mp$W)
  p_exact <- pchisq(W,length(ix),lower.tail=FALSE)
  cells <- data.table(endpoint_id=endpoint_id,design=design,outcome=outcome,
    contrast=keys,group=mp$group[ix],calendar_year=mp$t[ix],
    base_calendar_year=mp$t[ix]-1,event_time=mp$t[ix]-mp$group[ix],
    estimate=beta,se=mp$se[ix],analytical_se=sqrt(diag(V)),
    p_value=2*pnorm(-abs(beta/mp$se[ix])),scope="cohort_time")
  cells <- merge(cells,cohort[,.(group,cohort_cities,comparison_cities)],by="group",all.x=TRUE,sort=FALSE)
  cell_rows[[length(cell_rows)+1L]] <<- cells
  cov_rows[[length(cov_rows)+1L]] <<- export_covariance(V,keys,endpoint_id,"cohort_time")
  if_rows[[length(if_rows)+1L]] <<- export_if(mp$inffunc[,ix,drop=FALSE],keys,ids,endpoint_id,"cohort_time")
  eigenvalues <- eigen((V+t(V))/2,symmetric=TRUE,only.values=TRUE)$values
  global <- data.table(endpoint_id=endpoint_id,design=design,outcome=outcome,
    statistic=W,df=length(ix),p_value=p_exact,native_rounded_p=as.numeric(mp$Wpval),
    n_obs=nrow(data),n_cities=mp$n,n_fake_treated=units[group>0,.N],n_fake_controls=units[group==0,.N],
    covariance_rank=qr(V,tol=1e-10)$rank,minimum_covariance_eigenvalue=min(eigenvalues),
    pre_periods=paste(keys,collapse=","),reference_period=NA_integer_,
    inference="Original CS cohort-time omnibus; native analytical covariance and chi-square reference")
  if(design=="random_CS_pooled") {
    # This is a mean-event diagnostic, distinct from the retained full omnibus.
    original_rows[[length(original_rows)+1L]] <<- copy(global)
    dynamic <- did::aggte(mp,type="dynamic")
    w <- city_functions$extract_cs_pretest(list(agg_dynamic=dynamic))
    dpi <- which(dynamic$egt<0)
    dk <- paste0("e",dynamic$egt[dpi])
    dIF <- dynamic$inf.function$dynamic.inf.func.e[,dpi,drop=FALSE]
    dV <- crossprod(dIF)/(nrow(dIF)^2)
    dV_used <- dV;diag(dV_used)<-pmax(diag(dV_used),1e-10)
    pooled <- copy(global)
    pooled[,`:=`(statistic=w$stat,df=w$df,p_value=w$p_value,native_rounded_p=NA_real_,
      covariance_rank=qr(dV_used,tol=1e-10)$rank,
      minimum_covariance_eigenvalue=min(eigen((dV_used+t(dV_used))/2,symmetric=TRUE,only.values=TRUE)$values),
      pre_periods=paste(dynamic$egt[dpi],collapse=","),
      inference="All negative CS dynamic averages; original city joint-test IF covariance and diagonal guard")]
    pre_rows[[length(pre_rows)+1L]] <<- pooled
    dc <- data.table(endpoint_id=endpoint_id,design=design,outcome=outcome,contrast=dk,
      event_time=dynamic$egt[dpi],estimate=dynamic$att.egt[dpi],se=dynamic$se.egt[dpi],
      analytical_se=sqrt(diag(dV)),p_value=2*pnorm(-abs(dynamic$att.egt[dpi]/dynamic$se.egt[dpi])),
      scope="pooled_dynamic")
    cell_rows[[length(cell_rows)+1L]] <<- dc
    cov_rows[[length(cov_rows)+1L]] <<- export_covariance(dV,dk,endpoint_id,"pooled_dynamic_raw")
    cov_rows[[length(cov_rows)+1L]] <<- export_covariance(dV_used,dk,endpoint_id,"pooled_dynamic_joint_test")
    if_rows[[length(if_rows)+1L]] <<- export_if(dIF,dk,ids,endpoint_id,"pooled_dynamic")
    events <- data.table(endpoint_id=endpoint_id,design=design,outcome=outcome,event_time=dynamic$egt,
      estimate=dynamic$att.egt,se=dynamic$se.egt,p_value=2*pnorm(-abs(dynamic$att.egt/dynamic$se.egt)),
      ci_lo=dynamic$att.egt-1.96*dynamic$se.egt,ci_hi=dynamic$att.egt+1.96*dynamic$se.egt)
    event_rows[[length(event_rows)+1L]] <<- events
  } else {
    pre_rows[[length(pre_rows)+1L]] <<- global
  }
  mean_rows[[length(mean_rows)+1L]] <<- data.table(endpoint_id=endpoint_id,design=design,outcome=outcome,
    estimate=mean_result$estimate,se=mean_result$se,p_value=mean_result$p_value,n_obs=mean_result$n_obs,
    n_cities=mp$n,n_fake_treated=units[group>0,.N],n_fake_controls=units[group==0,.N],
    inference="Original CS simple aggregate; native bootstrap standard error and normal two-sided p")
  cat(endpoint_id,"mean",mean_result$estimate,"SE",mean_result$se,"p",mean_result$p_value,"\n")
}
for(outcome in c("government_win_rate","appeal_rate")) run_design(copy(assigned),"random_CS_pooled",outcome)
for(outcome in c("government_win_rate","appeal_rate")) run_design(copy(common_date),"uniform2017_CS",outcome)
fwrite(rbindlist(pre_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_pretests.csv"))
fwrite(rbindlist(mean_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_means.csv"))
fwrite(rbindlist(original_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_original_global_pretests.csv"))
fwrite(rbindlist(cov_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_pre_covariance.csv"))
fwrite(rbindlist(if_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_pre_IF.csv"))
fwrite(rbindlist(cell_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_pre_cells.csv"))
fwrite(rbindlist(support_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_cohort_support.csv"))
fwrite(rbindlist(event_rows,fill=TRUE),file.path(table_dir,"city_placebo_support_dynamic_coefficients.csv"))

write_support_table <- function(pre, means, original_global, cohort_support, path) {
  stopifnot(nrow(pre)==4L,nrow(means)==4L,nrow(original_global)==2L,
            !anyDuplicated(pre$endpoint_id),!anyDuplicated(means$endpoint_id))
  fmt <- function(x,digits=3L) if(is.finite(x)) formatC(x,digits=digits,format="g") else "NA"
  p_fmt <- function(x) if(is.finite(x) && x==0) "$<10^{-16}$" else fmt(x)
  outcome_label <- function(x) if(x=="government_win_rate") "Government win" else "Appeal"
  min_cohort <- function(id) cohort_support[endpoint_id==id & group>0,min(cohort_cities)]
  row_text <- function(values) paste0(paste(values,collapse=" & ")," \\\\")
  lines <- c("\\begin{table}[!htbp]","\\centering",
    "\\caption{Additional Assignment Diagnostics and Retained Original Tests}",
    "\\label{tab:city_placebo_support_diagnostics}",
    "\\begin{threeparttable}","\\footnotesize",
    "\\begin{tabular}{llrrrr}","\\toprule",
    "\\multicolumn{6}{l}{Panel A: Retained random-date cohort-time omnibus} \\\\",
    row_text(c("Design","Outcome","Statistic","df / rank","Pre-test $p$","Min. cohort")),
    "\\midrule")
  for(i in seq_len(nrow(original_global))) {
    r <- original_global[i]
    lines <- c(lines,row_text(c("Random dates",outcome_label(r$outcome),fmt(r$statistic),
      paste(r$df,r$covariance_rank,sep=" / "),p_fmt(r$p_value),fmt(min_cohort(r$endpoint_id),0L))))
  }
  lines <- c(lines,"\\midrule",
    "\\multicolumn{6}{l}{Panel B: Four additional comparisons} \\\\",
    row_text(c("Design","Outcome","Mean (SE)","Mean $p$","Pre-test $p$","df / rank")),
    "\\midrule")
  for(i in seq_len(nrow(pre))) {
    r <- pre[i]; m <- means[endpoint_id==r$endpoint_id]
    stopifnot(nrow(m)==1L)
    label <- if(r$design=="random_CS_pooled") "Random pooled" else "Common 2017"
    lines <- c(lines,row_text(c(label,outcome_label(r$outcome),
      paste0(fmt(m$estimate)," (",fmt(m$se),")"),p_fmt(m$p_value),p_fmt(r$p_value),
      paste(r$df,r$covariance_rank,sep=" / "))))
  }
  lines <- c(lines,"\\bottomrule","\\end{tabular}",
    "\\begin{tablenotes}[flushleft]","\\footnotesize",
    "\\item \\textit{Note:} All designs use the original 65 never-award cities, the same 46 assigned and 19 comparison cities, controls, city clusters and native estimator calls. Panel A retains the original random-date cohort-time omnibus, including its singleton-cohort support limitation. Panel B's random-date pre-test concerns all negative pooled event averages; its common-2017 pre-test concerns all negative cohort-time cells under the alternative date. The latter has 46 cities in one assigned cohort and changes the assignment design.",
    "\\item Mean estimates in Panel B are the four native CS simple aggregates with their original bootstrap standard errors and normal two-sided $p$-values. Panel A uses the same two random-date means and adds no mean tests. Pre-test statistics and covariance ranks retain the native analytical covariance or the original pooled influence-function covariance and diagonal guard. A nonrejecting pooled or common-date test does not repair or replace a rejected original omnibus. Every exported negative cell remains in the associated CSVs.",
    "\\end{tablenotes}","\\end{threeparttable}","\\end{table}")
  writeLines(lines,path,useBytes=TRUE)
}
write_support_table(rbindlist(pre_rows,fill=TRUE),rbindlist(mean_rows,fill=TRUE),
  rbindlist(original_rows,fill=TRUE),rbindlist(support_rows,fill=TRUE),
  file.path(table_dir,"city_placebo_support_diagnostics_table.tex"))
cat("Exported four supplementary pretests and four native mean estimates.\n")
