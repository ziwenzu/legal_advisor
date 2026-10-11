#!/usr/bin/env Rscript
# Exploratory effect differences across baseline characteristics and case context.
# All declared interaction tests remain in the output, including null results.

suppressPackageStartupMessages({library(data.table); library(fixest)})
script_arg <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
root <- if (length(script_arg)) normalizePath(file.path(dirname(sub("^--file=", "", script_arg[1L])), "..")) else normalizePath(getwd())
source(file.path(root, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root)
setFixest_nthreads(4L)

format_only <- "--format-only" %in% commandArgs(trailingOnly=TRUE)
if (!format_only) {
read_data <- function(name, columns=NULL) fread(file.path(dirs$data, paste0(name, ".csv")), select=columns, encoding="UTF-8")
standardize <- function(x) {
  s <- sd(x, na.rm=TRUE)
  if (!is.finite(s) || s <= 0) return(rep(NA_real_, length(x)))
  (x-mean(x, na.rm=TRUE))/s
}
definitions <- list()
add_definition <- function(modifier, label, timing, scale, center=NA_real_, spread=NA_real_, caveat="") {
  definitions[[length(definitions)+1L]] <<- data.table(modifier, label, timing, scale, center, spread, caveat)
}

# Freeze city context in the common first year, before any recorded adoption.
cy <- read_data("city_year_panel", c("province","city","year","treatment","log_gdp","log_population_10k","log_registered_lawyers"))
baseline_year <- min(cy$year)
city_base <- cy[year==baseline_year]
if (any(city_base$treatment!=0L)) stop("Common city baseline must precede adoption.")
city_base[, log_lawyer_density := log_registered_lawyers-log_population_10k]
for (item in list(c("log_gdp","pre_city_gdp","Baseline city GDP"),
                  c("log_population_10k","pre_city_population","Baseline city population"),
                  c("log_lawyer_density","pre_city_lawyer_density","Baseline city lawyer density"))) {
  value <- city_base[[item[1L]]]
  city_base[, (item[2L]) := standardize(value)]
  add_definition(item[2L], item[3L], paste("City year", baseline_year), "One baseline-city SD", mean(value,na.rm=TRUE), sd(value,na.rm=TRUE),
                 "Context measures follow the definitions and source limits in README.")
}
city_modifiers <- c("pre_city_gdp","pre_city_population","pre_city_lawyer_density")
city_base <- city_base[, c("province","city",city_modifiers), with=FALSE]

fy <- read_data("firm_level", c("award_id","firm_id","year","event_year","province","city","firm_size","civil_case_n"))
firm_base <- fy[year==event_year-1L]
if (anyDuplicated(firm_base[, .(award_id,firm_id)])) stop("Duplicate baseline firm keys.")
firm_base[, `:=`(log_size=log1p(firm_size), log_civil=log1p(civil_case_n))]
firm_base[, `:=`(pre_firm_size=standardize(log_size), pre_firm_caseload=standardize(log_civil))]
add_definition("pre_firm_size","Baseline firm size","Year immediately before award","One baseline-firm SD of log(1 + size)",mean(firm_base$log_size),sd(firm_base$log_size))
add_definition("pre_firm_caseload","Baseline firm civil volume","Year immediately before award","One baseline-firm SD of log(1 + civil cases)",mean(firm_base$log_civil),sd(firm_base$log_civil))
firm_base <- merge(firm_base, city_base, by=c("province","city"), all.x=TRUE, sort=FALSE)
firm_modifiers <- c("pre_firm_size","pre_firm_caseload",city_modifiers)
firm_base <- firm_base[, c("award_id","firm_id",firm_modifiers), with=FALSE]

lw <- read_data("lawyer_position_year",c("lawyer_id","firm_id","award_id","year","treated_firm","event_time","did_treatment",
  "lawyer_female","lawyer_ccp","lawyer_edu","practice_years","leadership_role","is_director","npc_deputy","cppcc_member",
  "npc_new_appointment","cppcc_new_appointment","npc_term_start_year","cppcc_term_start_year","npc_renewal","cppcc_renewal"))
lawyer_base <- lw[event_time==-1L,
  .(award_id,firm_id,lawyer_id,pre_female=as.integer(lawyer_female),pre_ccp=as.integer(lawyer_ccp),
    pre_master=as.integer(lawyer_edu>=4L),pre_practice_five=(practice_years-10)/5,
    pre_leader=as.integer(leadership_role),pre_director=as.integer(is_director),
    pre_representative=as.integer(npc_deputy==1L | cppcc_member==1L),pre_practice_years=practice_years)]
if (anyDuplicated(lawyer_base[, .(award_id,firm_id,lawyer_id)])) stop("Duplicate baseline lawyer keys.")
for (item in list(c("pre_female","Female lawyer"), c("pre_ccp","Party member"),
                  c("pre_master","Master's degree or above"),c("pre_leader","Firm leader"),
                  c("pre_director","Firm director"),c("pre_representative","Already held an NPC / CPPCC seat"))) {
  add_definition(item[1L],item[2L],"Year immediately before award","Indicator: 1 versus 0")
}
add_definition("pre_practice_five","Practice experience (+5 years)","Year immediately before award","Five years; centered at ten practice years",10,5)
lawyer_modifiers <- c("pre_practice_five","pre_female","pre_ccp","pre_master")

# Membership is the exact union of the two representative offices.
lw[, representative_seat := as.integer(npc_deputy==1L | cppcc_member==1L)]
stopifnot(!any(lw$npc_deputy==1L & is.na(lw$npc_term_start_year)),
          !any(lw$cppcc_member==1L & is.na(lw$cppcc_term_start_year)))
lw[, continuing_representative := (npc_deputy==1L & npc_term_start_year<year) |
                                  (cppcc_member==1L & cppcc_term_start_year<year)]
lw[, representative_new := as.integer((npc_new_appointment==1L | cppcc_new_appointment==1L) &
  !continuing_representative & npc_renewal==0L & cppcc_renewal==0L)]
# Verify annual flags against complete spells before selecting the baseline cohort.
terms <- read_data("lawyer_position_terms")
active <- terms[position_type %in% c("npc","cppcc") & appointment_year<2021 & actual_end_year>2010,
  .(year=seq.int(max(2010L,appointment_year),min(2020L,actual_end_year-1L))),
  by=.(lawyer_id,firm_id,position_id,position_type,appointment_year,actual_end_year,is_renewal)]
# Term firm_id records the appointment-time affiliation. A continuing seat
# follows the same person through a same-city transfer to another annual firm.
annual <- active[,.(expected_seat=1L,expected_new=as.integer(any(appointment_year==year & is_renewal==0L) &
  !any(appointment_year<year) & !any(appointment_year==year & is_renewal==1L))),by=.(lawyer_id,year)]
comparison <- merge(lw[,.(lawyer_id,firm_id,year,representative_seat,representative_new)],annual,
  by=c("lawyer_id","year"),all.x=TRUE,sort=FALSE)
# No active spell implies no formal seat or formal acquisition.
comparison[is.na(expected_seat),`:=`(expected_seat=0L,expected_new=0L)]
stopifnot(all(comparison$representative_seat==comparison$expected_seat),
          all(comparison$representative_new==comparison$expected_new))
rm(terms,active,annual,comparison)
lw <- merge(lw, lawyer_base, by=c("award_id","firm_id","lawyer_id"), all=FALSE, sort=FALSE)
lw <- merge(lw, firm_base, by=c("award_id","firm_id"), all.x=TRUE, sort=FALSE)
# Keep eligibility fixed to pre-award leaders. New entrants do not enter the cohort.
lw <- lw[pre_leader==1L & pre_practice_years>=5 & event_time>=-5 & event_time<=5]
lw[, `:=`(lawyer_stack_fe=paste(award_id,lawyer_id,sep="__"), stack_year_fe=paste(award_id,year,sep="__"))]
political_modifiers <- c(lawyer_modifiers,"pre_director","pre_representative",firm_modifiers)

doc <- read_data("document_level_winner_vs_loser",c("award_id","firm_id","lawyer_id","year","event_time","did_treatment","treated_firm",
  "case_decisive","case_win_binary","legal_reasoning_share","opponent_has_lawyer","plaintiff_party_is_entity","defendant_party_is_entity","court","cause","side"))
doc <- merge(doc, lawyer_base, by=c("award_id","firm_id","lawyer_id"), all=FALSE, sort=FALSE)
doc <- merge(doc, firm_base, by=c("award_id","firm_id"), all.x=TRUE, sort=FALSE)
doc <- doc[event_time>=-5 & event_time<=5]
doc[, `:=`(stack_year_fe=paste(award_id,year,sep="__"), cause_side_fe=paste(cause,side,sep="__"),
           baseline_gender_year=paste(year,pre_female,sep="__"),baseline_ccp_year=paste(year,pre_ccp,sep="__"),
           baseline_edu_year=paste(year,pre_master,sep="__"))]
civil_modifiers <- c(lawyer_modifiers,"pre_leader",firm_modifiers)

adm <- read_data("admin_case_level",c("province","city","year","court_std","cause_group","did_treatment","treated_city","event_time",
  "non_basic_court","plaintiff_is_entity","non_local_plaintiff","opponent_has_lawyer","government_win","petitioned"))
adm <- merge(adm,city_base,by=c("province","city"),all.x=TRUE,sort=FALSE)
adm <- adm[!is.na(court_std) & nzchar(court_std)]
adm[, `:=`(city_id=paste(province,city,sep="__"),court_id=court_std,
           case_elevated_court=as.integer(non_basic_court),case_entity_plaintiff=as.integer(plaintiff_is_entity),
           case_nonlocal_plaintiff=as.integer(non_local_plaintiff),case_opponent_lawyer=as.integer(opponent_has_lawyer))]
for (item in list(c("case_elevated_court","Non-basic court"),c("case_entity_plaintiff","Entity plaintiff"),
                  c("case_nonlocal_plaintiff","Non-local plaintiff"),c("case_opponent_lawyer","Plaintiff has a lawyer"))) {
  add_definition(item[1L],item[2L],"Observed case context","Indicator: 1 versus 0",caveat=
    "Case assignment and participant composition may change after treatment. This is a conditional descriptive contrast.")
}
admin_modifiers <- c(city_modifiers,"case_elevated_court","case_entity_plaintiff","case_nonlocal_plaintiff","case_opponent_lawyer")

specs <- list(
  list(id="civil_win",label="Civil case success",data=doc[case_decisive==1L],outcome="case_win_binary",modifiers=civil_modifiers,
       fe="firm_id + stack_year_fe + cause_side_fe + court + baseline_gender_year + baseline_ccp_year + baseline_edu_year",
       controls="opponent_has_lawyer + plaintiff_party_is_entity + defendant_party_is_entity",cluster=~firm_id+award_id,
       population="Cases assigned to lawyers present in the firm in the year before award; decisive cases only",qualification="Conditional on observed case composition and assignment within the pre-award lawyer roster"),
  list(id="civil_reasoning",label="Legal reasoning share",data=doc,outcome="legal_reasoning_share",modifiers=civil_modifiers,
       fe="firm_id + stack_year_fe + cause_side_fe + court + baseline_gender_year + baseline_ccp_year + baseline_edu_year",
       controls="opponent_has_lawyer + plaintiff_party_is_entity + defendant_party_is_entity",cluster=~firm_id+award_id,
       population="Cases assigned to lawyers present in the firm in the year before award",qualification="Conditional on observed case composition and assignment within the pre-award lawyer roster"),
  list(id="representative_stock",label="Leader holds an NPC / CPPCC seat",data=lw,outcome="representative_seat",modifiers=political_modifiers,
       fe="lawyer_stack_fe + stack_year_fe",controls=character(0),cluster=~firm_id+award_id,
       population="Fixed pre-award leaders with at least five practice years",qualification="NPC or CPPCC office; bar offices excluded; available annual employment records, without a future-employment eligibility requirement"),
  list(id="representative_acquisition",label="Leader acquires an NPC / CPPCC seat",data=lw,outcome="representative_new",modifiers=political_modifiers,
       fe="lawyer_stack_fe + stack_year_fe",controls=character(0),cluster=~firm_id+award_id,
       population="Fixed pre-award leaders with at least five practice years",qualification="Unconditional annual entry into NPC or CPPCC representation: no continuing office of either type and no renewal in that year. Noneligible leader-years remain with outcome zero. Later re-entry after an expired term counts. No selection on later risk-set or future-employment status"),
  list(id="government_win",label="Government wins an administrative case",data=adm,outcome="government_win",modifiers=admin_modifiers,
       fe="court_id + year + cause_group",controls="plaintiff_is_entity",cluster=~city_id+court_id,
       population="Administrative case records",qualification="Current-case modifiers describe case composition rather than fixed baseline persons"),
  list(id="petition",label="Administrative plaintiff petitions",data=adm,outcome="petitioned",modifiers=admin_modifiers,
       fe="court_id + year + cause_group",controls="plaintiff_is_entity",cluster=~city_id+court_id,
       population="Administrative case records",qualification="Current-case modifiers describe case composition rather than fixed baseline persons")
)

definition_dt <- rbindlist(definitions)
interactions <- list(); contrasts <- list(); support <- list()
declared_n <- sum(vapply(specs,function(x)length(x$modifiers),integer(1)))

linear_contrast <- function(model, weights) {
  b <- coef(model); v <- vcov(model)
  weights <- weights[names(weights)%in%names(b)]
  estimate <- sum(b[names(weights)]*weights)
  variance <- as.numeric(t(weights)%*%v[names(weights),names(weights),drop=FALSE]%*%weights)
  se <- if(is.finite(variance) && variance>=0) sqrt(variance) else NA_real_
  df <- fixest::degrees_freedom(model,type="t")
  critical <- qt(.975,df=df)
  list(estimate=estimate,se=se,ci_low=estimate-critical*se,ci_high=estimate+critical*se,
       p_value=2*pt(-abs(estimate/se),df=df),df=df)
}

for (s in specs) {
  for (modifier in s$modifiers) {
    cat("Estimating",s$id,"by",modifier,"\n")
    modifier_index <- match(modifier,definition_dt$modifier)
    modifier_definition <- definition_dt[modifier_index]
    d <- s$data[!is.na(get(modifier)) & !is.na(get(s$outcome))]
    d[, effect_modifier := get(modifier)]
    # Calendar-year slopes allow different common trajectories by modifier.
    rhs <- paste(c("did_treatment * effect_modifier",s$controls),collapse=" + ")
    fml <- as.formula(sprintf("%s ~ %s | %s + year[effect_modifier]",s$outcome,rhs,s$fe))
    base <- data.table(model_id=s$id,outcome=s$outcome,outcome_label=s$label,modifier=modifier,
      modifier_label=modifier_definition$label,population=s$population,qualification=s$qualification,
      formula=paste(deparse(fml),collapse=" "),cluster=if(s$id %in% c("government_win","petition")) "city and court" else "firm and stack",
      n_input=nrow(d),n_obs=NA_integer_,estimate=NA_real_,se=NA_real_,ci_low=NA_real_,ci_high=NA_real_,p_value=NA_real_,df=NA_real_,status="not estimable")
    support[[length(support)+1L]] <- d[,.(observations=.N,units=uniqueN(if("lawyer_id"%in%names(d)) lawyer_id else court_id),
      modifier_min=min(effect_modifier),modifier_max=max(effect_modifier),modifier_mean=mean(effect_modifier)),
      by=.(treated=if("treated_firm"%in%names(d)) treated_firm else treated_city,
           adoption_period=if("treated_firm"%in%names(d)) as.integer(event_time>=0) else fifelse(treated_city==1L,as.integer(event_time>=0),NA_integer_))][,
        `:=`(model_id=s$id,modifier=modifier,period_definition=if(s$id %in% c("government_win","petition"))
          "Relative to own adoption for treated cities; NA for never-adopters" else "Relative to the common assigned award within each firm stack")]
    fitted <- tryCatch(feols(fml,data=d,cluster=s$cluster,notes=FALSE),error=function(e)e)
    if (inherits(fitted,"error")) {
      base[,status:=conditionMessage(fitted)]
    } else {
      term <- intersect(c("did_treatment:effect_modifier","effect_modifier:did_treatment"),names(coef(fitted)))
      if(length(term)==1L && "did_treatment"%in%names(coef(fitted))) {
        x <- linear_contrast(fitted,setNames(1,term))
        for (nm in names(x)) set(base,j=nm,value=x[[nm]])
        base[,`:=`(n_obs=nobs(fitted),status="estimated")]
        for (z in c(0,1)) {
          effect <- linear_contrast(fitted,setNames(c(1,z),c("did_treatment",term)))
          contrasts[[length(contrasts)+1L]] <- as.data.table(c(list(model_id=s$id,outcome=s$outcome,modifier=modifier,
            modifier_value=z,scale=modifier_definition$scale),effect))
        }
      }
    }
    interactions[[length(interactions)+1L]] <- base
    rm(fitted,d); invisible(gc(FALSE))
  }
}
res <- rbindlist(interactions,fill=TRUE)
stopifnot(nrow(res)==declared_n,!anyDuplicated(res[,.(model_id,modifier)]))
# The full declared family is retained even if some terms lack support.
res[, p_bh := p.adjust(p_value,method="BH",n=declared_n)]
res[, `:=`(estimate_pp=100*estimate,ci_low_pp=100*ci_low,ci_high_pp=100*ci_high,
           declared_family_size=declared_n,interaction_discovery=as.logical(p_bh<.05))]
fwrite(res,file.path(dirs$tables,"exploratory_heterogeneity_interactions.csv"))
fwrite(rbindlist(contrasts,fill=TRUE),file.path(dirs$tables,"exploratory_heterogeneity_effects_at_modifier_values.csv"))
fwrite(rbindlist(support,fill=TRUE),file.path(dirs$tables,"exploratory_heterogeneity_support.csv"))
fwrite(definition_dt,file.path(dirs$tables,"exploratory_heterogeneity_baseline_definitions.csv"))
} else {
  # Rebuild exhibits from the saved estimates without changing their values.
  res <- fread(file.path(dirs$tables,"exploratory_heterogeneity_interactions.csv"))
  declared_n <- unique(res$declared_family_size)
  if (length(declared_n)!=1L) stop("Inconsistent declared interaction family.")
  specs <- lapply(unique(res$model_id),function(id) {
    r <- res[model_id==id][1L]
    list(id=id,label=r$outcome_label,population=r$population,qualification=r$qualification)
  })
}

tex <- c("% Exploratory interaction estimates; the complete declared family is reported.")
for (s in specs) {
  panel <- res[model_id==s$id]
  body <- vapply(seq_len(nrow(panel)),function(i) {
    r <- panel[i]
    paste(r$modifier_label,"&",fmt_num(r$estimate_pp,2),"&",
      sprintf("[%s, %s]",fmt_num(r$ci_low_pp,2),fmt_num(r$ci_high_pp,2)),"&",fmt_p(r$p_value),"&",fmt_p(r$p_bh),"&",fmt_int(r$n_obs),"\\\\")
  },character(1))
  tex <- c(tex,"\\begin{table}[p]","\\centering",paste0("\\caption{Exploratory Effect Differences: ",s$label,"}"),
    paste0("\\label{tab:heterogeneity_",s$id,"}"),"\\begin{threeparttable}","\\scriptsize","\\begin{tabular}{lrrrrr}","\\toprule",
    "Modifier & Difference (pp) & 95\\% CI & Interaction $p$ & BH $q$ & Observations \\\\","\\midrule",body,"\\bottomrule","\\end{tabular}",
    "\\begin{tablenotes}[flushleft]","\\scriptsize",paste0("\\item \\textit{Note:} ",s$population,". ",s$qualification,". "),
    paste0("Differences are treatment-by-modifier interactions, not comparisons of separate significance levels. Standard errors are clustered by ",panel$cluster[1L],
      ". Intervals use the model's cluster-adjusted Student $t$ degrees of freedom. BH adjustment covers all ",declared_n," declared interactions across the six outcomes. "),
    "Each row fits one modifier at a time. Binary modifiers compare 1 with 0; experience compares five additional pre-award practice years; firm and city continuous modifiers compare one baseline standard deviation. Year-specific modifier slopes are included. Findings are exploratory and do not establish independent mechanisms.",
    "\\end{tablenotes}","\\end{threeparttable}","\\end{table}")
}
writeLines(tex,file.path(dirs$tables,"exploratory_heterogeneity_interactions.tex"))

format_q_tex <- function(x) {
  if(!is.finite(x)) return("unavailable")
  if(x==0) return("$<0.001$")
  if(x>=.001) return(sprintf("%.3f",x))
  exponent <- floor(log10(x))
  sprintf("$%.2f\\times10^{%d}$",x/10^exponent,exponent)
}
estimated_n <- sum(res$status=="estimated" & is.finite(res$p_value))
discovery_n <- sum(res$p_bh<.05,na.rm=TRUE)
valid_q <- res$p_bh[is.finite(res$p_bh)]
q_range <- if(length(valid_q)) paste(format_q_tex(min(valid_q)),"to",format_q_tex(max(valid_q))) else "unavailable"
positive_n <- sum(res$estimate>0,na.rm=TRUE)
negative_n <- sum(res$estimate<0,na.rm=TRUE)
direction_sentence <- if(positive_n>0L && negative_n>0L)
  sprintf("The estimated differences have mixed directions (%d positive and %d negative).",positive_n,negative_n) else
  sprintf("There are %d positive and %d negative estimated differences.",positive_n,negative_n)
summary_tex <- paste0(
  "We examine ",declared_n," declared treatment-by-characteristic interactions across six outcome blocks; ",estimated_n," are estimable. ",
  discovery_n," survive a Benjamini--Hochberg adjustment at $q<0.05$ across the complete family (adjusted $q$ range: ",q_range,"). ",
  direction_sentence," Figure~\\ref{fig:exploratory_heterogeneity} plots the interaction differences and their 95\\% clustered intervals; ",
  "Tables~\\ref{tab:heterogeneity_civil_win}--\\ref{tab:heterogeneity_petition} report the complete results. ",
  "Binary modifiers compare one with zero, experience compares five additional pre-award practice years, and continuous firm and city context variables compare one baseline standard deviation. ",
  "Lawyer and firm characteristics are frozen in the year before the award, city context is frozen before adoption, and the representation analysis follows the fixed pre-award leadership cohort. ",
  "Each specification includes one modifier and its calendar-year slopes. Current court and plaintiff characteristics are conditional case-context comparisons that may also reflect changes in case assignment or participant composition. ",
  "The interaction tests assess differences in effects rather than differences in separate significance levels; exploratory patterns can qualify the scope of the proposed mechanisms without establishing independent causal channels."
)
writeLines(summary_tex,file.path(dirs$tables,"exploratory_heterogeneity_summary.tex"))

pdf(file.path(dirs$figures,"exploratory_heterogeneity_forest.pdf"),width=12.8,height=9.6,family="Helvetica")
par(mfrow=c(3,1),mar=c(3.1,14.0,2.1,4.7),oma=c(1.7,0,1.5,0),mgp=c(2,.65,0),las=1,bty="l",tcl=-.2)
for (i in seq_along(specs)) {
  s <- specs[[i]]; p <- res[model_id==s$id]
  y <- rev(seq_len(nrow(p)))
  xr <- range(c(p$ci_low_pp,p$ci_high_pp,0),na.rm=TRUE)
  if(!all(is.finite(xr))) xr <- c(-1,1)
  width <- max(diff(xr),.2); xr <- xr+c(-.06,.06)*width
  plot(p$estimate_pp,y,type="n",xlim=xr,ylim=c(.4,nrow(p)+.6),yaxt="n",xlab="Difference in treatment effect (percentage points)",ylab="",cex.axis=.82,cex.lab=.84)
  abline(v=0,lty=2,col="grey65")
  axis(2,at=y,labels=p$modifier_label,las=1,tick=FALSE,cex.axis=.83)
  color <- ifelse(!is.na(p$p_bh)&p$p_bh<.05,"#202020","grey35")
  segments(p$ci_low_pp,y,p$ci_high_pp,y,col=color,lwd=1.45)
  points(p$estimate_pp,y,pch=ifelse(!is.na(p$p_bh)&p$p_bh<.05,19,1),cex=.77,col=color)
  title(main=paste0(LETTERS[i],". ",s$label),adj=0,cex.main=1.01)
  q_label <- ifelse(is.na(p$p_bh),"NA",ifelse(p$p_bh<.001,"<.001",sprintf("%.3f",p$p_bh)))
  text(xr[2]+.065*width,y,labels=paste0("q ",q_label),xpd=NA,adj=0,cex=.77)
  if(i %in% c(3L,6L)) {
    mtext(paste0("One modifier per model; 95% clustered intervals; BH across all ",declared_n," interactions. Filled points mark q < 0.05."),side=1,outer=TRUE,line=.1,cex=.8)
    mtext(if(i==3L) "Pre-award characteristics and baseline city context" else "Representative acquisition and administrative-case context",side=3,outer=TRUE,line=0,cex=.94)
  }
}
dev.off()
cat("Completed",nrow(res),"declared interactions:",sum(res$status=="estimated"),"estimated;",sum(res$p_bh<.05,na.rm=TRUE),"BH discoveries.\n")
