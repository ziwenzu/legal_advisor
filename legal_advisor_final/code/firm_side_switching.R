#!/usr/bin/env Rscript
# firm_side_switching.R  (H6: alignment)
#
# Does the contracted firm stop suing the state? Firm-year stacked DID on the
# firm's administrative-litigation representation: cases in which the firm
# represented a plaintiff against a government agency, cases in which it
# represented an agency, and the plaintiff-side share of its administrative
# work. Event study for the plaintiff-side share.
#
# Outputs: output/tables/firm_side_switching_table.tex
#          output/figures/firm_side_switching_event_study.pdf

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
fy[, log_admin_plaintiff := log1p(admin_plaintiff_case_n)]
fy[, log_admin_gov := log1p(admin_gov_case_n)]
fy[, admin_total := admin_plaintiff_case_n + admin_gov_case_n]
fy[, plaintiff_share := fifelse(admin_total > 0, admin_plaintiff_case_n / admin_total, NA_real_)]
fy[, any_plaintiff_case := as.integer(admin_plaintiff_case_n > 0)]

est <- function(outcome) {
  m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome)),
             data = fy[!is.na(get(outcome))], cluster = ~ award_id + firm_id)
  r <- pull_term(m, "did_treatment")
  r$pre_mean <- fy[treated_firm == 1L & event_time < 0 & !is.na(get(outcome)), mean(get(outcome))]
  r
}
outcomes <- c("log_admin_plaintiff", "any_plaintiff_case", "plaintiff_share", "log_admin_gov")
labels <- c("Plaintiff cases", "Any plaintiff case", "Plaintiff share", "Government cases")
res <- lapply(outcomes, est)

# Event study for the plaintiff share of the firm's administrative work
m_es <- feols(plaintiff_share ~ i(et, treated_firm, ref = -1) | stack_firm_fe + stack_year_fe,
              data = fy[!is.na(plaintiff_share)], cluster = ~ award_id + firm_id)
es_dt <- extract_event_dt(m_es)
native <- as.data.table(coeftable(m_es),keep.rownames="term")[grepl("^et::-?[0-9]+:treated_firm$",term)]
native[,event_time:=as.integer(sub("^et::(-?[0-9]+):treated_firm$","\\1",term))]
event_export <- merge(es_dt,native[,.(event_time,se=`Std. Error`,p_value=`Pr(>|t|)`)],by="event_time",all.x=TRUE,sort=TRUE)
event_export[,outcome:="plaintiff_share"]
fwrite(event_export,file.path(dirs$tables,"firm_side_event_coefficients.csv"))
pre <- joint_pretest(m_es, "treated_firm", "et", c(-5, -4, -3, -2))
plot_event_study(es_dt, "Plaintiff Share of Administrative Work", "Years Since the Contract",
                 res[[3]]$estimate, res[[3]]$se, pre$p_value,
                 file.path(dirs$figures, "firm_side_switching_event_study.pdf"))

header <- c(" & (1) & (2) & (3) & (4) \\\\", paste("Outcome &", paste(labels, collapse = " & "), "\\\\"))
body <- c(
  paste("Winner $\\times$ Post &", paste(sapply(res, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(res, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Winner pre-award mean &", paste(sapply(res, function(r) fmt_num(r$pre_mean)), collapse = " & "), "\\\\"),
  paste("Observations &", paste(sapply(res, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  paste("$R^2$ &", paste(sapply(res, function(r) fmt_num(r$r2)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "Stack $\\times$ Firm FE & Yes & Yes & Yes & Yes \\\\",
  "Stack $\\times$ Year FE & Yes & Yes & Yes & Yes \\\\"
)
note <- paste(
  "Alignment (H6): the contracted firm's administrative-litigation work by side, from the firm-year panel of winners and sampled comparison firms (event window $[-5, 5]$).",
  "Plaintiff-side cases are those in which a firm lawyer represented a plaintiff against a government agency; government-side cases are those in which the firm represented an agency.",
  "Columns 1 and 4 use log(1 + cases); column 3 requires at least one administrative case.",
  sprintf("The event study for column 3 (Figure~\\ref{fig:firm_side_switching_es}) has a joint pre-period $p$-value of %s.", fmt_p(pre$p_value)),
  "Standard errors clustered by stack and firm."
)
write_tpt_table(file.path(dirs$tables, "firm_side_switching_table.tex"),
                "Side-Switching: The Contracted Firm's Administrative Work Against and For the State",
                "tab:firm_side_switching", "lcccc", header, body, note, size = "\\footnotesize")
