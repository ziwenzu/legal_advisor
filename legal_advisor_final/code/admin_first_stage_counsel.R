#!/usr/bin/env Rscript
# admin_first_stage_counsel.R  (H2, first stage)
#
# Does procurement change who represents the government in court? Estimates
# the effect of procurement on (i) whether the government appears with counsel
# (case-level LPM DID and city-year TWFE / Callaway-Sant'Anna on the counsel
# share) and (ii) whether the counsel is the contracted firm (case supplement).
# Also draws the city-year event study for the government counsel share.
#
# Outputs: output/tables/admin_first_stage_counsel_table.tex
#          output/figures/gov_lawyer_share_event_study.pdf

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
suppressPackageStartupMessages(library(did))
dirs <- sb_dirs(root_dir)

# ---- case level -----------------------------------------------------------
adm <- fread(file.path(dirs$data, "admin_case_level.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "admin_case_supplement.csv"), encoding = "UTF-8")
adm <- merge(adm, sup, by = "case_no")
adm <- adm[!is.na(court_std) & court_std != ""]
adm[, city_id := .GRP, by = .(province, city)]
adm[, court_id := .GRP, by = court_std]
adm[, contracted_firm := as.integer(gov_counsel_type == "contracted_firm")]

m_case <- feols(government_has_lawyer ~ did_treatment + plaintiff_is_entity + opponent_has_lawyer |
                  court_id + year + cause_group, data = adm, cluster = ~ city_id + court_id)
m_contract <- feols(contracted_firm ~ did_treatment + plaintiff_is_entity + opponent_has_lawyer |
                      court_id + year + cause_group, data = adm, cluster = ~ city_id + court_id)
m_contract_lawyered <- feols(contracted_firm ~ did_treatment + plaintiff_is_entity + opponent_has_lawyer |
                               court_id + year + cause_group, data = adm[government_has_lawyer == 1L],
                             cluster = ~ city_id + court_id)
r_case <- pull_term(m_case, "did_treatment")
r_contract <- pull_term(m_contract, "did_treatment")
r_contract_l <- pull_term(m_contract_lawyered, "did_treatment")

# ---- city-year ------------------------------------------------------------
cy <- fread(file.path(dirs$data, "city_year_panel.csv"), encoding = "UTF-8")
cy[, city_id := .GRP, by = .(province, city)]
cy[, first_treat := if (any(treatment == 1L)) min(year[treatment == 1L]) else 0, by = city_id]
# Numeric cohorts preserve never-treated units during preprocessing.
cy[, first_treat := as.numeric(first_treat)]
cy[, rel_time := fifelse(first_treat > 0, year - first_treat, NA_real_)]
cy[, ever_treated := as.integer(first_treat > 0)]

m_cy <- feols(gov_lawyer_share ~ treatment + log_population_10k + log_gdp + log_registered_lawyers |
                city_id + year, data = cy, cluster = ~ city_id)
r_cy <- pull_term(m_cy, "treatment")

set.seed(2026L)
cs <- att_gt(yname = "gov_lawyer_share", tname = "year", idname = "city_id", gname = "first_treat",
             xformla = ~ log_population_10k + log_gdp + log_registered_lawyers,
             data = as.data.frame(cy), panel = TRUE, control_group = "nevertreated",
             bstrap = TRUE, cband = TRUE, biters = 1000, clustervars = "city_id",
             est_method = "reg", base_period = "varying", print_details = FALSE)
cs_simple <- aggte(cs, type = "simple")
r_cs <- list(estimate = cs_simple$overall.att, se = cs_simple$overall.se,
             p_value = 2 * pnorm(-abs(cs_simple$overall.att / cs_simple$overall.se)),
             n_obs = nrow(cy), r2 = NA_real_)

# Event study (TWFE, binned endpoints at -5 / +5, never-treated as controls)
cy[, et_bin := fifelse(is.na(rel_time), -1000, pmin(pmax(rel_time, -5), 5))]
cy[, et_bin := fifelse(et_bin == -1000, -1, et_bin)]  # never-treated: all interactions zero
m_es <- feols(gov_lawyer_share ~ i(et_bin, ever_treated, ref = -1) + log_population_10k + log_gdp + log_registered_lawyers |
                city_id + year, data = cy, cluster = ~ city_id)
es_dt <- extract_event_dt(m_es)
fwrite(es_dt, file.path(dirs$tables, "gov_lawyer_share_event_coefficients.csv"))
pre <- joint_pretest(m_es, "ever_treated", "et_bin", c(-5, -4, -3, -2))
plot_event_study(es_dt, "Government Counsel Share", "Years Since Procurement",
                 r_cy$estimate, r_cy$se, pre$p_value,
                 file.path(dirs$figures, "gov_lawyer_share_event_study.pdf"))

# ---- table ----------------------------------------------------------------
cols <- list(r_case, r_contract, r_contract_l, r_cy, r_cs)
header <- c(
  " & \\multicolumn{3}{c}{Case level (LPM)} & \\multicolumn{2}{c}{City-year} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}",
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  "Outcome & Gov.\\ has counsel & Contracted firm & Contracted firm & Counsel share & Counsel share \\\\",
  "Sample & All cases & All cases & Lawyered cases & All city-years & All city-years \\\\",
  "Estimator & TWFE & TWFE & TWFE & TWFE & CS \\\\"
)
body <- c(
  paste("Treatment $\\times$ Post &", paste(sapply(cols, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(cols, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Pre-procurement mean &",
        fmt_num(adm[treated_city == 1L & post == 0L, mean(government_has_lawyer)]), "&",
        fmt_num(adm[treated_city == 1L & post == 0L, mean(contracted_firm)]), "&",
        fmt_num(adm[treated_city == 1L & post == 0L & government_has_lawyer == 1L, mean(contracted_firm)]), "&",
        fmt_num(cy[ever_treated == 1L & (year < first_treat), mean(gov_lawyer_share)]), "&",
        fmt_num(cy[ever_treated == 1L & (year < first_treat), mean(gov_lawyer_share)]), "\\\\"),
  paste("Observations &", paste(sapply(cols, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  paste("$R^2$ &", paste(sapply(cols, function(r) fmt_num(r$r2)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "Case controls & Yes & Yes & Yes & & \\\\",
  "City controls & & & & Yes & Yes \\\\",
  "Court / Year / Cause-group FE & Yes & Yes & Yes & & \\\\",
  "City / Year FE & & & & Yes & \\\\"
)
note <- paste(
  "First stage of the capacity channel (H2).",
  "Columns 1--3 are case-level linear-probability DID estimates: whether the government appears with counsel (column 1) and whether that counsel is the city's contracted firm (columns 2--3; column 3 restricts to cases in which the government has counsel). The contracted-firm indicator is zero before the award by construction, so columns 2--3 measure the post-award level of the contracted firm's share of the government's docket.",
  "Columns 4--5 use the city-year share of administrative cases with government counsel; column 5 is the Callaway--Sant'Anna overall ATT with never-adopting cities as controls.",
  "Case controls: plaintiff entity, opposing counsel. City controls: log population, log GDP, log registered lawyers.",
  "Standard errors clustered by city and court (columns 1--3) and by city (columns 4--5).",
  sprintf("The city-year event study (Figure~\\ref{fig:gov_lawyer_share_es}) has a joint pre-period $p$-value of %s.", fmt_p(pre$p_value))
)
write_tpt_table(file.path(dirs$tables, "admin_first_stage_counsel_table.tex"),
                "First Stage: Procurement and Government Representation in Court",
                "tab:admin_first_stage_counsel", "lccccc", header, body, note)
