#!/usr/bin/env Rscript
# city_year_robustness_fe_controls.R  (H1 robustness; H3 petition outcome)
#
# Re-estimates the city-year TWFE for the three headline outcomes under:
#   (1) baseline (contemporaneous city controls);
#   (2) no controls;
#   (3) controls lagged one year;
#   (4) province-by-year fixed effects;
#   (5) province-by-year FE plus court-reform timing indicators
#       (cross-regional jurisdiction pilot in force; provincial court-finance
#       centralization in force);
#   (6) never-adopting cities in provinces that funded outside counsel by
#       fiscal subsidy (possible unannounced hires) dropped from the controls.
# A second table reports the petition-rate outcome (H3) under TWFE and CS.
#
# Outputs: output/tables/city_year_fe_controls_robustness_table.tex
#          output/tables/city_year_petition_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
suppressPackageStartupMessages(library(did))
dirs <- sb_dirs(root_dir)

cy <- fread(file.path(dirs$data, "city_year_panel.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "city_year_supplement.csv"), encoding = "UTF-8")
cy <- merge(cy, sup, by = c("province", "city", "year"))
cy[, city_id := .GRP, by = .(province, city)]
cy[, province_year := sprintf("%s__%s", province, year)]
cy[, first_treat := if (any(treatment == 1L)) min(year[treatment == 1L]) else 0, by = city_id]
# Numeric cohorts preserve never-treated units during preprocessing.
cy[, first_treat := as.numeric(first_treat)]
setorder(cy, city_id, year)
cy[, `:=`(pop_l1 = shift(log_population_10k), gdp_l1 = shift(log_gdp), law_l1 = shift(log_registered_lawyers),
          caseload_l1 = shift(log_published_document_n)), by = city_id]
# Unverified policy fields are omitted; only the four available specifications are estimated.

outcomes <- c("government_win_rate", "appeal_rate", "log_admin_case_n")
out_labels <- c("Government win rate", "Appeal rate", "Log(1 + administrative cases)")
ctrl_now <- function(o) if (o == "government_win_rate") "log_population_10k + log_gdp + log_registered_lawyers + log_published_document_n" else "log_population_10k + log_gdp + log_registered_lawyers"
ctrl_lag <- function(o) if (o == "government_win_rate") "pop_l1 + gdp_l1 + law_l1 + caseload_l1" else "pop_l1 + gdp_l1 + law_l1"

spec <- function(o, k) {
  d <- cy
  rhs <- switch(k,
    "1" = paste("treatment +", ctrl_now(o)),
    "2" = "treatment",
    "3" = paste("treatment +", ctrl_lag(o)),
    "4" = paste("treatment +", ctrl_now(o)),
    "5" = paste("treatment + cross_region_pilot + finance_centralized +", ctrl_now(o)),
    "6" = paste("treatment +", ctrl_now(o)))
  fe <- if (k %in% c("4", "5")) "city_id + province_year" else "city_id + year"
  if (k == "6") d <- d[drop_subsidy_never == 0L]
  m <- feols(as.formula(sprintf("%s ~ %s | %s", o, rhs, fe)), data = d, cluster = ~ city_id)
  pull_term(m, "treatment")
}
available_specs <- as.character(1:4)
res <- lapply(outcomes, function(o) lapply(available_specs, function(k) spec(o, k)))

header <- c(
  " & (1) & (2) & (3) & (4) \\\\",
  "Specification & Baseline & No controls & Lagged controls & Prov.\\ $\\times$ year FE \\\\"
)
body <- unlist(lapply(seq_along(outcomes), function(i) {
  r <- res[[i]]
  c(sprintf("\\multicolumn{5}{l}{\\textit{Outcome: %s}} \\\\", out_labels[i]),
    paste("Treatment $\\times$ Post &", paste(sapply(r, function(x) coef_cell(x$estimate, x$se, x$p_value)[1]), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(r, function(x) coef_cell(x$estimate, x$se, x$p_value)[2]), collapse = " & "), "\\\\"),
    paste("Observations &", paste(sapply(r, function(x) fmt_int(x$n_obs)), collapse = " & "), "\\\\"),
    "\\addlinespace")
}))
body <- c(body,
  "City FE & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes &  \\\\",
  "Province $\\times$ year FE & & & & Yes \\\\")
note <- "Columns 1--4 use current controls, no controls, lagged controls, and province-by-year effects. Unverified reform dates and subsidy provinces are missing and are not estimated. Publication counts measure coverage rather than court incoming caseload. Standard errors clustered by city."
write_tpt_table(file.path(dirs$tables, "city_year_fe_controls_robustness_table.tex"),
                "City-Year Robustness: Controls and Province-by-Year Effects",
                "tab:city_year_fe_controls_robustness", "lcccc", header, body, note, size = "\\scriptsize")

# ---- petition rate (H3) ----------------------------------------------------
m_pet <- feols(petition_rate ~ treatment + log_population_10k + log_gdp + log_registered_lawyers | city_id + year,
               data = cy, cluster = ~ city_id)
r_pet <- pull_term(m_pet, "treatment")
set.seed(2026L)
cs <- att_gt(yname = "petition_rate", tname = "year", idname = "city_id", gname = "first_treat",
             xformla = ~ log_population_10k + log_gdp + log_registered_lawyers,
             data = as.data.frame(cy), panel = TRUE, control_group = "nevertreated",
             bstrap = TRUE, cband = TRUE, biters = 1000, clustervars = "city_id",
             est_method = "reg", base_period = "varying", print_details = FALSE)
cs_s <- aggte(cs, type = "simple")
r_cs <- list(estimate = cs_s$overall.att, se = cs_s$overall.se,
             p_value = 2 * pnorm(-abs(cs_s$overall.att / cs_s$overall.se)), n_obs = nrow(cy), r2 = NA_real_)
# case-level petition indicator
adm <- fread(file.path(dirs$data, "admin_case_level.csv"), encoding = "UTF-8",
             select = c("province", "city", "court_std", "year", "cause_group", "did_treatment", "petitioned",
                        "government_has_lawyer", "opponent_has_lawyer", "plaintiff_is_entity"))
adm <- adm[!is.na(court_std) & court_std != ""]
adm[, city_id := .GRP, by = .(province, city)]
adm[, court_id := .GRP, by = court_std]
m_case <- feols(petitioned ~ did_treatment + government_has_lawyer + opponent_has_lawyer + plaintiff_is_entity | court_id + year + cause_group,
                data = adm, cluster = ~ city_id + court_id)
r_case <- pull_term(m_case, "did_treatment")
cols <- list(r_pet, r_cs, r_case)
header2 <- c(
  " & (1) & (2) & (3) \\\\",
  "Level & City-year & City-year & Case \\\\",
  "Estimator & TWFE & CS & TWFE (LPM) \\\\"
)
body2 <- c(
  paste("Treatment $\\times$ Post &", paste(sapply(cols, function(r) coef_cell(r$estimate, r$se, r$p_value)[1]), collapse = " & "), "\\\\"),
  paste("&", paste(sapply(cols, function(r) coef_cell(r$estimate, r$se, r$p_value)[2]), collapse = " & "), "\\\\"),
  paste("Pre-procurement mean &", fmt_num(cy[first_treat > 0 & year < first_treat, mean(petition_rate)]), "&",
        fmt_num(cy[first_treat > 0 & year < first_treat, mean(petition_rate)]), "&",
        fmt_num(adm[did_treatment == 0L, mean(petitioned)]), "\\\\"),
  paste("Observations &", paste(sapply(cols, function(r) fmt_int(r$n_obs)), collapse = " & "), "\\\\"),
  "Controls & City & City & Case \\\\",
  "Fixed effects & City, year & --- & Court, year, cause group \\\\"
)
note2 <- paste(
  "Deterrence beyond the courtroom (H3): the share of administrative cases whose plaintiff also petitioned.",
  "Columns 1--2 use the city-year share (TWFE with city controls; Callaway--Sant'Anna with never-adopting controls); column 3 uses the case-level indicator.",
  "Standard errors clustered by city (columns 1--2) and by city and court (column 3)."
)
write_tpt_table(file.path(dirs$tables, "city_year_petition_table.tex"),
                "Procurement and Petitioning by Plaintiffs",
                "tab:city_year_petition", "lccc", header2, body2, note2)
