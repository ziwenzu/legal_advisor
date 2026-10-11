#!/usr/bin/env Rscript
# city_year_adoption_hazard.R  (timing exogeneity)
#
# Does a city's recent litigation record predict WHEN it first procures
# counsel? Discrete-time hazard: among city-years not yet treated, regress the
# first-adoption indicator on lagged outcomes (t-1 and t-2 changes), city
# characteristics, and year fixed effects (linear probability, clustered by
# city). A logit version is reported in the last column.
#
# Output: output/tables/city_year_adoption_hazard_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

cy <- fread(file.path(dirs$data, "city_year_panel.csv"), encoding = "UTF-8")
cy[, city_id := .GRP, by = .(province, city)]
cy[, province_id := .GRP, by = province]
cy[, first_treat := if (any(treatment == 1L)) min(year[treatment == 1L]) else 0, by = city_id]
setorder(cy, city_id, year)
cy[, adopt := as.integer(first_treat > 0 & year == first_treat)]
cy[, `:=`(gw_l1 = shift(government_win_rate, 1), gw_l2 = shift(government_win_rate, 2),
          ap_l1 = shift(appeal_rate, 1), ap_l2 = shift(appeal_rate, 2),
          lc_l1 = shift(log(admin_case_n), 1), lc_l2 = shift(log(admin_case_n), 2),
          pet_l1 = shift(petition_rate, 1), gl_l1 = shift(gov_lawyer_share, 1)), by = city_id]
cy[, `:=`(d_gw = gw_l1 - gw_l2, d_ap = ap_l1 - ap_l2, d_lc = lc_l1 - lc_l2)]
# risk set: not yet treated (year <= first_treat for adopters; all years for never-adopters), 2015-2020
risk <- cy[year >= 2015 & (first_treat == 0 | year <= first_treat) & !is.na(gw_l2)]

f1 <- adopt ~ gw_l1 + ap_l1 + lc_l1 | year
f2 <- adopt ~ gw_l1 + ap_l1 + lc_l1 + pet_l1 + gl_l1 + log_population_10k + log_gdp + log_registered_lawyers | year
f3 <- adopt ~ d_gw + d_ap + d_lc + log_population_10k + log_gdp + log_registered_lawyers | year
f4 <- adopt ~ gw_l1 + ap_l1 + lc_l1 + pet_l1 + gl_l1 + log_population_10k + log_gdp + log_registered_lawyers | year + province_id
m1 <- feols(f1, data = risk, cluster = ~ city_id)
m2 <- feols(f2, data = risk, cluster = ~ city_id)
m3 <- feols(f3, data = risk, cluster = ~ city_id)
m4 <- feols(f4, data = risk, cluster = ~ city_id)
m5 <- feglm(adopt ~ gw_l1 + ap_l1 + lc_l1 + pet_l1 + gl_l1 + log_population_10k + log_gdp + log_registered_lawyers | year,
            data = risk, family = binomial("logit"), cluster = ~ city_id)

models <- list(m1, m2, m3, m4, m5)
vars <- c(gw_l1 = "Gov.\\ win rate, $t-1$", ap_l1 = "Appeal rate, $t-1$", lc_l1 = "Log filings, $t-1$",
          d_gw = "$\\Delta$ Gov.\\ win rate, $t-1$", d_ap = "$\\Delta$ Appeal rate, $t-1$", d_lc = "$\\Delta$ Log filings, $t-1$",
          pet_l1 = "Petition rate, $t-1$", gl_l1 = "Gov.\\ counsel share, $t-1$",
          log_population_10k = "Log population", log_gdp = "Log GDP", log_registered_lawyers = "Log registered lawyers")
row_for <- function(v) {
  cells <- sapply(models, function(m) {
    r <- pull_term(m, v)
    if (is.na(r$estimate)) c("", "") else coef_cell(r$estimate, r$se, r$p_value)
  })
  c(paste(vars[[v]], "&", paste(cells[1, ], collapse = " & "), "\\\\"),
    paste("&", paste(cells[2, ], collapse = " & "), "\\\\"))
}
joint_p <- sapply(models[1:2], function(m) suppressMessages(wald(m, keep = "^(gw_l1|ap_l1|lc_l1)$", print = FALSE))$p)
joint_p3 <- suppressMessages(wald(m3, keep = "^(d_gw|d_ap|d_lc)$", print = FALSE))$p
joint_p4 <- suppressMessages(wald(m4, keep = "^(gw_l1|ap_l1|lc_l1)$", print = FALSE))$p
joint_p5 <- suppressMessages(wald(m5, keep = "^(gw_l1|ap_l1|lc_l1)$", print = FALSE))$p

header <- c(
  " & (1) & (2) & (3) & (4) & (5) \\\\",
  "Estimator & LPM & LPM & LPM & LPM & Logit \\\\"
)
body <- c(
  unlist(lapply(names(vars), row_for)),
  "\\addlinespace",
  paste("Joint test, litigation outcomes ($p$) &", fmt_p(joint_p[1]), "&", fmt_p(joint_p[2]), "&", fmt_p(joint_p3), "&", fmt_p(joint_p4), "&", fmt_p(joint_p5), "\\\\"),
  paste("Adoption rate in risk set &", paste(rep(fmt_num(mean(risk$adopt)), 5), collapse = " & "), "\\\\"),
  paste("Observations (city-years at risk) &", paste(sapply(models, function(m) fmt_int(nobs(m))), collapse = " & "), "\\\\"),
  "Year FE & Yes & Yes & Yes & Yes & Yes \\\\",
  "Province FE & & & & Yes & \\\\"
)
note <- paste(
  "Discrete-time hazard of first procurement among city-years not yet treated, 2015--2020.",
  "The outcome is an indicator for the city's first award year. Lagged outcomes are the city's own administrative-litigation record in the previous year (columns 1, 2, 4, 5) or its change over the previous two years (column 3).",
  "The joint test asks whether the litigation outcomes predict adoption timing once city characteristics and year effects are held fixed.",
  "Column 5 reports logit coefficients. Standard errors clustered by city."
)
write_tpt_table(file.path(dirs$tables, "city_year_adoption_hazard_table.tex"),
                "Does the Litigation Record Predict the Timing of Adoption?",
                "tab:city_year_adoption_hazard", "lccccc", header, body, note, size = "\\scriptsize")
