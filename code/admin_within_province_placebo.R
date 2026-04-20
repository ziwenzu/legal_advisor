#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
city_path <- file.path(root_dir, "data", "city_year_panel.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

NEVER_TREATED_SENTINEL <- 0

stars <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

read_panel <- function() {
  city <- fread(city_path)
  city[, city_id := .GRP, by = .(province, city)]
  city[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  city[, province_id := .GRP, by = province]
  city[
    ,
    first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else NEVER_TREATED_SENTINEL,
    by = city_id
  ]
  city[]
}

estimate_twfe <- function(panel, outcome, fe_terms = "city_id + year") {
  rhs <- paste(c("treatment", preferred_controls(outcome)), collapse = " + ")
  f <- as.formula(sprintf("%s ~ %s | %s", outcome, rhs, fe_terms))
  m <- feols(f, data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m), r2 = fitstat(m, "r2")[[1]])
}

estimate_cs <- function(panel, outcome) {
  controls_formula <- as.formula(paste("~", paste(preferred_controls(outcome), collapse = " + ")))
  cs_dt <- as.data.frame(panel)
  att_gt_obj <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "city_id",
    gname = "first_treat_year",
    xformla = controls_formula,
    data = cs_dt,
    panel = TRUE,
    allow_unbalanced_panel = FALSE,
    control_group = "nevertreated",
    anticipation = 0,
    bstrap = TRUE,
    cband = TRUE,
    biters = 1000,
    clustervars = "city_id",
    est_method = "reg",
    base_period = "varying",
    print_details = FALSE
  )
  overall <- aggte(att_gt_obj, type = "simple")
  list(
    estimate = overall$overall.att,
    se = overall$overall.se,
    p_value = 2 * pnorm(abs(overall$overall.att / overall$overall.se), lower.tail = FALSE),
    n_obs = nrow(cs_dt)
  )
}

main <- function() {
  panel <- read_panel()
  province_support <- unique(panel[, .(province, city_id, ever_treated)])
  province_support <- province_support[
    ,
    .(
      has_treated_city = any(ever_treated == 1L),
      has_never_treated_city = any(ever_treated == 0L)
    ),
    by = province
  ]
  support_provinces <- province_support[
    has_treated_city == TRUE & has_never_treated_city == TRUE,
    province
  ]
  in_province <- panel[province %in% support_provinces]

  full_treated <- length(unique(panel[ever_treated == 1L, city_id]))
  full_control <- length(unique(panel[ever_treated == 0L, city_id]))
  ip_treated <- length(unique(in_province[ever_treated == 1L, city_id]))
  ip_control <- length(unique(in_province[ever_treated == 0L, city_id]))

  outcomes <- list(
    list(label = "Gov.\\ Win Rate", key = "government_win_rate"),
    list(label = "Appeal Rate", key = "appeal_rate"),
    list(label = "Admin.\\ Cases", key = "admin_case_n")
  )

  twfe_rows <- character(0)
  cs_rows <- character(0)
  for (spec in outcomes) {
    h_twfe <- estimate_twfe(panel, spec$key, fe_terms = "city_id + year")
    s_twfe <- estimate_twfe(in_province, spec$key, fe_terms = "city_id + year")
    p_twfe <- estimate_twfe(in_province, spec$key, fe_terms = "city_id + province_id^year")
    twfe_rows <- c(
      twfe_rows,
      paste(
        spec$label, "&",
        paste0(fmt_num(h_twfe$estimate), stars(h_twfe$p_value)), "&",
        paste0("(", fmt_num(h_twfe$se), ")"), "&",
        paste0(fmt_num(s_twfe$estimate), stars(s_twfe$p_value)), "&",
        paste0("(", fmt_num(s_twfe$se), ")"), "&",
        paste0(fmt_num(p_twfe$estimate), stars(p_twfe$p_value)), "&",
        paste0("(", fmt_num(p_twfe$se), ")"),
        "\\\\"
      )
    )
    h_cs <- estimate_cs(panel, spec$key)
    s_cs <- estimate_cs(in_province, spec$key)
    cs_rows <- c(
      cs_rows,
      paste(
        spec$label, "&",
        paste0(fmt_num(h_cs$estimate), stars(h_cs$p_value)), "&",
        paste0("(", fmt_num(h_cs$se), ")"), "&",
        paste0(fmt_num(s_cs$estimate), stars(s_cs$p_value)), "&",
        paste0("(", fmt_num(s_cs$se), ")"),
        "& -- & -- \\\\"
      )
    )
  }

  twfe_obs_h <- nobs(feols(government_win_rate ~ treatment + log_population_10k + log_gdp +
                             log_registered_lawyers + log_court_caseload_n | city_id + year,
                           data = panel, cluster = ~ city_id))
  twfe_obs_s <- nobs(feols(government_win_rate ~ treatment + log_population_10k + log_gdp +
                             log_registered_lawyers + log_court_caseload_n | city_id + year,
                           data = in_province, cluster = ~ city_id))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Same-Province Donor-Pool Placebo for City-Year Administrative Estimates}",
    "\\label{tab:admin_within_province_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Headline} & \\multicolumn{2}{c}{Same-province sample} & \\multicolumn{2}{c}{+ Province $\\times$ Year FE} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    "Outcome & Coefficient & SE & Coefficient & SE & Coefficient & SE \\\\",
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Panel A. Two-way fixed effects (TWFE)}} \\\\",
    "\\addlinespace",
    twfe_rows,
    "\\addlinespace",
    paste0("Observations & \\multicolumn{2}{c}{", fmt_int(twfe_obs_h),
           "} & \\multicolumn{2}{c}{", fmt_int(twfe_obs_s),
           "} & \\multicolumn{2}{c}{", fmt_int(twfe_obs_s), "} \\\\"),
    paste0("Cities (treated / never-treated) & \\multicolumn{2}{c}{",
           full_treated, " / ", full_control,
           "} & \\multicolumn{2}{c}{",
           ip_treated, " / ", ip_control,
           "} & \\multicolumn{2}{c}{",
           ip_treated, " / ", ip_control, "} \\\\"),
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Panel B. Callaway and Sant'Anna (CS) staggered estimator}} \\\\",
    "\\addlinespace",
    cs_rows,
    "\\addlinespace",
    paste0("Observations & \\multicolumn{2}{c}{", fmt_int(nrow(panel)),
           "} & \\multicolumn{2}{c}{", fmt_int(nrow(in_province)),
           "} & \\multicolumn{2}{c}{--} \\\\"),
    paste0("Cities (treated / never-treated) & \\multicolumn{2}{c}{",
           full_treated, " / ", full_control,
           "} & \\multicolumn{2}{c}{",
           ip_treated, " / ", ip_control,
           "} & \\multicolumn{2}{c}{--} \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each cell reports the city-year procurement effect on one outcome.",
      "Panel A reports Treatment $\\times$ Post from two-way fixed-effects regressions; Panel B reports the overall ATT from the Callaway and Sant'Anna (CS) staggered estimator with never-treated cities as the comparison group.",
      "Headline columns reproduce the main city-year specification on all sample cities; Same-province sample columns restrict the sample to provinces that contain both at least one procurement-adopting city and at least one never-treated city; + Province $\\times$ Year FE columns add province-by-year fixed effects to that sub-sample (not applicable to the CS estimator and reported as ``--'').",
      "City-year controls follow the main city-year table: log population, log GDP, and log registered lawyers in all columns, with log court caseload added only for the government-win-rate specification.",
      "Standard errors clustered by city (TWFE) and obtained from the multiplier bootstrap clustered by city (CS).",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, "admin_within_province_placebo_appendix_table.tex")
  writeLines(lines, out_path)
  cat("Wrote", out_path, "\n")
}

main()
