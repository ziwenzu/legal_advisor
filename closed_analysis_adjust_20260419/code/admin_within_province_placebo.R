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
  city[, first_treat_year := as.numeric(first_treat_year)]
  setorder(city, city_id, year)
  city[]
}

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") {
    controls <- c(controls, "log_court_caseload_n")
  }
  controls
}

estimate_twfe <- function(panel, outcome, fe_terms = "city_id + year") {
  rhs <- paste(c("treatment", preferred_controls(outcome)), collapse = " + ")
  f <- as.formula(sprintf("%s ~ %s | %s", outcome, rhs, fe_terms))
  m <- feols(f, data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(
    estimator = "TWFE",
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(m),
    r2 = fitstat(m, "r2")[[1]]
  )
}

estimate_cs <- function(panel, outcome) {
  controls_formula <- as.formula(paste("~", paste(preferred_controls(outcome), collapse = " + ")))
  cs_dt <- as.data.frame(copy(panel))

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
  agg <- aggte(att_gt_obj, type = "simple")
  effective_n <- tryCatch(nrow(att_gt_obj$DIDparams$data), error = function(e) NA_integer_)
  list(
    estimator = "CS",
    estimate = agg$overall.att,
    se = agg$overall.se,
    p_value = 2 * pnorm(abs(agg$overall.att / agg$overall.se), lower.tail = FALSE),
    n_obs = effective_n,
    r2 = NA_real_
  )
}

count_cities <- function(panel) {
  meta <- unique(panel[, .(city_id, ever_treated)])
  list(
    treated = sum(meta$ever_treated == 1L),
    never_treated = sum(meta$ever_treated == 0L)
  )
}

cells_with_paren <- function(res) {
  c(
    paste0(fmt_num(res$estimate), stars(res$p_value)),
    paste0("(", fmt_num(res$se), ")")
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
  support_provinces <- province_support[has_treated_city == TRUE & has_never_treated_city == TRUE, province]
  in_province <- panel[province %in% support_provinces]

  outcomes <- list(
    list(key = "government_win_rate", label = "Gov.\\ Win Rate"),
    list(key = "appeal_rate", label = "Appeal Rate"),
    list(key = "admin_case_n", label = "Admin.\\ Cases")
  )

  twfe_rows <- vector("list", length(outcomes))
  cs_rows <- vector("list", length(outcomes))
  for (i in seq_along(outcomes)) {
    spec <- outcomes[[i]]
    twfe_rows[[i]] <- list(
      label = spec$label,
      headline = estimate_twfe(panel, spec$key, fe_terms = "city_id + year"),
      sample = estimate_twfe(in_province, spec$key, fe_terms = "city_id + year"),
      pyear = estimate_twfe(in_province, spec$key, fe_terms = "city_id + province_id^year")
    )
    cs_rows[[i]] <- list(
      label = spec$label,
      headline = estimate_cs(panel, spec$key),
      sample = estimate_cs(in_province, spec$key)
    )
  }

  twfe_obs_headline <- unique(vapply(twfe_rows, function(r) r$headline$n_obs, numeric(1)))
  twfe_obs_sample <- unique(vapply(twfe_rows, function(r) r$sample$n_obs, numeric(1)))
  twfe_obs_pyear <- unique(vapply(twfe_rows, function(r) r$pyear$n_obs, numeric(1)))
  cs_obs_headline <- unique(vapply(cs_rows, function(r) r$headline$n_obs, numeric(1)))
  cs_obs_sample <- unique(vapply(cs_rows, function(r) r$sample$n_obs, numeric(1)))
  if (length(twfe_obs_headline) != 1L || length(twfe_obs_sample) != 1L || length(twfe_obs_pyear) != 1L ||
      length(cs_obs_headline) != 1L || length(cs_obs_sample) != 1L) {
    stop("Outcome-specific observation counts differ in admin_within_province_placebo.R")
  }

  cities_full <- count_cities(panel)
  cities_in_prov <- count_cities(in_province)
  cities_cell <- function(cnt) sprintf("%d / %d", cnt$treated, cnt$never_treated)

  twfe_body <- vapply(twfe_rows, function(r) {
    h <- cells_with_paren(r$headline)
    s <- cells_with_paren(r$sample)
    p <- cells_with_paren(r$pyear)
    paste(
      r$label, "&",
      h[1], "&", h[2], "&",
      s[1], "&", s[2], "&",
      p[1], "&", p[2],
      "\\\\"
    )
  }, character(1))

  cs_body <- vapply(cs_rows, function(r) {
    h <- cells_with_paren(r$headline)
    s <- cells_with_paren(r$sample)
    paste(
      r$label, "&",
      h[1], "&", h[2], "&",
      s[1], "&", s[2], "&",
      "--", "&", "--",
      "\\\\"
    )
  }, character(1))

  twfe_obs_row <- paste0(
    "Observations & \\multicolumn{2}{c}{", fmt_int(twfe_obs_headline), "}",
    " & \\multicolumn{2}{c}{", fmt_int(twfe_obs_sample), "}",
    " & \\multicolumn{2}{c}{", fmt_int(twfe_obs_pyear), "} \\\\"
  )
  twfe_cities_row <- paste0(
    "Cities (treated / never-treated) & \\multicolumn{2}{c}{", cities_cell(cities_full), "}",
    " & \\multicolumn{2}{c}{", cities_cell(cities_in_prov), "}",
    " & \\multicolumn{2}{c}{", cities_cell(cities_in_prov), "} \\\\"
  )
  cs_obs_row <- paste0(
    "Observations & \\multicolumn{2}{c}{", fmt_int(cs_obs_headline), "}",
    " & \\multicolumn{2}{c}{", fmt_int(cs_obs_sample), "}",
    " & \\multicolumn{2}{c}{--} \\\\"
  )
  cs_cities_row <- paste0(
    "Cities (treated / never-treated) & \\multicolumn{2}{c}{", cities_cell(cities_full), "}",
    " & \\multicolumn{2}{c}{", cities_cell(cities_in_prov), "}",
    " & \\multicolumn{2}{c}{--} \\\\"
  )

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
    twfe_body,
    "\\addlinespace",
    twfe_obs_row,
    twfe_cities_row,
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Panel B. Callaway and Sant'Anna (CS) staggered estimator}} \\\\",
    "\\addlinespace",
    cs_body,
    "\\addlinespace",
    cs_obs_row,
    cs_cities_row,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each cell reports the city-year procurement effect on one outcome.",
      "Panel A reports Treatment $\\times$ Post from two-way fixed-effects regressions; Panel B reports the overall average treatment effect on the treated from the Callaway and Sant'Anna (CS) staggered estimator with never-treated cities as the comparison group.",
      "Headline columns reproduce the main city-year specification on all sample cities.",
      "Same-province sample columns restrict the sample to provinces that contain both at least one procurement-adopting city and at least one never-treated city, so that the donor pool is supported within province; the TWFE specification keeps city and year fixed effects.",
      "+ Province $\\times$ Year FE columns add province-by-year fixed effects to the same support-restricted sample, identifying the procurement effect from within-province-year variation; this column is not applicable to the CS estimator and is reported as ``--''.",
      "The CS coefficients on government win rate, appeal rate, and administrative-case volume are essentially unchanged when the sample is restricted to within-province donor pools, indicating that the staggered-DiD findings are not driven by cross-province comparisons.",
      "Under the tighter within-province identification, the TWFE point estimate for administrative-case volume grows in magnitude and gains precision, suggesting that the headline TWFE understates the contraction.",
      "City-year controls: log population, log GDP, log registered lawyers, and log court caseload (the last enters only for the government-win-rate specification, matching the main city-year table).",
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
