#!/usr/bin/env Rscript
# admin_random_fake_treatment_placebo.R
#
# True placebo: among the 65 never-treated cities only, assign each
# city a *fake* event year drawn from the empirical distribution of
# real treatment years (2014--2020). Rebuild a fake `treatment`
# indicator (= 1 from the fake event year onward) and re-estimate
# the headline city-year DID.
#
# Identification logic: never-treated cities have, by construction,
# never received the procurement intervention. Any non-zero DID
# coefficient on a *random* assignment of treatment timing inside this
# group can only reflect either chance (sampling error from the
# permutation) or a violation of parallel pre-trends. We therefore
# expect coefficients close to zero and statistically insignificant
# under both TWFE and Callaway-Sant'Anna estimators.
#
# Input  : data/city_year_panel.csv
# Output : output/tables/admin_random_fake_treatment_placebo_appendix_table.tex

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did)
})

# --- locale guard for portable UTF-8 reads (RStudio + Rscript safe) ---
local({
  utf8_locales <- c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")
  for (loc in utf8_locales) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG = loc, LC_ALL = loc, LC_CTYPE = loc, LC_COLLATE = loc)
      break
    }
  }
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
setFixest_nthreads(4)

# Reproducibility seed for the random fake-event-year assignment.
PLACEBO_SEED <- 2026L

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
  if (outcome == "government_win_rate") controls <- c(controls, "log_published_document_n")
  controls
}

read_panel <- function() {
  city <- fread(city_path, encoding = "UTF-8")
  city[, city_id := .GRP, by = .(province, city)]
  city[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  city[
    ,
    real_first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else NA_real_,
    by = city_id
  ]
  city[]
}

# Empirical distribution of real first-treatment years across the 217 ever-
# treated cities. We sample fake event years from this distribution so the
# fake treatment timing matches the real one in shape.
empirical_event_years <- function(panel) {
  ever <- unique(panel[ever_treated == 1L, .(city_id, real_first_treat_year)])
  ever[!is.na(real_first_treat_year)]$real_first_treat_year
}

assign_fake_treatment <- function(panel, fake_treat_share = 0.70) {
  # In the never-treated city subsample, randomly designate
  # `fake_treat_share` of the cities as "fake-treated" (each gets a fake
  # event year drawn from the empirical distribution); the remaining
  # cities stay as "fake-never-treated" (fake_event_year = NA, fake
  # treatment = 0 always) and serve as the comparison group for the CS
  # estimator.
  set.seed(PLACEBO_SEED)
  emp <- empirical_event_years(panel)
  never <- unique(panel[ever_treated == 0L, .(city_id)])
  n_never <- nrow(never)
  n_fake_treated <- round(n_never * fake_treat_share)
  treated_ids <- sample(never$city_id, size = n_fake_treated, replace = FALSE)
  never[, fake_treated_indicator := as.integer(city_id %in% treated_ids)]
  never[fake_treated_indicator == 1L,
        fake_event_year := sample(emp, size = .N, replace = TRUE)]
  never[fake_treated_indicator == 0L, fake_event_year := NA_integer_]
  panel <- merge(panel, never[, .(city_id, fake_event_year, fake_treated_indicator)],
                 by = "city_id", all.x = TRUE)
  panel[, fake_treatment := 0L]
  panel[ever_treated == 0L & !is.na(fake_event_year),
        fake_treatment := as.integer(year >= fake_event_year)]
  panel[, fake_first_treat_year := 0.0]
  panel[ever_treated == 0L & !is.na(fake_event_year),
        fake_first_treat_year := as.numeric(fake_event_year)]
  panel
}

estimate_twfe_placebo <- function(panel, outcome) {
  rhs <- paste(c("fake_treatment", preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "fake_treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m))
}

estimate_cs_placebo <- function(panel, outcome) {
  controls_formula <- as.formula(paste("~", paste(preferred_controls(outcome), collapse = " + ")))
  cs_dt <- as.data.frame(panel)
  set.seed(2026L)
  att_gt_obj <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "city_id",
    gname = "fake_first_treat_year",
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
  panel <- assign_fake_treatment(panel)

  # The placebo sample: only never-treated cities. Any DID effect here
  # is entirely driven by random assignment of the fake event year.
  placebo_sample <- panel[ever_treated == 0L]

  outcomes <- list(
    list(label = "Gov.\\ Win Rate", key = "government_win_rate"),
    list(label = "Appeal Rate", key = "appeal_rate"),
    list(label = "Admin.\\ Cases", key = "log_admin_case_n")
  )

  twfe_rows <- character(0)
  cs_rows <- character(0)
  for (spec in outcomes) {
    h_twfe <- estimate_twfe_placebo(placebo_sample, spec$key)
    twfe_rows <- c(twfe_rows, paste(
      spec$label, "&",
      paste0(fmt_num(h_twfe$estimate), stars(h_twfe$p_value)), "&",
      paste0("(", fmt_num(h_twfe$se), ")"), "&",
      fmt_int(h_twfe$n_obs),
      "\\\\"
    ))
    h_cs <- tryCatch(estimate_cs_placebo(placebo_sample, spec$key),
                     error = function(e) NULL)
    if (!is.null(h_cs)) {
      cs_rows <- c(cs_rows, paste(
        spec$label, "&",
        paste0(fmt_num(h_cs$estimate), stars(h_cs$p_value)), "&",
        paste0("(", fmt_num(h_cs$se), ")"), "&",
        fmt_int(h_cs$n_obs),
        "\\\\"
      ))
    } else {
      cs_rows <- c(cs_rows, paste(
        spec$label, "& -- & -- & -- \\\\"
      ))
    }
  }

  n_never <- length(unique(placebo_sample$city_id))
  n_fake_treated <- length(unique(placebo_sample[fake_treated_indicator == 1L, city_id]))
  n_fake_never  <- length(unique(placebo_sample[fake_treated_indicator == 0L, city_id]))
  range_str <- paste(range(panel$year), collapse = "--")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Random Fake-Treatment Placebo within Never-Treated Cities}",
    "\\label{tab:admin_random_fake_treatment_placebo_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Outcome & Coefficient & SE & Observations \\\\",
    "\\midrule",
    "\\multicolumn{4}{l}{\\textit{Panel A. Two-way fixed effects (TWFE)}} \\\\",
    "\\addlinespace",
    twfe_rows,
    "\\midrule",
    "\\multicolumn{4}{l}{\\textit{Panel B. Callaway and Sant'Anna (CS) staggered estimator}} \\\\",
    "\\addlinespace",
    cs_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} Random-assignment diagnostic. The sample is restricted to the %d cities that never received the procurement intervention over %s.",
      "Within this never-treated subsample we randomly designate %d cities as ``fake-treated'' (each assigned a fake event year drawn with replacement from the empirical distribution of real adopters' first-treatment years, seed = %d) and leave the remaining %d cities as the fake-never-treated comparison group.",
      "None of these cities received the intervention. Interpretation of the assigned timing and data design follows the accompanying README.",
      "Panel A reports Treatment $\\times$ Post from a TWFE regression on the full placebo sample (%d cities) with city and year fixed effects; Panel B reports the overall CS ATT comparing fake-treated to fake-never-treated cells.",
      "Nonrejection is a diagnostic in the supplied panel and does not independently establish causal identification.",
      "City-year controls follow the main city-year table.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ), n_never, range_str, n_fake_treated, PLACEBO_SEED, n_fake_never, n_never),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, "admin_random_fake_treatment_placebo_appendix_table.tex")
  writeLines(lines, out_path)
  cat("Wrote", out_path, "\n")
}

main()
