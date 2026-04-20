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
CITY_CONTROL_VARS <- c(
  "log_population_10k",
  "log_gdp",
  "log_registered_lawyers",
  "log_court_caseload_n"
)

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

preferred_controls <- function(outcome_name) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome_name == "government_win_rate") {
    controls <- c(controls, "log_court_caseload_n")
  }
  controls
}

read_city_panel <- function(path) {
  dt <- fread(path)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  dt[
    ,
    first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else NEVER_TREATED_SENTINEL,
    by = city_id
  ]
  dt[, first_treat_year := as.numeric(first_treat_year)]
  setorder(dt, city_id, year)
  dt[]
}

city_means <- function(panel) {
  panel[
    ,
    .(
      ever_treated = ever_treated[1],
      log_population_10k = mean(log_population_10k, na.rm = TRUE),
      log_gdp = mean(log_gdp, na.rm = TRUE),
      log_registered_lawyers = mean(log_registered_lawyers, na.rm = TRUE),
      log_court_caseload_n = mean(log_court_caseload_n, na.rm = TRUE)
    ),
    by = .(city_id)
  ]
}

estimate_propensity <- function(city_dt) {
  city_dt[, ever_treated := as.integer(ever_treated)]
  fit <- glm(
    ever_treated ~ log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n,
    data = city_dt,
    family = binomial(link = "logit")
  )
  city_dt[, pscore := pmin(0.95, pmax(0.05, predict(fit, type = "response")))]
  city_dt[, ipw_weight := fifelse(ever_treated == 1L, 1.0, pscore / (1 - pscore))]
  city_dt[]
}

entropy_balance <- function(city_dt, vars) {
  treated <- city_dt[ever_treated == 1L]
  control <- city_dt[ever_treated == 0L]
  if (nrow(control) == 0L || nrow(treated) == 0L) {
    city_dt[, eb_weight := 1.0]
    return(city_dt)
  }
  X_full <- as.matrix(rbind(treated[, ..vars], control[, ..vars]))
  center <- colMeans(X_full)
  scale_x <- apply(X_full, 2, sd)
  scale_x[scale_x == 0] <- 1
  Xc <- scale(as.matrix(control[, ..vars]), center = center, scale = scale_x)
  target <- as.numeric(scale(matrix(colMeans(as.matrix(treated[, ..vars])), nrow = 1),
                             center = center, scale = scale_x))

  loss <- function(lambda) {
    z <- as.numeric(Xc %*% lambda)
    z <- z - max(z)
    w <- exp(z)
    w <- w / sum(w)
    moments <- as.numeric(colSums(Xc * w))
    sum((moments - target)^2)
  }

  best <- list(par = rep(0, length(vars)), value = loss(rep(0, length(vars))))
  for (method in c("BFGS", "Nelder-Mead", "CG")) {
    fit <- tryCatch(
      optim(rep(0, length(vars)), loss, method = method, control = list(maxit = 2000, reltol = 1e-12)),
      error = function(e) NULL
    )
    if (!is.null(fit) && !is.na(fit$value) && fit$value < best$value) best <- fit
  }

  z <- as.numeric(Xc %*% best$par)
  z <- z - max(z)
  w <- exp(z)
  w <- w / sum(w) * nrow(treated)
  city_dt[, eb_weight := 1.0]
  city_dt[ever_treated == 0L, eb_weight := as.numeric(w)]
  city_dt[]
}

attach_weights <- function(panel, city_dt) {
  weights <- city_dt[, .(city_id, pscore, ipw_weight, eb_weight)]
  panel <- merge(panel, weights, by = "city_id", all.x = TRUE, sort = FALSE)
  panel[]
}

caliper_subset <- function(panel, city_dt, sd_cutoff = 0.5) {
  treated <- city_dt[ever_treated == 1L]
  treated_mean <- colMeans(as.matrix(treated[, ..CITY_CONTROL_VARS]), na.rm = TRUE)
  treated_sd <- apply(as.matrix(treated[, ..CITY_CONTROL_VARS]), 2, sd, na.rm = TRUE)
  is_control <- city_dt$ever_treated == 0L
  within_caliper <- rep(TRUE, nrow(city_dt))
  for (v in CITY_CONTROL_VARS) {
    within_caliper <- within_caliper &
      (abs(city_dt[[v]] - treated_mean[[v]]) <= sd_cutoff * treated_sd[[v]])
  }
  keep <- city_dt$ever_treated == 1L | (is_control & within_caliper)
  cities_keep <- city_dt$city_id[keep]
  panel[city_id %in% cities_keep]
}

estimate_twfe <- function(panel, outcome, weights = NULL) {
  rhs <- paste(c("treatment", preferred_controls(outcome)), collapse = " + ")
  f <- as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs))
  if (is.null(weights)) {
    m <- feols(f, data = panel, cluster = ~ city_id)
  } else {
    m <- feols(f, data = panel, weights = panel[[weights]], cluster = ~ city_id)
  }
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(m),
    r2 = fitstat(m, "r2")[[1]]
  )
}

estimate_cs <- function(panel, outcome, weight_col = NULL) {
  controls_formula <- as.formula(
    paste("~", paste(preferred_controls(outcome), collapse = " + "))
  )
  cs_dt <- as.data.frame(panel)
  args <- list(
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
  if (!is.null(weight_col)) args$weightsname <- weight_col
  att_gt_obj <- do.call(att_gt, args)
  overall <- aggte(att_gt_obj, type = "simple")
  list(
    estimate = overall$overall.att,
    se = overall$overall.se,
    p_value = 2 * pnorm(abs(overall$overall.att / overall$overall.se), lower.tail = FALSE),
    n_obs = nrow(cs_dt),
    r2 = NA_real_
  )
}

balance_diff <- function(city_dt, weight_col, restrict_ids = NULL) {
  out <- list()
  treated <- city_dt[ever_treated == 1L]
  controls <- city_dt[ever_treated == 0L]
  if (!is.null(restrict_ids)) controls <- controls[city_id %in% restrict_ids]
  for (v in CITY_CONTROL_VARS) {
    t_mean <- mean(treated[[v]], na.rm = TRUE)
    if (is.null(weight_col) || nrow(controls) == 0L) {
      c_mean <- mean(controls[[v]], na.rm = TRUE)
    } else {
      w <- controls[[weight_col]]
      if (sum(w, na.rm = TRUE) > 0) {
        c_mean <- sum(controls[[v]] * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
      } else {
        c_mean <- mean(controls[[v]], na.rm = TRUE)
      }
    }
    out[[v]] <- list(treated = t_mean, control = c_mean, diff = t_mean - c_mean)
  }
  out
}

main <- function() {
  panel <- read_city_panel(city_path)
  cdt <- city_means(panel)
  cdt <- estimate_propensity(cdt)
  cdt <- entropy_balance(cdt, vars = CITY_CONTROL_VARS)
  panel <- attach_weights(panel, cdt)

  cal_panel <- caliper_subset(panel, cdt, sd_cutoff = 1.0)
  caliper_ids <- unique(cal_panel$city_id)
  caliper_treated <- length(intersect(caliper_ids, cdt[ever_treated == 1L, city_id]))
  caliper_control <- length(intersect(caliper_ids, cdt[ever_treated == 0L, city_id]))
  full_treated <- cdt[ever_treated == 1L, .N]
  full_control <- cdt[ever_treated == 0L, .N]

  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  variants <- c("baseline", "ipw", "eb", "cal")

  twfe_results <- list()
  cs_results <- list()
  for (outcome in outcomes) {
    twfe_results[[paste(outcome, "baseline", sep = "_")]] <- estimate_twfe(panel, outcome)
    twfe_results[[paste(outcome, "ipw", sep = "_")]] <- estimate_twfe(panel, outcome, weights = "ipw_weight")
    twfe_results[[paste(outcome, "eb", sep = "_")]] <- estimate_twfe(panel, outcome, weights = "eb_weight")
    twfe_results[[paste(outcome, "cal", sep = "_")]] <- estimate_twfe(cal_panel, outcome)
    cs_results[[paste(outcome, "baseline", sep = "_")]] <- estimate_cs(panel, outcome)
    cs_results[[paste(outcome, "ipw", sep = "_")]] <- estimate_cs(panel, outcome, weight_col = "ipw_weight")
    cs_results[[paste(outcome, "eb", sep = "_")]] <- estimate_cs(panel, outcome, weight_col = "eb_weight")
    cs_results[[paste(outcome, "cal", sep = "_")]] <- estimate_cs(cal_panel, outcome)
  }

  bal_raw <- balance_diff(cdt, weight_col = NULL)
  bal_ipw <- balance_diff(cdt, weight_col = "ipw_weight")
  bal_eb <- balance_diff(cdt, weight_col = "eb_weight")
  bal_cal <- balance_diff(cdt, weight_col = NULL, restrict_ids = caliper_ids)

  fmt_cell <- function(res) paste0(fmt_num(res$estimate), stars(res$p_value))
  fmt_se <- function(res) paste0("(", fmt_num(res$se), ")")
  ordered_keys <- unlist(lapply(outcomes, function(o) paste(o, variants, sep = "_")))
  fmt_keys <- function(res_list) {
    list(
      coef = sapply(ordered_keys, function(k) fmt_cell(res_list[[k]])),
      se = sapply(ordered_keys, function(k) fmt_se(res_list[[k]])),
      n = sapply(ordered_keys, function(k) fmt_int(res_list[[k]]$n_obs))
    )
  }
  twfe_cells <- fmt_keys(twfe_results)
  cs_cells <- fmt_keys(cs_results)
  twfe_r2 <- sapply(ordered_keys, function(k) fmt_num(twfe_results[[k]]$r2))
  cities_row <- vapply(variants, function(v) {
    if (v == "cal") sprintf("%d / %d", caliper_treated, caliper_control)
    else sprintf("%d / %d", full_treated, full_control)
  }, character(1))
  cities_row_full <- rep(cities_row, length(outcomes))

  bal_block <- function() {
    var_labels <- c(
      log_population_10k = "Log Population (10k)",
      log_gdp = "Log GDP",
      log_registered_lawyers = "Log Registered Lawyers",
      log_court_caseload_n = "Log Court Caseload"
    )
    rows <- character(0)
    for (v in names(var_labels)) {
      rows <- c(
        rows,
        paste(
          var_labels[[v]],
          "&", fmt_num(bal_raw[[v]]$treated),
          "&", fmt_num(bal_raw[[v]]$control),
          "&", fmt_num(bal_raw[[v]]$diff),
          "&", fmt_num(bal_ipw[[v]]$control),
          "&", fmt_num(bal_ipw[[v]]$diff),
          "&", fmt_num(bal_eb[[v]]$control),
          "&", fmt_num(bal_eb[[v]]$diff),
          "&", fmt_num(bal_cal[[v]]$control),
          "&", fmt_num(bal_cal[[v]]$diff),
          "& & & \\\\"
        )
      )
    }
    rows
  }

  variant_labels <- c("Baseline", "IPW", "Entropy", "Caliper")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Selection-into-Treatment Robustness for City-Year Administrative Estimates}",
    "\\label{tab:city_year_selection_robustness_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{l*{12}{c}}",
    "\\toprule",
    "\\multicolumn{13}{l}{\\textit{Panel A. Treatment effect under alternative weighting and sample restrictions (TWFE)}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{4}{c}{Government Win Rate} & \\multicolumn{4}{c}{Appeal Rate} & \\multicolumn{4}{c}{Administrative Cases} \\\\",
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13}",
    paste(" &", paste(rep(variant_labels, length(outcomes)), collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(twfe_cells$coef, collapse = " & "), "\\\\"),
    paste("&", paste(twfe_cells$se, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(twfe_cells$n, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(twfe_r2, collapse = " & "), "\\\\"),
    paste("Cities (treated / never-treated) &", paste(cities_row_full, collapse = " & "), "\\\\"),
    "\\midrule",
    "\\multicolumn{13}{l}{\\textit{Panel B. Treatment effect under the same variants (Callaway--Sant'Anna)}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{4}{c}{Government Win Rate} & \\multicolumn{4}{c}{Appeal Rate} & \\multicolumn{4}{c}{Administrative Cases} \\\\",
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13}",
    paste(" &", paste(rep(variant_labels, length(outcomes)), collapse = " & "), "\\\\"),
    "\\midrule",
    paste("CS overall ATT &", paste(cs_cells$coef, collapse = " & "), "\\\\"),
    paste("&", paste(cs_cells$se, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(cs_cells$n, collapse = " & "), "\\\\"),
    paste("Cities (treated / never-treated) &", paste(cities_row_full, collapse = " & "), "\\\\"),
    "\\midrule",
    "\\multicolumn{13}{l}{\\textit{Panel C. Covariate balance between treated and never-treated cities}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{3}{c}{Unweighted} & \\multicolumn{2}{c}{IPW-weighted} & \\multicolumn{2}{c}{Entropy-weighted} & \\multicolumn{2}{c}{Caliper-restricted} & \\multicolumn{2}{c}{} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\\cmidrule(lr){9-10}",
    " & Treated & Control & Diff & Control & Diff & Control & Diff & Control & Diff & & \\\\",
    "\\midrule",
    bal_block(),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Panels A and B report the city-year procurement effect on each outcome under four variants of the donor pool.",
      "Panel A reports Treatment $\\times$ Post from two-way fixed-effects regressions; Panel B reports the overall average treatment effect on the treated from the Callaway and Sant'Anna (CS) staggered estimator with never-treated cities as the comparison group.",
      "\\textit{Baseline} is the unweighted main specification; \\textit{IPW} weights each never-treated city by its propensity-score odds $\\hat{p}/(1-\\hat{p})$ from a logit on the four city-mean covariates and trimmed to $[0.05, 0.95]$ (treated cities receive unit weight); \\textit{Entropy} reweights the never-treated cities to match the treated means of the four covariates exactly (Hainmueller 2012); \\textit{Caliper} restricts both treated and never-treated cities to those whose four covariates jointly fall within $\\pm 1$ standard deviation of the treated mean and re-estimates unweighted.",
      "Panel C reports the means of the four covariates for treated and never-treated cities under each scheme.",
      "City-year controls follow the main city-year table: log population, log GDP, and log registered lawyers in all columns, with log court caseload added only for the government-win-rate specification.",
      "Standard errors clustered by city (TWFE) and obtained from the multiplier bootstrap clustered by city (CS).",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_selection_robustness_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
