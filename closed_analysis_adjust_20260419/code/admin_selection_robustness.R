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
  if (outcome == "government_win_rate") {
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
  vars <- c("log_population_10k", "log_gdp", "log_registered_lawyers", "log_court_caseload_n")
  treated_mean <- colMeans(as.matrix(treated[, ..vars]), na.rm = TRUE)
  treated_sd <- apply(as.matrix(treated[, ..vars]), 2, sd, na.rm = TRUE)
  keep <- city_dt$ever_treated == 1L
  for (v in vars) {
    keep <- keep | (
      city_dt$ever_treated == 0L &
      abs(city_dt[[v]] - treated_mean[[v]]) <= sd_cutoff * treated_sd[[v]]
    )
  }
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

estimate_cs <- function(panel, outcome, weights = NULL) {
  controls_formula <- as.formula(paste("~", paste(preferred_controls(outcome), collapse = " + ")))
  cs_dt <- as.data.frame(copy(panel))
  weightsname <- if (is.null(weights)) NULL else weights

  att_gt_obj <- att_gt(
    yname = outcome,
    tname = "year",
    idname = "city_id",
    gname = "first_treat_year",
    xformla = controls_formula,
    data = cs_dt,
    weightsname = weightsname,
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

balance_diff <- function(city_dt, weight_col) {
  vars <- c("log_population_10k", "log_gdp", "log_registered_lawyers", "log_court_caseload_n")
  out <- list()
  for (v in vars) {
    t_vals <- city_dt[ever_treated == 1L][[v]]
    c_vals <- city_dt[ever_treated == 0L][[v]]
    c_w <- city_dt[ever_treated == 0L][[weight_col]]
    t_mean <- mean(t_vals, na.rm = TRUE)
    if (sum(c_w, na.rm = TRUE) > 0) {
      c_mean <- sum(c_vals * c_w, na.rm = TRUE) / sum(c_w, na.rm = TRUE)
    } else {
      c_mean <- mean(c_vals, na.rm = TRUE)
    }
    out[[v]] <- list(treated = t_mean, control = c_mean, diff = t_mean - c_mean)
  }
  out
}

caliper_balance_diff <- function(city_dt, cal_panel) {
  cities_keep <- unique(cal_panel$city_id)
  cal_city_dt <- city_dt[city_id %in% cities_keep]
  cal_city_dt[, raw_w := 1.0]
  balance_diff(cal_city_dt, "raw_w")
}

main <- function() {
  panel <- read_city_panel(city_path)
  cdt <- city_means(panel)
  cdt <- estimate_propensity(cdt)
  cdt <- entropy_balance(cdt, vars = c(
    "log_population_10k", "log_gdp",
    "log_registered_lawyers", "log_court_caseload_n"
  ))
  panel <- attach_weights(panel, cdt)
  cal_panel <- caliper_subset(panel, cdt, sd_cutoff = 0.5)

  outcomes <- list(
    list(key = "government_win_rate", label = "Gov.\\ Win Rate"),
    list(key = "appeal_rate", label = "Appeal Rate"),
    list(key = "admin_case_n", label = "Admin.\\ Cases")
  )
  variants <- c("baseline", "ipw", "eb", "cal")

  twfe_results <- list()
  cs_results <- list()
  for (spec in outcomes) {
    twfe_results[[paste0(spec$key, "_baseline")]] <- estimate_twfe(panel, spec$key)
    twfe_results[[paste0(spec$key, "_ipw")]] <- estimate_twfe(panel, spec$key, weights = "ipw_weight")
    twfe_results[[paste0(spec$key, "_eb")]] <- estimate_twfe(panel, spec$key, weights = "eb_weight")
    twfe_results[[paste0(spec$key, "_cal")]] <- estimate_twfe(cal_panel, spec$key)

    cs_results[[paste0(spec$key, "_baseline")]] <- estimate_cs(panel, spec$key)
    cs_results[[paste0(spec$key, "_ipw")]] <- estimate_cs(panel, spec$key, weights = "ipw_weight")
    cs_results[[paste0(spec$key, "_eb")]] <- estimate_cs(panel, spec$key, weights = "eb_weight")
    cs_results[[paste0(spec$key, "_cal")]] <- estimate_cs(cal_panel, spec$key)
  }

  bal_raw <- balance_diff(
    cdt[, .(city_id, ever_treated,
            log_population_10k, log_gdp,
            log_registered_lawyers, log_court_caseload_n,
            pscore, ipw_weight, eb_weight,
            raw_w = 1.0)],
    "raw_w"
  )
  bal_ipw <- balance_diff(cdt, "ipw_weight")
  bal_eb <- balance_diff(cdt, "eb_weight")
  bal_cal <- caliper_balance_diff(cdt, cal_panel)

  cities_full <- count_cities(panel)
  cities_cal <- count_cities(cal_panel)
  cities_cell <- function(cnt) sprintf("%d / %d", cnt$treated, cnt$never_treated)
  cities_full_str <- cities_cell(cities_full)
  cities_cal_str <- cities_cell(cities_cal)

  outcome_keys <- vapply(outcomes, function(o) o$key, character(1))
  col_keys <- unlist(lapply(outcome_keys, function(o) paste0(o, "_", variants)))

  twfe_coef_row <- sapply(col_keys, function(k) {
    res <- twfe_results[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  twfe_se_row <- sapply(col_keys, function(k) paste0("(", fmt_num(twfe_results[[k]]$se), ")"))
  twfe_obs_row <- sapply(col_keys, function(k) fmt_int(twfe_results[[k]]$n_obs))
  twfe_r2_row <- sapply(col_keys, function(k) fmt_num(twfe_results[[k]]$r2))

  cs_coef_row <- sapply(col_keys, function(k) {
    res <- cs_results[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  cs_se_row <- sapply(col_keys, function(k) paste0("(", fmt_num(cs_results[[k]]$se), ")"))
  cs_obs_row <- sapply(col_keys, function(k) fmt_int(cs_results[[k]]$n_obs))

  cities_cells <- vapply(seq_along(col_keys), function(i) {
    if (variants[((i - 1L) %% 4L) + 1L] == "cal") cities_cal_str else cities_full_str
  }, character(1))

  bal_block <- function() {
    var_labels <- c(
      log_population_10k = "Log Population (10k)",
      log_gdp = "Log GDP",
      log_registered_lawyers = "Log Registered Lawyers",
      log_court_caseload_n = "Log Court Caseload"
    )
    body <- c()
    for (v in names(var_labels)) {
      body <- c(
        body,
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
          "& & &",
          "\\\\"
        )
      )
    }
    body
  }

  variant_header <- paste(rep(c("Baseline", "IPW", "Entropy", "Caliper"), 3), collapse = " & ")
  yes_or_blank_row <- function(values, label) {
    paste0(label, " & ", paste(values, collapse = " & "), " \\\\")
  }

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
    paste(" &", variant_header, "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(twfe_coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(twfe_se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(twfe_obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(twfe_r2_row, collapse = " & "), "\\\\"),
    paste("Cities (treated / never-treated) &", paste(cities_cells, collapse = " & "), "\\\\"),
    "\\midrule",
    "\\multicolumn{13}{l}{\\textit{Panel B. Treatment effect under the same variants (Callaway--Sant'Anna)}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{4}{c}{Government Win Rate} & \\multicolumn{4}{c}{Appeal Rate} & \\multicolumn{4}{c}{Administrative Cases} \\\\",
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13}",
    paste(" &", variant_header, "\\\\"),
    "\\midrule",
    paste("CS overall ATT &", paste(cs_coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(cs_se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(cs_obs_row, collapse = " & "), "\\\\"),
    paste("Cities (treated / never-treated) &", paste(cities_cells, collapse = " & "), "\\\\"),
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
      "\\textit{Baseline} is the unweighted main specification.",
      "\\textit{IPW} weights each never-treated city by its propensity-score odds $\\hat{p}/(1-\\hat{p})$, with $\\hat{p}$ from a logit on the four city-mean covariates and trimmed to $[0.05, 0.95]$; treated cities receive unit weight.",
      "\\textit{Entropy} reweights the never-treated cities to match the treated means of the four covariates exactly (Hainmueller 2012).",
      "\\textit{Caliper} restricts the never-treated cities to those whose four covariates fall within $\\pm 0.5$ standard deviations of the treated mean and re-estimates unweighted; the IPW and entropy weights enter the CS estimator through its \\texttt{weightsname} argument.",
      "Panel C reports the means of the four covariates for treated and never-treated cities under each scheme; under entropy balancing the treated--control difference is zero by construction, while the caliper restriction shifts the never-treated comparison cities toward the treated mean by sample selection.",
      "Two patterns are noteworthy.",
      "First, reweighting roughly doubles the TWFE coefficient on government win rate (from 0.009 in the baseline to 0.020--0.026 under IPW and entropy), narrowing--but not closing--the gap to the CS estimate; the residual difference reflects the negative weights that TWFE places on forbidden comparisons in staggered designs.",
      "Second, the CS coefficients on government win rate, appeal rate, and administrative-case volume are stable across the baseline, IPW, and entropy variants, indicating that the headline CS estimates are not driven by selection on the four observed city-level covariates.",
      "The caliper variant drops a large share of never-treated cities (from 65 to 39), which inflates the standard errors of the CS estimates and renders the government-win-rate coefficient insignificant despite the same point estimate sign.",
      "City-year controls: log population, log GDP, log registered lawyers, and log court caseload (the last enters only for the government-win-rate specification, matching the main city-year table).",
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
