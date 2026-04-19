#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
city_path <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

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

read_city_panel <- function(path) {
  dt <- fread(path)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  dt[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(year[treatment == 1L]), NA_integer_),
    by = city_id
  ]
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

estimate <- function(panel, outcome, weights = NULL) {
  rhs <- "treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n"
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

main <- function() {
  panel <- read_city_panel(city_path)
  cdt <- city_means(panel)
  cdt <- estimate_propensity(cdt)
  cdt <- entropy_balance(cdt, vars = c(
    "log_population_10k", "log_gdp",
    "log_registered_lawyers", "log_court_caseload_n"
  ))
  panel <- attach_weights(panel, cdt)

  outcomes <- list(
    list(key = "government_win_rate", label = "Gov.\\ Win Rate"),
    list(key = "appeal_rate", label = "Appeal Rate"),
    list(key = "admin_case_n", label = "Admin.\\ Cases")
  )

  results <- list()
  for (spec in outcomes) {
    results[[paste0(spec$key, "_baseline")]] <- estimate(panel, spec$key)
    results[[paste0(spec$key, "_ipw")]] <- estimate(panel, spec$key, weights = "ipw_weight")
    results[[paste0(spec$key, "_eb")]] <- estimate(panel, spec$key, weights = "eb_weight")
    cal_panel <- caliper_subset(panel, cdt, sd_cutoff = 0.5)
    results[[paste0(spec$key, "_cal")]] <- estimate(cal_panel, spec$key)
  }

  bal_raw <- balance_diff(cdt[, .(city_id, ever_treated,
                                  log_population_10k, log_gdp,
                                  log_registered_lawyers, log_court_caseload_n,
                                  pscore, ipw_weight, eb_weight,
                                  raw_w = 1.0)],
                          "raw_w")
  bal_ipw <- balance_diff(cdt, "ipw_weight")
  bal_eb <- balance_diff(cdt, "eb_weight")

  col_keys <- unlist(lapply(c("government_win_rate", "appeal_rate", "admin_case_n"),
                            function(o) paste0(o, c("_baseline", "_ipw", "_eb", "_cal"))))
  outcome_cells <- rep(c("Gov.\\ Win Rate", "Appeal Rate", "Admin.\\ Cases"), each = 4)
  spec_cells <- rep(c("Baseline", "IPW", "Entropy", "Caliper"), 3)

  coef_row <- sapply(col_keys, function(k) {
    res <- results[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(col_keys, function(k) paste0("(", fmt_num(results[[k]]$se), ")"))
  obs_row <- sapply(col_keys, function(k) fmt_int(results[[k]]$n_obs))
  r2_row <- sapply(col_keys, function(k) fmt_num(results[[k]]$r2))

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
          "\\\\"
        )
      )
    }
    body
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
    "\\multicolumn{13}{l}{\\textit{Panel A. Treatment effect under alternative weighting and sample restrictions}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{4}{c}{Government Win Rate} & \\multicolumn{4}{c}{Appeal Rate} & \\multicolumn{4}{c}{Administrative Cases} \\\\",
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13}",
    paste(" &", paste(rep(c("Baseline", "IPW", "Entropy", "Caliper"), 3), collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    "\\midrule",
    "\\multicolumn{13}{l}{\\textit{Panel B. Covariate balance between treated and never-treated cities}} \\\\",
    "\\addlinespace",
    " & \\multicolumn{3}{c}{Unweighted} & \\multicolumn{2}{c}{IPW-weighted} & \\multicolumn{2}{c}{Entropy-weighted} & \\multicolumn{4}{c}{} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
    " & Treated & Control & Diff & Control & Diff & Control & Diff & & & & \\\\",
    "\\midrule",
    bal_block(),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:} Panel A reports Treatment $\\times$ Post from city-year two-way fixed-effects regressions under four sample-or-weight variants.",
      "IPW weights each never-treated city by its propensity-score odds $\\hat{p}/(1-\\hat{p})$, with $\\hat{p}$ estimated by logit on the four city-mean covariates in Panel B and trimmed to $[0.05, 0.95]$; treated cities receive unit weight.",
      "Entropy reweights the never-treated cities to match the treated means of the four covariates exactly (Hainmueller 2012).",
      "Caliper restricts the never-treated cities to those whose four covariates fall within $\\pm 0.5$ standard deviations of the treated mean and re-estimates unweighted.",
      "Panel B reports the means of the four covariates for treated and never-treated cities under each weighting scheme.",
      "Panel A controls: log population, log GDP, log registered lawyers, log court caseload.",
      "Standard errors clustered by city.",
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
