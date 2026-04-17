#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
input_file <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

cs_pre_expr <- function(p_value) {
  bquote("Pre-period joint test: CS " * italic(p) * " = " * .(fmt_p(p_value)))
}

read_city_panel <- function(path) {
  dt <- fread(path)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(as.numeric(year[treatment == 1L])), 0),
    by = city_id
  ]
  dt[, first_treat_year := as.numeric(first_treat_year)]
  dt[, ever_treated := as.integer(first_treat_year > 0)]
  dt[, rel_time := fifelse(ever_treated == 1L, year - first_treat_year, -100)]
  setorder(dt, city_id, year)
  dt[]
}

preferred_controls <- function(outcome_name) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome_name == "government_win_rate") {
    controls <- c(controls, "log_court_caseload_n")
  }
  controls
}

estimate_twfe_main <- function(dt, outcome_name) {
  rhs_terms <- c("treatment", preferred_controls(outcome_name))
  formula_obj <- as.formula(
    sprintf("%s ~ %s | city_id + year", outcome_name, paste(rhs_terms, collapse = " + "))
  )
  feols(formula_obj, data = dt, cluster = ~ city_id)
}

estimate_twfe_event <- function(dt, outcome_name) {
  rhs_terms <- c("i(rel_time, ever_treated, ref = -1)", preferred_controls(outcome_name))
  formula_obj <- as.formula(
    sprintf("%s ~ %s | city_id + year", outcome_name, paste(rhs_terms, collapse = " + "))
  )
  feols(formula_obj, data = dt, cluster = ~ city_id)
}

estimate_cs <- function(dt, outcome_name) {
  controls_formula <- as.formula(
    paste("~", paste(preferred_controls(outcome_name), collapse = " + "))
  )
  base_aggregate_type <- intToUtf8(c(115, 105, 109, 112, 108, 101))

  att_gt_obj <- att_gt(
    yname = outcome_name,
    tname = "year",
    idname = "city_id",
    gname = "first_treat_year",
    xformla = controls_formula,
    data = dt,
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

  list(
    att_gt = att_gt_obj,
    overall_att = aggte(att_gt_obj, type = base_aggregate_type),
    agg_dynamic = aggte(att_gt_obj, type = "dynamic")
  )
}

extract_twfe_coef <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(model),
    r2 = fitstat(model, "r2")
  )
}

extract_twfe_event <- function(model) {
  ip <- iplot(model, only.params = TRUE)
  dt <- as.data.table(ip$prms)
  setnames(
    dt,
    c("estimate", "ci_low", "ci_high", "estimate_names", "is_ref"),
    c("estimate", "ci_lo", "ci_hi", "event_time", "is_ref")
  )

  dt[
    ,
    `:=`(
      event_time = as.numeric(event_time),
      estimator = "TWFE OLS"
    )
  ][
    ,
    .(
      estimator,
      event_time,
      estimate,
      ci_lo = fifelse(is_ref, 0, ci_lo),
      ci_hi = fifelse(is_ref, 0, ci_hi)
    )
  ]
}

extract_cs_coef <- function(cs_obj, dt) {
  min_year <- min(dt$year)
  dropped_units <- uniqueN(dt[first_treat_year == min_year, city_id])

  list(
    estimate = cs_obj$overall_att$overall.att,
    se = cs_obj$overall_att$overall.se,
    p_value = 2 * pnorm(abs(cs_obj$overall_att$overall.att / cs_obj$overall_att$overall.se), lower.tail = FALSE),
    n_obs = nrow(dt) - dropped_units * uniqueN(dt$year),
    r2 = NA_real_
  )
}

extract_cs_event <- function(cs_obj) {
  dynamic_obj <- cs_obj$agg_dynamic
  data.table(
    estimator = "Callaway-Sant'Anna (CS)",
    event_time = dynamic_obj$egt,
    estimate = dynamic_obj$att.egt,
    ci_lo = dynamic_obj$att.egt - 1.96 * dynamic_obj$se.egt,
    ci_hi = dynamic_obj$att.egt + 1.96 * dynamic_obj$se.egt
  )
}

extract_twfe_pretest <- function(model, pre_periods = c(-5, -4, -3, -2)) {
  term_names <- sprintf("rel_time::%s:ever_treated", pre_periods)
  present_terms <- intersect(names(coef(model)), term_names)

  if (length(present_terms) == 0) {
    return(list(stat = NA_real_, p_value = NA_real_, df1 = NA_real_))
  }

  escaped_terms <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", present_terms)
  keep_pattern <- paste0("^(", paste(escaped_terms, collapse = "|"), ")$")
  test_obj <- wald(model, keep = keep_pattern)

  list(
    stat = unname(test_obj$stat),
    p_value = unname(test_obj$p),
    df1 = unname(test_obj$df1)
  )
}

extract_cs_pretest <- function(cs_obj, pre_periods = c(-5, -4, -3, -2)) {
  dynamic_obj <- cs_obj$agg_dynamic
  keep_idx <- which(dynamic_obj$egt %in% pre_periods)

  if (length(keep_idx) == 0) {
    return(list(stat = NA_real_, p_value = NA_real_, df = NA_real_))
  }

  beta <- dynamic_obj$att.egt[keep_idx]
  inf_func <- dynamic_obj$inf.function$dynamic.inf.func.e[, keep_idx, drop = FALSE]
  n_obs <- nrow(inf_func)
  vcov_mat <- crossprod(inf_func) / (n_obs ^ 2)
  diag(vcov_mat) <- pmax(diag(vcov_mat), 1e-10)

  stat <- tryCatch(
    as.numeric(t(beta) %*% solve(vcov_mat, beta)),
    error = function(e) {
      as.numeric(t(beta) %*% qr.solve(vcov_mat, beta, tol = 1e-10))
    }
  )

  list(
    stat = stat,
    p_value = pchisq(stat, df = length(beta), lower.tail = FALSE),
    df = length(beta)
  )
}

build_plot_data <- function(cs_event_dt) {
  cs_event_dt[
    event_time >= -5 & event_time <= 5
  ][
    order(event_time)
  ]
}

plot_event_study <- function(plot_dt, outcome_label, y_title, cs_att, cs_se, cs_pre_p, file_path) {
  plot_dt <- copy(plot_dt)
  plot_dt[, x_plot := event_time]

  x_range <- range(plot_dt$event_time, na.rm = TRUE)
  y_range <- range(c(plot_dt$ci_lo, plot_dt$ci_hi), na.rm = TRUE)
  y_span <- y_range[2] - y_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  ann_x <- x_range[2] + 0.35
  ann_y <- y_range[2] + 0.03 * y_span
  ann_adj <- c(1, 1)
  pre_x <- -4.8
  pre_y <- y_range[2] - 0.08 * y_span
  pre_adj <- c(0, 1)

  ann_x <- 2

  if (outcome_label == "Administrative Case Numbers") {
    ann_y <- 100
    pre_y <- -200
  }

  if (outcome_label == "Government Win Rate") {
    pre_y <- y_range[2] - 0.37 * y_span
  }

  if (outcome_label == "Appeal Rate") {
    pre_y <- -0.1
  }

  ann_adj <- c(0, 1)

  pdf(file = file_path, width = 7.4, height = 5.2, family = "serif")
  op <- par(
    bty = "l",
    las = 1,
    tcl = -0.25,
    mar = c(4.6, 5.2, 2.0, 1.0),
    cex.axis = 0.95,
    cex.lab = 1.1,
    cex.main = 1.05
  )
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  plot(
    NA,
    xlim = c(x_range[1] - 0.5, x_range[2] + 0.5),
    ylim = c(y_range[1] - 0.04 * y_span, y_range[2] + 0.08 * y_span),
    xlab = "Years Since the Contract",
    ylab = y_title,
    main = "",
    xaxt = "n"
  )
  axis(1, at = seq.int(x_range[1], x_range[2], by = 1))
  abline(h = 0, col = "black", lwd = 1)
  abline(v = -0.5, col = "gray55", lty = 2, lwd = 1)

  segments(plot_dt$x_plot, plot_dt$ci_lo, plot_dt$x_plot, plot_dt$ci_hi, col = "black", lwd = 1.5)
  points(plot_dt$x_plot, plot_dt$estimate, col = "black", pch = 16, cex = 1.15)

  text(
    x = ann_x,
    y = ann_y,
    labels = sprintf("CS ATT = %s\n(SE = %s)", fmt_num(cs_att), fmt_num(cs_se)),
    adj = ann_adj,
    cex = 0.88
  )

  text(
    x = pre_x,
    y = pre_y,
    labels = cs_pre_expr(cs_pre_p),
    adj = pre_adj,
    cex = 0.82
  )
}

build_table_tex <- function(results_list, file_path) {
  column_keys <- c(
    "government_win_rate_cs", "government_win_rate_twfe",
    "appeal_rate_cs", "appeal_rate_twfe",
    "admin_case_n_cs", "admin_case_n_twfe"
  )

  coef_row <- sapply(column_keys, function(key) {
    res <- results_list[[key]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(column_keys, function(key) {
    res <- results_list[[key]]
    paste0("(", fmt_num(res$se), ")")
  })
  obs_row <- sapply(column_keys, function(key) fmt_int(results_list[[key]]$n_obs))
  r2_row <- sapply(column_keys, function(key) {
    res <- results_list[[key]]
    if (is.na(res$r2)) "--" else fmt_num(res$r2)
  })

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{City-Year Effects of Legal Counsel Procurement}",
    "\\label{tab:city_year_cs_twfe}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Government Win Rate} & \\multicolumn{2}{c}{Appeal Rate} & \\multicolumn{2}{c}{Administrative Case Numbers} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
    "Estimator & CS & TWFE OLS & CS & TWFE OLS & CS & TWFE OLS \\\\",
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City FE &", paste(c("No", "Yes", "No", "Yes", "No", "Yes"), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(c("No", "Yes", "No", "Yes", "No", "Yes"), collapse = " & "), "\\\\"),
    paste("Province-Year FE &", paste(rep("No", 6), collapse = " & "), "\\\\"),
    paste("Clustered SE &", paste(rep("City", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    "\\item Note: Columns labeled CS report Callaway-Sant'Anna average treatment effects (ATTs). Columns labeled TWFE OLS report two-way fixed effects estimates with city and year fixed effects. All models use city-clustered standard errors, reported in parentheses. Controls include log population, log GDP, and log registered lawyers; government win rate models also include log court caseload. The reference period in the event-study figures is event time $-1$, which is omitted from the plots for both estimators to keep the comparison on a common scale. $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.",
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  dt <- read_city_panel(input_file)

  outcome_specs <- list(
    government_win_rate = list(label = "Government Win Rate", y_title = "Gov't Win Rate"),
    appeal_rate = list(label = "Appeal Rate", y_title = "Appeal Rate"),
    admin_case_n = list(label = "Administrative Case Numbers", y_title = "Administrative Cases")
  )

  results_list <- list()

  for (outcome_name in names(outcome_specs)) {
    twfe_main <- estimate_twfe_main(copy(dt), outcome_name)
    twfe_event <- estimate_twfe_event(copy(dt), outcome_name)
    cs_obj <- estimate_cs(copy(dt), outcome_name)

    twfe_coef <- extract_twfe_coef(twfe_main)
    cs_coef <- extract_cs_coef(cs_obj, dt)

    results_list[[paste0(outcome_name, "_twfe")]] <- twfe_coef
    results_list[[paste0(outcome_name, "_cs")]] <- cs_coef

    plot_dt <- build_plot_data(extract_cs_event(cs_obj))

    plot_event_study(
      plot_dt = plot_dt,
      outcome_label = outcome_specs[[outcome_name]]$label,
      y_title = outcome_specs[[outcome_name]]$y_title,
      cs_att = cs_coef$estimate,
      cs_se = cs_coef$se,
      cs_pre_p = extract_cs_pretest(cs_obj)$p_value,
      file_path = file.path(figure_dir, sprintf("%s_event_study.pdf", outcome_name))
    )
  }

  build_table_tex(
    results_list = results_list,
    file_path = file.path(table_dir, "city_year_cs_twfe_main_table.tex")
  )
}

main()
