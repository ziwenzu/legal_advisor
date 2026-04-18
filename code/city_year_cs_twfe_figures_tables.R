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

estimate_twfe_main <- function(dt, outcome_name, extra_controls = character(0)) {
  rhs_terms <- c("treatment", preferred_controls(outcome_name), extra_controls)
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

build_event_study_table <- function(plot_dt, outcome_label, cs_att, cs_se, cs_pre_p,
                                    file_path, file_label, caption) {
  dt <- copy(plot_dt)
  setorder(dt, event_time)
  dt[, se := (ci_hi - estimate) / 1.96]

  body_lines <- vapply(seq_len(nrow(dt)), function(i) {
    row <- dt[i]
    paste(
      sprintf("%d", as.integer(row$event_time)),
      "&", fmt_num(row$estimate),
      "&", fmt_num(row$se),
      "&", fmt_num(row$ci_lo),
      "&", fmt_num(row$ci_hi),
      "\\\\"
    )
  }, character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{tab:%s}", file_label),
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Event Time & Estimate & SE & 95\\% CI Low & 95\\% CI High \\\\",
    "\\midrule",
    body_lines,
    "\\midrule",
    paste0(
      "Average post-period CS ATT & ",
      fmt_num(cs_att),
      " & ", fmt_num(cs_se),
      " & -- & -- \\\\"
    ),
    paste0(
      "Pre-period joint test ($p$) & ",
      fmt_p(cs_pre_p),
      " & -- & -- & -- \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      sprintf("Companion table for the city-year %s event-study figure.", outcome_label),
      "Each row reports the Callaway and Sant'Anna staggered ATT at the indicated event time relative to the city's first procurement year, with influence-function 95\\% confidence intervals.",
      "The Average post-period CS ATT line aggregates the post-period dynamic effects.",
      "The pre-period joint test is the Wald statistic that all pre-treatment ATTs from $-5$ through $-2$ are jointly zero."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
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
    ann_y <- 0.20
    pre_y <- 0.20
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

  ylim_lo <- y_range[1] - 0.04 * y_span
  ylim_hi <- y_range[2] + 0.08 * y_span
  text_y_values <- c(ann_y, pre_y)
  text_y_values <- text_y_values[is.finite(text_y_values)]
  if (length(text_y_values) > 0) {
    ylim_hi <- max(ylim_hi, max(text_y_values) + 0.04 * y_span)
    ylim_lo <- min(ylim_lo, min(text_y_values) - 0.04 * y_span)
  }

  plot(
    NA,
    xlim = c(x_range[1] - 0.5, x_range[2] + 0.5),
    ylim = c(ylim_lo, ylim_hi),
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
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are estimated effects of legal-counsel procurement on city-year administrative outcomes.",
      "The outcomes are the city-year government win rate (columns 1--2), the appeal rate filed by administrative-litigation parties (columns 3--4), and the count of administrative cases brought against the city government (columns 5--6).",
      "Odd-numbered columns report the average treatment effect on the treated from the Callaway and Sant'Anna staggered estimator;",
      "even-numbered columns report the coefficient on Treatment $\\times$ Post from a two-way fixed-effects regression with city and year fixed effects.",
      "All specifications include city-year controls for log population, log GDP, log registered lawyers, and log court caseload.",
      "Cluster-robust standard errors at the city level are in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_lawyer_share_appendix_table <- function(results_list, file_path) {
  column_keys <- c(
    "government_win_rate_baseline",
    "government_win_rate_lawyer",
    "appeal_rate_baseline",
    "appeal_rate_lawyer",
    "admin_case_n_baseline",
    "admin_case_n_lawyer"
  )
  outcome_short <- c(
    "Gov.\\ Win Rate",
    "Gov.\\ Win Rate",
    "Appeal Rate",
    "Appeal Rate",
    "Admin.\\ Cases",
    "Admin.\\ Cases"
  )
  lawyer_yes <- c("", "Yes", "", "Yes", "", "Yes")

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
    "\\caption{City-Year Treatment Effects with and without Lawyer-Presence Controls}",
    "\\label{tab:city_year_lawyer_share_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
    paste("Outcome &", paste(outcome_short, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Government Counsel Share &", paste(lawyer_yes, collapse = " & "), "\\\\"),
    paste("Opposing Counsel Share &", paste(lawyer_yes, collapse = " & "), "\\\\"),
    paste("Petitioning Share &", paste(lawyer_yes, collapse = " & "), "\\\\"),
    paste("City-Year Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on the procurement-treatment interaction Treatment $\\times$ Post from city-year two-way fixed-effects regressions on the administrative-litigation panel.",
      "The dependent variables are the government win rate (columns 1 and 2), the appeal rate filed by administrative-litigation parties (columns 3 and 4), and the count of administrative cases against the city government (columns 5 and 6), each computed from the underlying case-level data.",
      "Even-numbered columns add three contemporaneous covariates: the within-city-year share of administrative cases in which the government appears with counsel, the share in which the opposing party appears with counsel, and the share of cases that involve petitioning behaviour outside the courtroom.",
      "These additions capture the fact that neither pre-procurement governments nor never-procuring control cities operate at zero counsel presence, and that petitioning intensity is a parallel margin that may correlate with case outcomes; absorbing them isolates the residual procurement effect.",
      "All specifications include city and year fixed effects together with city-year controls for log population, log GDP, log registered lawyers, and log court caseload.",
      "Cluster-robust standard errors at the city level are in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

extract_twfe_with_extras <- function(model) {
  base <- extract_twfe_coef(model)
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  pull <- function(term_name) {
    row <- ct[term == term_name]
    if (nrow(row) == 0) {
      list(est = NA_real_, se = NA_real_, p = NA_real_)
    } else {
      list(
        est = row[["Estimate"]],
        se = row[["Std. Error"]],
        p = row[["Pr(>|t|)"]]
      )
    }
  }
  gov <- pull("gov_lawyer_share")
  opp <- pull("opp_lawyer_share")
  base$gov_share_estimate <- gov$est
  base$gov_share_se <- gov$se
  base$gov_share_p <- gov$p
  base$opp_share_estimate <- opp$est
  base$opp_share_se <- opp$se
  base$opp_share_p <- opp$p
  base
}

main <- function() {
  dt <- read_city_panel(input_file)

  outcome_specs <- list(
    government_win_rate = list(label = "Government Win Rate", y_title = "Government Win Rate"),
    appeal_rate = list(label = "Appeal Rate", y_title = "Appeal Rate"),
    admin_case_n = list(label = "Administrative Case Numbers", y_title = "Administrative Case Count")
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
    cs_pre_p <- extract_cs_pretest(cs_obj)$p_value

    plot_event_study(
      plot_dt = plot_dt,
      outcome_label = outcome_specs[[outcome_name]]$label,
      y_title = outcome_specs[[outcome_name]]$y_title,
      cs_att = cs_coef$estimate,
      cs_se = cs_coef$se,
      cs_pre_p = cs_pre_p,
      file_path = file.path(figure_dir, sprintf("%s_event_study.pdf", outcome_name))
    )

    build_event_study_table(
      plot_dt = plot_dt,
      outcome_label = outcome_specs[[outcome_name]]$label,
      cs_att = cs_coef$estimate,
      cs_se = cs_coef$se,
      cs_pre_p = cs_pre_p,
      file_path = file.path(table_dir, sprintf("%s_event_study_table.tex", outcome_name)),
      file_label = sprintf("%s_event_study", outcome_name),
      caption = sprintf(
        "Event-Study Estimates Behind the City-Year %s Figure",
        outcome_specs[[outcome_name]]$label
      )
    )
  }

  build_table_tex(
    results_list = results_list,
    file_path = file.path(table_dir, "city_year_cs_twfe_main_table.tex")
  )

  if (all(c("gov_lawyer_share", "opp_lawyer_share", "petition_rate") %in% names(dt))) {
    appendix_results <- list()
    for (outcome_name in names(outcome_specs)) {
      base_model <- estimate_twfe_main(copy(dt), outcome_name)
      lawyer_model <- estimate_twfe_main(
        copy(dt),
        outcome_name,
        extra_controls = c("gov_lawyer_share", "opp_lawyer_share", "petition_rate")
      )
      appendix_results[[paste0(outcome_name, "_baseline")]] <- extract_twfe_with_extras(base_model)
      appendix_results[[paste0(outcome_name, "_lawyer")]] <- extract_twfe_with_extras(lawyer_model)
    }
    build_lawyer_share_appendix_table(
      results_list = appendix_results,
      file_path = file.path(table_dir, "city_year_lawyer_share_appendix_table.tex")
    )
  }
}

main()
