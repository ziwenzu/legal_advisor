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
input_file <- file.path(root_dir, "data", "city_year_panel.csv")
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

NEVER_TREATED_SENTINEL <- 0

EVENT_PLOT_LAYOUT <- list(
  "Government Win Rate" = list(ann_y = 0.18, pre_y = 0.18, ylim_hi = 0.20),
  "Appeal Rate" = list(ann_y = 0.10, pre_y = 0.10),
  "Administrative Case Numbers" = list(ann_y = 100, pre_y = 100)
)

stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

read_city_panel <- function(path) {
  dt <- fread(path)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, province_id := .GRP, by = province]
  dt[
    ,
    first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else NEVER_TREATED_SENTINEL,
    by = city_id
  ]
  dt[, first_treat_year := as.numeric(first_treat_year)]
  dt[, ever_treated := as.integer(first_treat_year > 0)]
  dt[, rel_time := fifelse(ever_treated == 1L, year - first_treat_year, NA_real_)]
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

estimate_cs <- function(dt, outcome_name) {
  controls_formula <- as.formula(
    paste("~", paste(preferred_controls(outcome_name), collapse = " + "))
  )
  cs_dt <- as.data.frame(copy(dt))

  att_gt_obj <- att_gt(
    yname = outcome_name,
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

  list(
    att_gt = att_gt_obj,
    overall_att = aggte(att_gt_obj, type = "simple"),
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

extract_cs_coef <- function(cs_obj) {
  ag <- cs_obj$att_gt
  effective_n <- tryCatch(
    nrow(ag$DIDparams$data),
    error = function(e) NA_integer_
  )
  list(
    estimate = cs_obj$overall_att$overall.att,
    se = cs_obj$overall_att$overall.se,
    p_value = 2 * pnorm(abs(cs_obj$overall_att$overall.att / cs_obj$overall_att$overall.se), lower.tail = FALSE),
    n_obs = effective_n,
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

  layout <- EVENT_PLOT_LAYOUT[[outcome_label]]
  ann_x <- 2
  ann_y <- if (!is.null(layout) && !is.null(layout$ann_y)) layout$ann_y else y_range[2] + 0.04 * y_span
  pre_x <- -4.8
  pre_y <- if (!is.null(layout) && !is.null(layout$pre_y)) layout$pre_y else y_range[1] - 0.05 * y_span
  ann_adj <- c(0, 1)
  pre_adj <- c(0, 1)

  ylim_lo <- min(y_range[1] - 0.05 * y_span, ann_y, pre_y) - 0.05 * y_span
  ylim_hi <- max(y_range[2] + 0.08 * y_span, ann_y, pre_y) + 0.05 * y_span
  if (!is.null(layout) && !is.null(layout$ylim_hi)) {
    ylim_hi <- layout$ylim_hi
  }

  pdf(file = file_path, width = 7.4, height = 5.2, family = "serif")
  op <- par(
    bty = "l",
    las = 1,
    tcl = -0.25,
    mar = c(4.6, 5.2, 2.0, 1.0),
    cex.axis = 0.95,
    cex.lab = 1.1
  )
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  plot(
    NA,
    xlim = c(x_range[1] - 0.4, x_range[2] + 0.4),
    ylim = c(ylim_lo, ylim_hi),
    xlab = "Years Since Procurement",
    ylab = y_title,
    main = "",
    xaxt = "n"
  )
  axis(1, at = seq.int(x_range[1], x_range[2], by = 1))
  abline(h = 0, col = "black", lwd = 1)
  abline(v = -0.5, col = "gray55", lty = 2, lwd = 1)

  segments(plot_dt$event_time, plot_dt$ci_lo, plot_dt$event_time, plot_dt$ci_hi,
           col = "black", lwd = 1.5)
  points(plot_dt$event_time, plot_dt$estimate, col = "black", pch = 16, cex = 1.15)

  text(
    x = ann_x,
    y = ann_y,
    labels = sprintf("ATT (CS) = %s\n(SE = %s)", fmt_num(cs_att), fmt_num(cs_se)),
    adj = ann_adj,
    cex = 0.88
  )

  text(
    x = pre_x,
    y = pre_y,
    labels = bquote("Pre-period joint test " * italic(p) * " = " * .(fmt_p(cs_pre_p))),
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
    if (is.na(res$r2)) "" else fmt_num(res$r2)
  })

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
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
    paste("City Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City Fixed Effects &", paste(c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), collapse = " & "), "\\\\"),
    paste("Year Fixed Effects &", paste(c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes"), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:}",
      "Odd columns report the average treatment effect on the treated from the Callaway and Sant'Anna (CS) staggered estimator with never-treated cities as the comparison group;",
      "even columns report Treatment $\\times$ Post from a two-way fixed-effects (TWFE) regression.",
      "All specifications control for log population, log GDP, and log registered lawyers; government-win columns additionally control for log court caseload.",
      "CS observation counts report the estimator's effective sample and can differ from TWFE columns when cities first treated in the initial sample period are excluded.",
      "Standard errors clustered by city.",
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
      list(est = row[["Estimate"]], se = row[["Std. Error"]], p = row[["Pr(>|t|)"]])
    }
  }
  for (term_name in c("gov_lawyer_share", "opp_lawyer_share", "petition_rate")) {
    pulled <- pull(term_name)
    base[[paste0(term_name, "_estimate")]] <- pulled$est
    base[[paste0(term_name, "_se")]] <- pulled$se
    base[[paste0(term_name, "_p")]] <- pulled$p
  }
  base
}

build_lawyer_share_appendix_table <- function(results_list, file_path) {
  column_keys <- c(
    "government_win_rate_baseline", "government_win_rate_lawyer",
    "appeal_rate_baseline", "appeal_rate_lawyer",
    "admin_case_n_baseline", "admin_case_n_lawyer"
  )
  outcome_short <- c(
    "Government Win Rate", "Government Win Rate",
    "Appeal Rate", "Appeal Rate",
    "Administrative Cases", "Administrative Cases"
  )

  fmt_aux <- function(est, se, p) {
    if (is.na(est)) return(c("", ""))
    c(paste0(fmt_num(est), stars(p)), paste0("(", fmt_num(se), ")"))
  }

  coef_row <- sapply(column_keys, function(k) {
    res <- results_list[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(column_keys, function(k) {
    res <- results_list[[k]]
    paste0("(", fmt_num(res$se), ")")
  })

  gov_cells <- lapply(column_keys, function(k) {
    res <- results_list[[k]]
    fmt_aux(res$gov_lawyer_share_estimate, res$gov_lawyer_share_se, res$gov_lawyer_share_p)
  })
  opp_cells <- lapply(column_keys, function(k) {
    res <- results_list[[k]]
    fmt_aux(res$opp_lawyer_share_estimate, res$opp_lawyer_share_se, res$opp_lawyer_share_p)
  })
  pet_cells <- lapply(column_keys, function(k) {
    res <- results_list[[k]]
    fmt_aux(res$petition_rate_estimate, res$petition_rate_se, res$petition_rate_p)
  })

  obs_row <- sapply(column_keys, function(k) fmt_int(results_list[[k]]$n_obs))
  r2_row <- sapply(column_keys, function(k) {
    res <- results_list[[k]]
    if (is.na(res$r2)) "" else fmt_num(res$r2)
  })

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
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
    paste("Government counsel share &", paste(sapply(gov_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(gov_cells, `[`, 2), collapse = " & "), "\\\\"),
    paste("Opposing counsel share &", paste(sapply(opp_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(opp_cells, `[`, 2), collapse = " & "), "\\\\"),
    paste("Petition rate &", paste(sapply(pet_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(pet_cells, `[`, 2), collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("City Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("City Fixed Effects &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Year Fixed Effects &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each pair of columns reports baseline and lawyer-share specifications for one outcome.",
      "Even columns add three contemporaneous city-year controls: the share of administrative cases in which the government appears with counsel, the share in which the opposing party appears with counsel, and the share involving petitioning behaviour.",
      "Baseline city-year controls are log population, log GDP, log registered lawyers, and log court caseload.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
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
    cs_obj <- estimate_cs(copy(dt), outcome_name)

    twfe_coef <- extract_twfe_coef(twfe_main)
    cs_coef <- extract_cs_coef(cs_obj)

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

if (sys.nframe() == 0) {
  main()
}
