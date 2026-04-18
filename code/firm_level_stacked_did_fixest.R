#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
input_file <- Sys.getenv(
  "FIRM_LEVEL_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "firm_level.csv")
)
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")
output_tag <- Sys.getenv("FIRM_LEVEL_OUTPUT_TAG", unset = "")
control_note <- Sys.getenv(
  "FIRM_LEVEL_CONTROL_NOTE",
  unset = "matched runner-up firms within the same stack"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

build_output_name <- function(stem, ext) {
  if (nzchar(output_tag)) {
    sprintf("%s_%s.%s", output_tag, stem, ext)
  } else {
    sprintf("%s.%s", stem, ext)
  }
}

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

single_pre_expr <- function(p_value) {
  bquote("Pre-period joint test: " * italic(p) * " = " * .(fmt_p(p_value)))
}

client_mix_pre_expr <- function(enterprise_p, personal_p) {
  bquote(
    atop(
      "Pre-period joint tests",
      "Enterprise " * italic(p) * " = " * .(fmt_p(enterprise_p)) * "; Personal " * italic(p) * " = " * .(fmt_p(personal_p))
    )
  )
}

read_firm_panel <- function(path) {
  dt <- fread(path)

  dt[, stack_firm_fe := sprintf("%s__%s", stack_id, firm_id)]
  dt[, stack_year_fe := sprintf("%s__%s", stack_id, year)]

  dt[, event_time_window := fifelse(event_time < -5, NA_real_, fifelse(event_time > 5, NA_real_, event_time))]

  if ("enterprise_case_n" %in% names(dt)) {
    dt[, log_enterprise_case_n := log1p(pmax(0, enterprise_case_n))]
  }
  if ("personal_case_n" %in% names(dt)) {
    dt[, log_personal_case_n := log1p(pmax(0, personal_case_n))]
  }

  setorder(dt, stack_id, firm_id, year)
  dt[]
}

estimate_main_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  if (!is.null(sample_filter)) {
    work_dt <- work_dt[eval(sample_filter)]
  }

  formula_obj <- as.formula(
    sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome_name)
  )

  feols(
    formula_obj,
    data = work_dt,
    cluster = ~ stack_id + firm_id
  )
}

estimate_event_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  base_filter <- quote(!is.na(event_time_window))
  if (is.null(sample_filter)) {
    work_dt <- work_dt[eval(base_filter)]
  } else {
    work_dt <- work_dt[eval(base_filter) & eval(sample_filter)]
  }

  formula_obj <- as.formula(
    sprintf("%s ~ i(event_time_window, treated_firm, ref = -1) | stack_firm_fe + stack_year_fe", outcome_name)
  )

  feols(
    formula_obj,
    data = work_dt,
    cluster = ~ stack_id + firm_id
  )
}

extract_main_coef <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  list(
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(model),
    r2 = fitstat(model, "r2")
  )
}

extract_event_dt <- function(model) {
  ip <- iplot(model, only.params = TRUE)
  dt <- as.data.table(ip$prms)
  dt[
    ,
    .(
      event_time = as.numeric(estimate_names),
      estimate,
      ci_lo = fifelse(is_ref, 0, ci_low),
      ci_hi = fifelse(is_ref, 0, ci_high),
      is_ref
    )
  ][order(event_time)]
}

extract_pretest <- function(model, pre_periods = c(-5, -4, -3, -2)) {
  term_names <- sprintf("event_time_window::%s:treated_firm", pre_periods)
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

plot_event_study <- function(event_dt, outcome_label, y_title, main_effect, main_se, pre_p, file_path) {
  x_range <- range(event_dt$event_time, na.rm = TRUE)
  y_range <- range(c(event_dt$ci_lo, event_dt$ci_hi), na.rm = TRUE)
  y_span <- y_range[2] - y_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  ann_x <- 2
  ann_y <- y_range[2] + 0.03 * y_span
  ann_adj <- c(0, 1)
  pre_x <- -4.8
  pre_y <- y_range[2] - 0.08 * y_span
  pre_adj <- c(0, 1)

  if (outcome_label == "Log Civil Cases") {
    ann_y <- 0.3
  }
  if (outcome_label == "Civil Win Rate") {
    ann_y <- 0.20
    pre_y <- 0.20
  }
  if (outcome_label == "Civil Fee Win Rate") {
    ann_y <- 0.30
    pre_y <- 0.30
  }
  if (outcome_label == "Log Firm Size") {
    ann_y <- y_range[2] - 0.08 * y_span
  }
  if (outcome_label == "Average Hearing Time") {
    ann_y <- 20
    pre_y <- -20
  }

  if (outcome_label == "Log Personal Cases") {
    pre_y <- -0.2
  }

  pdf(file = file_path, width = 7.2, height = 5.0, family = "serif")
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

  ylim_lo <- y_range[1] - 0.05 * y_span
  ylim_hi <- y_range[2] + 0.08 * y_span
  text_y_values <- c(ann_y, pre_y)
  text_y_values <- text_y_values[is.finite(text_y_values)]
  if (length(text_y_values) > 0) {
    ylim_hi <- max(ylim_hi, max(text_y_values) + 0.04 * y_span)
    ylim_lo <- min(ylim_lo, min(text_y_values) - 0.04 * y_span)
  }

  plot(
    NA,
    xlim = c(x_range[1] - 0.4, x_range[2] + 0.4),
    ylim = c(ylim_lo, ylim_hi),
    xlab = "Years Since the Contract",
    ylab = y_title,
    main = "",
    xaxt = "n"
  )
  axis(1, at = seq.int(x_range[1], x_range[2], by = 1))
  abline(h = 0, col = "black", lwd = 1)
  abline(v = -0.5, col = "gray55", lty = 2, lwd = 1)

  segments(event_dt$event_time, event_dt$ci_lo, event_dt$event_time, event_dt$ci_hi, col = "black", lwd = 1.5)
  points(event_dt$event_time, event_dt$estimate, col = "black", pch = 16, cex = 1.15)

  text(
    x = ann_x,
    y = ann_y,
    labels = sprintf("Main DID = %s\n(SE = %s)", fmt_num(main_effect), fmt_num(main_se)),
    adj = ann_adj,
    cex = 0.88
  )

  text(
    x = pre_x,
    y = pre_y,
    labels = single_pre_expr(pre_p),
    adj = pre_adj,
    cex = 0.82
  )
}

plot_client_mix_comparison <- function(enterprise_dt, personal_dt, enterprise_effect, enterprise_se, personal_effect, personal_se, enterprise_pre_p, personal_pre_p, file_path) {
  enterprise_dt <- copy(enterprise_dt)[, series := "Enterprise"]
  personal_dt <- copy(personal_dt)[, series := "Personal"]
  plot_dt <- rbindlist(list(enterprise_dt, personal_dt), use.names = TRUE)

  x_range <- range(plot_dt$event_time, na.rm = TRUE)
  y_range <- range(c(plot_dt$ci_lo, plot_dt$ci_hi), na.rm = TRUE)
  y_span <- y_range[2] - y_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  plot_dt[series == "Enterprise", x_plot := event_time - 0.08]
  plot_dt[series == "Personal", x_plot := event_time + 0.08]

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
    ylim = c(y_range[1] - 0.06 * y_span, y_range[2] + 0.12 * y_span),
    xlab = "Years Since the Contract",
    ylab = "Log Cases",
    main = "",
    xaxt = "n"
  )
  axis(1, at = seq.int(x_range[1], x_range[2], by = 1))
  abline(h = 0, col = "black", lwd = 1)
  abline(v = -0.5, col = "gray55", lty = 2, lwd = 1)

  style_map <- list(
    Enterprise = list(pch = 16),
    Personal = list(pch = 17)
  )

  for (series_name in c("Enterprise", "Personal")) {
    sub <- plot_dt[series == series_name][order(event_time)]
    segments(sub$x_plot, sub$ci_lo, sub$x_plot, sub$ci_hi, col = "black", lwd = 1.5)
    points(sub$x_plot, sub$estimate, col = "black", pch = style_map[[series_name]]$pch, cex = 1.15)
  }

  legend(
    "topleft",
    legend = c(
      sprintf("Enterprise (DID = %s, SE = %s)", fmt_num(enterprise_effect), fmt_num(enterprise_se)),
      sprintf("Personal (DID = %s, SE = %s)", fmt_num(personal_effect), fmt_num(personal_se))
    ),
    col = c("black", "black"),
    pch = c(16, 17),
    pt.cex = 1.1,
    bty = "n"
  )

  text(
    x = -4.8,
    y = 0.20,
    labels = client_mix_pre_expr(enterprise_pre_p, personal_pre_p),
    adj = c(0, 0),
    cex = 0.82
  )
}

build_event_study_table <- function(event_dt, outcome_label, y_title, main_effect, main_se, main_p, pre_p, file_path,
                                    file_label, caption) {
  dt <- copy(event_dt)
  setorder(dt, event_time)
  dt[, se := (ci_hi - estimate) / 1.96]

  body_lines <- vapply(seq_len(nrow(dt)), function(i) {
    row <- dt[i]
    if (isTRUE(row$is_ref)) {
      paste(
        sprintf("%d", as.integer(row$event_time)),
        "& 0.000 & -- & -- & -- (reference) \\\\"
      )
    } else {
      paste(
        sprintf("%d", as.integer(row$event_time)),
        "&", fmt_num(row$estimate),
        "&", fmt_num(row$se),
        "&", fmt_num(row$ci_lo),
        "&", fmt_num(row$ci_hi),
        "\\\\"
      )
    }
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
      "Average post-period effect & ",
      paste0(fmt_num(main_effect), stars(main_p)),
      " & ", fmt_num(main_se),
      " & -- & -- \\\\"
    ),
    paste0(
      "Pre-period joint test ($p$) & ",
      fmt_p(pre_p),
      " & -- & -- & -- \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      sprintf("Companion table for the %s event-study figure.", outcome_label),
      "Each row reports the difference-in-differences coefficient at the indicated event time, with the 95\\% confidence interval obtained from two-way cluster-robust standard errors by stack and firm.",
      "Event time $-1$ is the reference period.",
      "The Average post-period effect line reproduces the static Winner $\\times$ Post coefficient.",
      "The pre-period joint test is the Wald statistic that all event-time leads from $-5$ through $-2$ are jointly zero.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_client_mix_event_study_table <- function(enterprise_dt, personal_dt,
                                               enterprise_pack, personal_pack,
                                               file_path) {
  combined <- merge(
    enterprise_dt[, .(event_time, est_e = estimate, se_e = (ci_hi - estimate) / 1.96, ref_e = is_ref)],
    personal_dt[, .(event_time, est_p = estimate, se_p = (ci_hi - estimate) / 1.96, ref_p = is_ref)],
    by = "event_time",
    all = TRUE
  )
  setorder(combined, event_time)

  body_lines <- vapply(seq_len(nrow(combined)), function(i) {
    row <- combined[i]
    e_estimate <- if (isTRUE(row$ref_e)) "0.000" else fmt_num(row$est_e)
    e_se <- if (isTRUE(row$ref_e)) "--" else fmt_num(row$se_e)
    p_estimate <- if (isTRUE(row$ref_p)) "0.000" else fmt_num(row$est_p)
    p_se <- if (isTRUE(row$ref_p)) "--" else fmt_num(row$se_p)
    paste(
      sprintf("%d", as.integer(row$event_time)),
      "&", e_estimate, "&", e_se, "&", p_estimate, "&", p_se, "\\\\"
    )
  }, character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Event-Study Estimates Behind the Client-Mix Comparison Figure}",
    "\\label{tab:firm_level_client_mix_event_study}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{log(Enterprise Cases + 1)} & \\multicolumn{2}{c}{log(Personal Cases + 1)} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Event Time & Estimate & SE & Estimate & SE \\\\",
    "\\midrule",
    body_lines,
    "\\midrule",
    paste0(
      "Average post-period effect & ",
      paste0(fmt_num(enterprise_pack$coef$estimate), stars(enterprise_pack$coef$p_value)),
      " & ", fmt_num(enterprise_pack$coef$se),
      " & ", paste0(fmt_num(personal_pack$coef$estimate), stars(personal_pack$coef$p_value)),
      " & ", fmt_num(personal_pack$coef$se), " \\\\"
    ),
    paste0(
      "Pre-period joint test ($p$) & ",
      fmt_p(enterprise_pack$pre$p_value),
      " & -- & ",
      fmt_p(personal_pack$pre$p_value),
      " & -- \\\\"
    ),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Companion table for the firm-level client-mix event-study figure.",
      "Each row reports the difference-in-differences coefficient at the indicated event time for the enterprise-case (log) outcome and for the personal-case (log) outcome.",
      "Event time $-1$ is the reference period; standard errors are two-way cluster-robust by stack and firm.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_main_table <- function(results_list, file_path) {
  column_keys <- c(
    "civil_win_rate_mean",
    "avg_filing_to_hearing_days",
    "civil_win_rate_fee_mean"
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
  r2_row <- sapply(column_keys, function(key) fmt_num(results_list[[key]]$r2))

  sample_row <- c(
    "Decisive civil cases",
    "Civil cases",
    "Decisive cases with fee share"
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Stacked DID Estimates for Firm-Year Civil Litigation Outcomes}",
    "\\label{tab:firm_level_stacked_did}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "Outcome & Civil Win Rate & Avg. Hearing Days & Fee-Based Win Rate \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Firm FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are stacked difference-in-differences coefficients on Winner $\\times$ Post estimated from the firm-year panel.",
      sprintf("The treatment group is procurement winners and the control group is %s.", control_note),
      "Column 1 uses firm-year mean win rates among decisive civil cases.",
      "Column 2 uses the firm-year mean filing-to-hearing time, in days, across all civil cases.",
      "Column 3 uses the firm-year mean fee-based win rate among decisive cases with observed fee allocation.",
      "All specifications include stack $\\times$ firm and stack $\\times$ year fixed effects.",
      "Two-way cluster-robust standard errors by stack and firm appear in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_mechanism_table <- function(results_list, file_path) {
  column_keys <- c(
    "log_enterprise_case_n",
    "log_personal_case_n"
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
  r2_row <- sapply(column_keys, function(key) fmt_num(results_list[[key]]$r2))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Procurement Wins Reallocate Firm Caseload Toward Enterprise Clients}",
    "\\label{tab:firm_level_client_mix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    "Outcome & log(Enterprise Cases + 1) & log(Personal Cases + 1) \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Firm FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are stacked difference-in-differences coefficients on Winner $\\times$ Post estimated from the firm-year panel.",
      sprintf("The treatment group is procurement winners and the control group is %s.", control_note),
      "Outcomes are the natural log of one plus the number of civil cases the firm represents in a given year, separately for enterprise clients (column 1) and individual clients (column 2).",
      "All specifications include stack $\\times$ firm and stack $\\times$ year fixed effects.",
      "Two-way cluster-robust standard errors by stack and firm appear in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  dt <- read_firm_panel(input_file)

  model_specs <- list(
    civil_win_rate_mean = list(
      label = "Civil Win Rate",
      y_title = "Firm Win Rate (Decisive Civil Cases)",
      sample_filter = quote(civil_decisive_case_n > 0)
    ),
    avg_filing_to_hearing_days = list(
      label = "Average Hearing Time",
      y_title = "Average Filing-to-Hearing Days",
      sample_filter = quote(civil_case_n > 0)
    )
  )

  results_list <- list()

  for (outcome_name in names(model_specs)) {
    spec <- model_specs[[outcome_name]]

    main_model <- estimate_main_model(
      dt = dt,
      outcome_name = outcome_name,
      sample_filter = spec$sample_filter
    )

    event_model <- estimate_event_model(
      dt = dt,
      outcome_name = outcome_name,
      sample_filter = spec$sample_filter
    )

    main_coef <- extract_main_coef(main_model)
    results_list[[outcome_name]] <- main_coef
    pre_test <- extract_pretest(event_model)

    plot_event_study(
      event_dt = extract_event_dt(event_model),
      outcome_label = spec$label,
      y_title = spec$y_title,
      main_effect = main_coef$estimate,
      main_se = main_coef$se,
      pre_p = pre_test$p_value,
      file_path = file.path(
        figure_dir,
        build_output_name(sprintf("firm_level_%s_event_study", outcome_name), "pdf")
      )
    )

    build_event_study_table(
      event_dt = extract_event_dt(event_model),
      outcome_label = spec$label,
      y_title = spec$y_title,
      main_effect = main_coef$estimate,
      main_se = main_coef$se,
      main_p = main_coef$p_value,
      pre_p = pre_test$p_value,
      file_path = file.path(
        table_dir,
        build_output_name(sprintf("firm_level_%s_event_study_table", outcome_name), "tex")
      ),
      file_label = sprintf("firm_level_%s_event_study", outcome_name),
      caption = sprintf(
        "Event-Study Estimates Behind the Firm-Level %s Figure",
        spec$label
      )
    )
  }

  fee_spec <- list(
    label = "Civil Fee Win Rate",
    y_title = "Firm Fee-Based Win Rate",
    sample_filter = quote(civil_fee_decisive_case_n > 0)
  )
  fee_model <- estimate_main_model(
    dt = dt,
    outcome_name = "civil_win_rate_fee_mean",
    sample_filter = fee_spec$sample_filter
  )
  fee_event_model <- estimate_event_model(
    dt = dt,
    outcome_name = "civil_win_rate_fee_mean",
    sample_filter = fee_spec$sample_filter
  )
  fee_result <- extract_main_coef(fee_model)
  fee_pretest <- extract_pretest(fee_event_model)
  results_list[["civil_win_rate_fee_mean"]] <- fee_result

  plot_event_study(
    event_dt = extract_event_dt(fee_event_model),
    outcome_label = fee_spec$label,
    y_title = fee_spec$y_title,
    main_effect = fee_result$estimate,
    main_se = fee_result$se,
    pre_p = fee_pretest$p_value,
    file_path = file.path(
      figure_dir,
      build_output_name("firm_level_civil_fee_win_rate_event_study", "pdf")
    )
  )

  build_event_study_table(
    event_dt = extract_event_dt(fee_event_model),
    outcome_label = fee_spec$label,
    y_title = fee_spec$y_title,
    main_effect = fee_result$estimate,
    main_se = fee_result$se,
    main_p = fee_result$p_value,
    pre_p = fee_pretest$p_value,
    file_path = file.path(
      table_dir,
      build_output_name("firm_level_civil_fee_win_rate_event_study_table", "tex")
    ),
    file_label = "firm_level_civil_fee_win_rate_event_study",
    caption = "Event-Study Estimates Behind the Firm-Level Fee-Based Win-Rate Figure"
  )

  build_main_table(
    results_list = results_list,
    file_path = file.path(table_dir, build_output_name("firm_level_stacked_did_main_table", "tex"))
  )

  if (all(c("log_enterprise_case_n", "log_personal_case_n") %in% names(dt))) {
    client_mix_filter <- quote(civil_case_n > 0)
    client_mix_results <- list()

    for (mix_outcome in c("log_enterprise_case_n", "log_personal_case_n")) {
      mix_main <- estimate_main_model(
        dt = dt,
        outcome_name = mix_outcome,
        sample_filter = client_mix_filter
      )
      mix_event <- estimate_event_model(
        dt = dt,
        outcome_name = mix_outcome,
        sample_filter = client_mix_filter
      )
      client_mix_results[[mix_outcome]] <- list(
        coef = extract_main_coef(mix_main),
        event_dt = extract_event_dt(mix_event),
        pre = extract_pretest(mix_event)
      )
    }

    enterprise_pack <- client_mix_results[["log_enterprise_case_n"]]
    personal_pack <- client_mix_results[["log_personal_case_n"]]

    plot_client_mix_comparison(
      enterprise_dt = enterprise_pack$event_dt,
      personal_dt = personal_pack$event_dt,
      enterprise_effect = enterprise_pack$coef$estimate,
      enterprise_se = enterprise_pack$coef$se,
      personal_effect = personal_pack$coef$estimate,
      personal_se = personal_pack$coef$se,
      enterprise_pre_p = enterprise_pack$pre$p_value,
      personal_pre_p = personal_pack$pre$p_value,
      file_path = file.path(
        figure_dir,
        build_output_name("firm_level_client_mix_event_study", "pdf")
      )
    )

    build_mechanism_table(
      results_list = list(
        log_enterprise_case_n = enterprise_pack$coef,
        log_personal_case_n = personal_pack$coef
      ),
      file_path = file.path(
        table_dir,
        build_output_name("firm_level_client_mix_mechanism_table", "tex")
      )
    )

    build_client_mix_event_study_table(
      enterprise_dt = enterprise_pack$event_dt,
      personal_dt = personal_pack$event_dt,
      enterprise_pack = enterprise_pack,
      personal_pack = personal_pack,
      file_path = file.path(
        table_dir,
        build_output_name("firm_level_client_mix_event_study_table", "tex")
      )
    )
  }
}

main()
