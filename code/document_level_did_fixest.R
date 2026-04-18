#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
input_file <- Sys.getenv(
  "DOCUMENT_LEVEL_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "document_level_winner_vs_loser_clean.csv")
)
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")
output_tag <- Sys.getenv("DOCUMENT_LEVEL_OUTPUT_TAG", unset = "")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

setFixest_nthreads(0)

build_output_name <- function(stem, ext) {
  if (nzchar(output_tag)) {
    sprintf("%s_%s.%s", output_tag, stem, ext)
  } else {
    sprintf("%s.%s", stem, ext)
  }
}

stars <- function(p_value) {
  p_value <- p_value[[1]]
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  x <- x[[1]]
  if (is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  x <- x[[1]]
  if (is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p_value) {
  p_value <- p_value[[1]]
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

single_pre_expr <- function(p_value) {
  bquote("Pre-period joint test: " * italic(p) * " = " * .(fmt_p(p_value)))
}

read_document_panel <- function(path) {
  dt <- fread(path)

  dt[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  dt[, court_year_fe := sprintf("%s__%s", court, year)]
  dt[, cause_side_fe := sprintf("%s__%s", cause, side)]
  dt[, event_time_window := fifelse(event_time < -5, NA_real_, fifelse(event_time > 5, NA_real_, event_time))]

  dt[, plaintiff_party_is_entity := as.integer(plaintiff_party_is_entity)]
  dt[, defendant_party_is_entity := as.integer(defendant_party_is_entity)]
  dt[, opponent_has_lawyer := as.integer(opponent_has_lawyer)]
  dt[, case_decisive := as.integer(case_decisive)]

  dt[, lawyer_female := fifelse(lawyer_gender == "女", 1L, 0L, na = NA_integer_)]
  dt[, lawyer_ccp_bin := fifelse(lawyer_ccp == 1, 1L, 0L, na = NA_integer_)]
  dt[, lawyer_high_edu := fifelse(lawyer_edu %chin% c("master", "PhD"), 1L, 0L, na = NA_integer_)]
  dt[, lawyer_gender_group := fifelse(is.na(lawyer_female), "unknown", fifelse(lawyer_female == 1L, "female", "male"))]
  dt[, lawyer_ccp_group := fifelse(is.na(lawyer_ccp_bin), "unknown", fifelse(lawyer_ccp_bin == 1L, "ccp", "nonccp"))]
  dt[, lawyer_edu_group := fifelse(is.na(lawyer_high_edu), "unknown", fifelse(lawyer_high_edu == 1L, "highedu", "baselineedu"))]
  practice_mean <- mean(dt$lawyer_practice_years, na.rm = TRUE)
  practice_sd <- sd(dt$lawyer_practice_years, na.rm = TRUE)
  dt[, lawyer_practice_years_std := (lawyer_practice_years - practice_mean) / practice_sd]
  dt[is.na(lawyer_practice_years_std), lawyer_practice_years_std := 0]

  dt[, year_gender_fe := sprintf("%s__%s", year, lawyer_gender_group)]
  dt[, year_ccp_fe := sprintf("%s__%s", year, lawyer_ccp_group)]
  dt[, year_edu_fe := sprintf("%s__%s", year, lawyer_edu_group)]

  setorder(dt, stack_id, firm_id, year, case_uid)
  dt[]
}

control_rhs <- paste(
  "opponent_has_lawyer",
  "plaintiff_party_is_entity",
  "defendant_party_is_entity",
  "lawyer_practice_years_std",
  sep = " + "
)

build_formula <- function(outcome_name, rhs_terms, fe_terms) {
  as.formula(sprintf("%s ~ %s | %s", outcome_name, rhs_terms, fe_terms))
}

estimate_model <- function(dt, outcome_name, sample_filter = NULL, fe_variant = c("main", "court_year")) {
  fe_variant <- match.arg(fe_variant)
  work_dt <- copy(dt)

  keep_mask <- !is.na(work_dt[[outcome_name]])
  if (!is.null(sample_filter)) {
    keep_mask <- keep_mask & work_dt[, eval(sample_filter)]
  }
  work_dt <- work_dt[keep_mask]

  fe_terms <- if (fe_variant == "main") {
    "firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe + year_ccp_fe + year_edu_fe"
  } else {
    "firm_id + court_year_fe + cause_side_fe + year_gender_fe + year_ccp_fe + year_edu_fe"
  }

  cluster_terms <- if (fe_variant == "main") {
    ~ firm_id + stack_id
  } else {
    ~ firm_id + court
  }

  formula_obj <- build_formula(
    outcome_name = outcome_name,
    rhs_terms = paste("did_treatment", control_rhs, sep = " + "),
    fe_terms = fe_terms
  )

  feols(formula_obj, data = work_dt, cluster = cluster_terms)
}

estimate_event_model <- function(dt, outcome_name, sample_filter = NULL, fe_variant = c("main", "court_year")) {
  fe_variant <- match.arg(fe_variant)
  work_dt <- copy(dt)

  keep_mask <- !is.na(work_dt[[outcome_name]]) & !is.na(work_dt$event_time_window)
  if (!is.null(sample_filter)) {
    keep_mask <- keep_mask & work_dt[, eval(sample_filter)]
  }
  work_dt <- work_dt[keep_mask]

  fe_terms <- if (fe_variant == "main") {
    "firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe + year_ccp_fe + year_edu_fe"
  } else {
    "firm_id + court_year_fe + cause_side_fe + year_gender_fe + year_ccp_fe + year_edu_fe"
  }

  cluster_terms <- if (fe_variant == "main") {
    ~ firm_id + stack_id
  } else {
    ~ firm_id + court
  }

  formula_obj <- build_formula(
    outcome_name = outcome_name,
    rhs_terms = paste("i(event_time_window, treated_firm, ref = -1)", control_rhs, sep = " + "),
    fe_terms = fe_terms
  )

  feols(formula_obj, data = work_dt, cluster = cluster_terms)
}

estimate_attribute_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  keep_mask <- !is.na(work_dt[[outcome_name]]) &
    !is.na(work_dt$lawyer_gender) &
    !is.na(work_dt$lawyer_practice_years_std)
  if (!is.null(sample_filter)) {
    keep_mask <- keep_mask & work_dt[, eval(sample_filter)]
  }
  work_dt <- work_dt[keep_mask]

  rhs_terms <- paste(
    "did_treatment",
    "did_treatment:lawyer_ccp_bin",
    "did_treatment:lawyer_high_edu",
    "lawyer_practice_years_std",
    "did_treatment:lawyer_practice_years_std",
    control_rhs,
    sep = " + "
  )

  formula_obj <- build_formula(
    outcome_name = outcome_name,
    rhs_terms = rhs_terms,
    fe_terms = "firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe + year_ccp_fe + year_edu_fe"
  )

  feols(formula_obj, data = work_dt, cluster = ~ firm_id + stack_id)
}

estimate_single_attribute_model <- function(dt, outcome_name, attribute_term, sample_filter = NULL, is_continuous = FALSE) {
  work_dt <- copy(dt)

  keep_mask <- !is.na(work_dt[[outcome_name]]) & !is.na(work_dt[[attribute_term]])
  if (!is.null(sample_filter)) {
    keep_mask <- keep_mask & work_dt[, eval(sample_filter)]
  }
  work_dt <- work_dt[keep_mask]

  rhs_terms <- c("did_treatment")
  if (is_continuous) {
    rhs_terms <- c(rhs_terms, attribute_term)
  }
  rhs_terms <- c(rhs_terms, sprintf("did_treatment:%s", attribute_term), control_rhs)

  formula_obj <- build_formula(
    outcome_name = outcome_name,
    rhs_terms = paste(rhs_terms, collapse = " + "),
    fe_terms = "firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe + year_ccp_fe + year_edu_fe"
  )

  feols(formula_obj, data = work_dt, cluster = ~ firm_id + stack_id)
}

extract_main_coef <- function(model, target_term = "did_treatment") {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[ct$term == target_term, ]
  if (nrow(row) == 0) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      p_value = NA_real_,
      n_obs = nobs(model),
      r2 = fitstat(model, "r2")
    ))
  }
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

build_event_study_table <- function(event_dt, outcome_label, main_effect, main_se, main_p,
                                    pre_p, file_path, file_label, caption,
                                    cluster_label = "firm and stack") {
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
      "Each row reports the difference-in-differences coefficient at the indicated event time, with the 95\\% confidence interval obtained from two-way cluster-robust standard errors by",
      paste0(cluster_label, "."),
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

plot_event_study <- function(event_dt, outcome_name, y_title, main_effect, main_se, pre_p, file_path) {
  x_range <- range(event_dt$event_time, na.rm = TRUE)
  y_range <- range(c(event_dt$ci_lo, event_dt$ci_hi), na.rm = TRUE)
  y_span <- y_range[2] - y_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
  }

  ann_y <- y_range[2] + 0.09 * y_span
  pre_y <- y_range[2] - 0.08 * y_span
  ann_x <- 2
  pre_x <- -5
  if (outcome_name %in% c("legal_reasoning_share", "log_legal_reasoning_length_chars")) {
    ann_y <- -0.01
    pre_y <- -0.04
  }
  if (outcome_name == "log_legal_reasoning_length_chars") {
    ann_y <- -0.1
    pre_y <- -0.1
  }
  if (outcome_name == "legal_reasoning_share") {
    pre_y <- -0.01
  }
  if (outcome_name == "case_win_rate_fee") {
    ann_y <- 0.20
    pre_y <- 0.20
  }

  ylim_lo <- y_range[1] - 0.05 * y_span
  ylim_hi <- y_range[2] + 0.12 * y_span
  text_y_values <- c(ann_y, pre_y)
  text_y_values <- text_y_values[is.finite(text_y_values)]
  if (length(text_y_values) > 0) {
    ylim_hi <- max(ylim_hi, max(text_y_values) + 0.04 * y_span)
    ylim_lo <- min(ylim_lo, min(text_y_values) - 0.04 * y_span)
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
  points(event_dt$event_time, event_dt$estimate, col = "black", pch = 16, cex = 1.1)

  text(
    x = ann_x,
    y = ann_y,
    labels = sprintf("Main DID = %s\n(SE = %s)", fmt_num(main_effect), fmt_num(main_se)),
    adj = c(0, 1),
    cex = 0.88
  )

  text(
    x = pre_x,
    y = pre_y,
    labels = single_pre_expr(pre_p),
    adj = c(0, 1),
    cex = 0.82
  )
}

build_main_table <- function(main_results, robust_results, file_path) {
  column_keys <- c(
    "legal_reasoning_share",
    "log_legal_reasoning_length_chars",
    "case_win_binary"
  )

  all_results <- c(
    main_results[column_keys],
    robust_results[column_keys]
  )

  coef_row <- sapply(all_results, function(res) paste0(fmt_num(res$estimate), stars(res$p_value)))
  se_row <- sapply(all_results, function(res) paste0("(", fmt_num(res$se), ")"))
  obs_row <- sapply(all_results, function(res) fmt_int(res$n_obs))
  r2_row <- sapply(all_results, function(res) fmt_num(res$r2))
  sample_row <- c("All documents", "All documents", "Decisive cases", "All documents", "All documents", "Decisive cases")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Document-Level DID Estimates}",
    "\\label{tab:document_level_did_main}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{3}{c}{Stack $\\times$ Year FE} & \\multicolumn{3}{c}{Court $\\times$ Year FE} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    " & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
    "Outcome & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Firm FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(c("Yes", "Yes", "Yes", "No", "No", "No"), collapse = " & "), "\\\\"),
    paste("Court $\\times$ Year FE &", paste(c("No", "No", "No", "Yes", "Yes", "Yes"), collapse = " & "), "\\\\"),
    paste("Cause $\\times$ Side FE &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Court FE &", paste(c("Yes", "Yes", "Yes", "No", "No", "No"), collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    paste("Lawyer Controls &", paste(rep("Yes", 6), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on Winner $\\times$ Post from document-level difference-in-differences regressions.",
      "Reasoning Share is the share of the judgment text devoted to legal reasoning;",
      "log(Reasoning Length + 1) is the natural log of one plus the character count of the reasoning section;",
      "Case Win Binary is an indicator for the represented side prevailing among decisive cases.",
      "Columns 1--3 use stack $\\times$ year fixed effects; columns 4--6 replace them with court $\\times$ year fixed effects.",
      "All specifications include firm, court, and cause $\\times$ side fixed effects, case controls for opposing-side representation and plaintiff/defendant entity status, and lawyer controls.",
      "Two-way cluster-robust standard errors appear in parentheses, clustered by firm and stack in columns 1--3 and by firm and court in columns 4--6.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_fee_winrate_appendix_table <- function(main_result, robust_result, file_path) {
  coef_row <- c(
    paste0(fmt_num(main_result$estimate), stars(main_result$p_value)),
    paste0(fmt_num(robust_result$estimate), stars(robust_result$p_value))
  )
  se_row <- c(
    paste0("(", fmt_num(main_result$se), ")"),
    paste0("(", fmt_num(robust_result$se), ")")
  )
  obs_row <- c(fmt_int(main_result$n_obs), fmt_int(robust_result$n_obs))
  r2_row <- c(fmt_num(main_result$r2), fmt_num(robust_result$r2))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Document-Level Fee-Based Win-Rate Robustness}",
    "\\label{tab:document_level_fee_winrate_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    "Outcome & Case Fee Win Rate & Case Fee Win Rate \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Firm FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(c("Yes", "No"), collapse = " & "), "\\\\"),
    paste("Court $\\times$ Year FE &", paste(c("No", "Yes"), collapse = " & "), "\\\\"),
    paste("Cause $\\times$ Side FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Lawyer Controls &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on Winner $\\times$ Post from document-level difference-in-differences regressions.",
      "The outcome is the represented side's fee-based win rate in decisive cases with observed fee allocation.",
      "Column 1 uses stack $\\times$ year fixed effects and column 2 replaces them with court $\\times$ year fixed effects.",
      "All specifications include firm and cause $\\times$ side fixed effects, case controls for opposing-side representation and plaintiff/defendant entity status, and lawyer controls.",
      "Two-way cluster-robust standard errors appear in parentheses, clustered by firm and stack in column 1 and by firm and court in column 2.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_attribute_table <- function(results_list, fee_results_list, file_path) {
  base_keys <- c("legal_reasoning_share", "log_legal_reasoning_length_chars", "case_win_binary")
  panel_order <- c("ccp", "gender", "seniority", "masterplus")
  panel_titles <- c(
    ccp = "Panel A. CCP",
    gender = "Panel B. Female",
    seniority = "Panel C. Seniority",
    masterplus = "Panel D. Master and Above"
  )
  row_labels <- c(
    ccp = "Winner $\\times$ Post $\\times$ CCP",
    gender = "Winner $\\times$ Post $\\times$ Female",
    seniority = "Winner $\\times$ Post $\\times$ Seniority (std.)",
    masterplus = "Winner $\\times$ Post $\\times$ Master and Above"
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Document-Level Heterogeneity by Lawyer Attributes}",
    "\\label{tab:document_level_attribute_heterogeneity}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) \\\\",
    "Outcome & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary & Case Fee Win Rate \\\\",
    "\\midrule"
  )

  for (panel_key in panel_order) {
    base_coef <- sapply(base_keys, function(key) {
      res <- results_list[[panel_key]][[key]]
      paste0(fmt_num(res$estimate), stars(res$p_value))
    })
    base_se <- sapply(base_keys, function(key) {
      res <- results_list[[panel_key]][[key]]
      paste0("(", fmt_num(res$se), ")")
    })
    base_obs <- sapply(base_keys, function(key) fmt_int(results_list[[panel_key]][[key]]$n_obs))
    base_r2 <- sapply(base_keys, function(key) fmt_num(results_list[[panel_key]][[key]]$r2))

    fee_res <- fee_results_list[[panel_key]]
    fee_coef <- paste0(fmt_num(fee_res$estimate), stars(fee_res$p_value))
    fee_se <- paste0("(", fmt_num(fee_res$se), ")")
    fee_obs <- fmt_int(fee_res$n_obs)
    fee_r2 <- fmt_num(fee_res$r2)

    coef_row <- c(base_coef, fee_coef)
    se_row <- c(base_se, fee_se)
    obs_row <- c(base_obs, fee_obs)
    r2_row <- c(base_r2, fee_r2)

    lines <- c(
      lines,
      "\\addlinespace",
      paste0("\\multicolumn{5}{l}{\\textit{", panel_titles[[panel_key]], "}} \\\\"),
      paste(row_labels[[panel_key]], "&", paste(coef_row, collapse = " & "), "\\\\"),
      paste("&", paste(se_row, collapse = " & "), "\\\\"),
      paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
      paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\")
    )
  }

  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on the triple interaction Winner $\\times$ Post $\\times$ Attribute, where Attribute is the lawyer characteristic listed at the top of each panel.",
      "CCP equals 1 if the matched lawyer is a Communist Party member.",
      "Female equals 1 if the matched lawyer is a woman.",
      "Seniority is standardized lawyer practice years.",
      "Master and Above equals 1 if the matched lawyer has a master's or PhD degree.",
      "Outcomes are the share of legal reasoning (column 1), the log of one plus reasoning length in characters (column 2), an indicator for a binary win in decisive cases (column 3), and the fee-based win rate in decisive cases with observed fee allocation (column 4).",
      "All specifications include firm, stack $\\times$ year, court, and cause $\\times$ side fixed effects, plus case controls for opposing-side representation and plaintiff/defendant entity status.",
      "Two-way cluster-robust standard errors by firm and stack appear in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  dt <- read_document_panel(input_file)

  model_specs <- list(
    legal_reasoning_share = list(
      y_title = "Share of Legal Reasoning",
      sample_filter = NULL,
      make_figure = TRUE
    ),
    log_legal_reasoning_length_chars = list(
      y_title = "Log Legal-Reasoning Length",
      sample_filter = NULL,
      make_figure = TRUE
    ),
    case_win_binary = list(
      y_title = "Win Probability (Decisive Civil Cases)",
      sample_filter = quote(case_decisive == 1),
      make_figure = FALSE
    )
  )
  fee_winrate_spec <- list(
    y_title = "Fee-Based Win Rate",
    sample_filter = quote(case_decisive == 1 & !is.na(case_win_rate_fee))
  )

  main_results <- list()
  robust_results <- list()

  for (outcome_name in names(model_specs)) {
    spec <- model_specs[[outcome_name]]

    main_model <- estimate_model(dt, outcome_name, sample_filter = spec$sample_filter, fe_variant = "main")
    main_event <- estimate_event_model(dt, outcome_name, sample_filter = spec$sample_filter, fe_variant = "main")
    main_coef <- extract_main_coef(main_model)
    main_pre <- extract_pretest(main_event)
    main_results[[outcome_name]] <- main_coef

    robust_model <- estimate_model(dt, outcome_name, sample_filter = spec$sample_filter, fe_variant = "court_year")
    robust_event <- estimate_event_model(dt, outcome_name, sample_filter = spec$sample_filter, fe_variant = "court_year")
    robust_coef <- extract_main_coef(robust_model)
    robust_pre <- extract_pretest(robust_event)
    robust_results[[outcome_name]] <- robust_coef

    if (isTRUE(spec$make_figure)) {
      plot_event_study(
        event_dt = extract_event_dt(main_event),
        outcome_name = outcome_name,
        y_title = spec$y_title,
        main_effect = main_coef$estimate,
        main_se = main_coef$se,
        pre_p = main_pre$p_value,
        file_path = file.path(
          figure_dir,
          build_output_name(sprintf("document_level_%s_event_study", outcome_name), "pdf")
        )
      )

      build_event_study_table(
        event_dt = extract_event_dt(main_event),
        outcome_label = spec$y_title,
        main_effect = main_coef$estimate,
        main_se = main_coef$se,
        main_p = main_coef$p_value,
        pre_p = main_pre$p_value,
        file_path = file.path(
          table_dir,
          build_output_name(sprintf("document_level_%s_event_study_table", outcome_name), "tex")
        ),
        file_label = sprintf("document_level_%s_event_study", outcome_name),
        caption = sprintf(
          "Event-Study Estimates Behind the Document-Level %s Figure",
          spec$y_title
        )
      )
    }
  }

  build_main_table(
    main_results = main_results,
    robust_results = robust_results,
    file_path = file.path(table_dir, build_output_name("document_level_did_main_table", "tex"))
  )

  fee_main_model <- estimate_model(dt, "case_win_rate_fee", sample_filter = fee_winrate_spec$sample_filter, fe_variant = "main")
  fee_main_event <- estimate_event_model(dt, "case_win_rate_fee", sample_filter = fee_winrate_spec$sample_filter, fe_variant = "main")
  fee_main_coef <- extract_main_coef(fee_main_model)
  fee_main_pre <- extract_pretest(fee_main_event)

  fee_robust_model <- estimate_model(dt, "case_win_rate_fee", sample_filter = fee_winrate_spec$sample_filter, fe_variant = "court_year")
  fee_robust_coef <- extract_main_coef(fee_robust_model)

  plot_event_study(
    event_dt = extract_event_dt(fee_main_event),
    outcome_name = "case_win_rate_fee",
    y_title = fee_winrate_spec$y_title,
    main_effect = fee_main_coef$estimate,
    main_se = fee_main_coef$se,
    pre_p = fee_main_pre$p_value,
    file_path = file.path(
      figure_dir,
      build_output_name("document_level_case_fee_win_rate_event_study", "pdf")
    )
  )

  build_event_study_table(
    event_dt = extract_event_dt(fee_main_event),
    outcome_label = fee_winrate_spec$y_title,
    main_effect = fee_main_coef$estimate,
    main_se = fee_main_coef$se,
    main_p = fee_main_coef$p_value,
    pre_p = fee_main_pre$p_value,
    file_path = file.path(
      table_dir,
      build_output_name("document_level_case_fee_win_rate_event_study_table", "tex")
    ),
    file_label = "document_level_case_fee_win_rate_event_study",
    caption = "Event-Study Estimates Behind the Document-Level Fee-Based Win-Rate Figure"
  )

  build_fee_winrate_appendix_table(
    main_result = fee_main_coef,
    robust_result = fee_robust_coef,
    file_path = file.path(table_dir, build_output_name("document_level_fee_winrate_appendix_table", "tex"))
  )

  attribute_specs <- list(
    ccp = list(term = "lawyer_ccp_bin", label = "did_treatment:lawyer_ccp_bin", is_continuous = FALSE),
    gender = list(term = "lawyer_female", label = "did_treatment:lawyer_female", is_continuous = FALSE),
    seniority = list(term = "lawyer_practice_years_std", label = "did_treatment:lawyer_practice_years_std", is_continuous = TRUE),
    masterplus = list(term = "lawyer_high_edu", label = "did_treatment:lawyer_high_edu", is_continuous = FALSE)
  )

  attribute_results <- list()
  for (attr_name in names(attribute_specs)) {
    attr_spec <- attribute_specs[[attr_name]]
    attribute_results[[attr_name]] <- list()

    for (outcome_name in names(model_specs)) {
      sample_filter <- model_specs[[outcome_name]]$sample_filter
      model <- estimate_single_attribute_model(
        dt = dt,
        outcome_name = outcome_name,
        attribute_term = attr_spec$term,
        sample_filter = sample_filter,
        is_continuous = attr_spec$is_continuous
      )
      attribute_results[[attr_name]][[outcome_name]] <- extract_main_coef(model, attr_spec$label)
    }
  }

  fee_attribute_results <- list()
  for (attr_name in names(attribute_specs)) {
    attr_spec <- attribute_specs[[attr_name]]
    model <- estimate_single_attribute_model(
      dt = dt,
      outcome_name = "case_win_rate_fee",
      attribute_term = attr_spec$term,
      sample_filter = fee_winrate_spec$sample_filter,
      is_continuous = attr_spec$is_continuous
    )
    fee_attribute_results[[attr_name]] <- extract_main_coef(model, attr_spec$label)
  }

  build_attribute_table(
    results_list = attribute_results,
    fee_results_list = fee_attribute_results,
    file_path = file.path(table_dir, build_output_name("document_level_attribute_heterogeneity_table", "tex"))
  )
}

main()
