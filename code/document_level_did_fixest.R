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

  # Party type is exactly redundant with the entity indicator in this clean sample.
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

plot_event_study <- function(event_dt, outcome_name, y_title, main_effect, main_se, pre_p, file_path) {
  x_range <- range(event_dt$event_time, na.rm = TRUE)
  y_range <- range(c(event_dt$ci_lo, event_dt$ci_hi), na.rm = TRUE)
  y_span <- y_range[2] - y_range[1]
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- 1
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
    ylim = c(y_range[1] - 0.05 * y_span, y_range[2] + 0.12 * y_span),
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

  ann_y <- y_range[2] + 0.09 * y_span
  pre_y <- y_range[2] - 0.08 * y_span
  ann_x <- -5
  pre_x <- -5
  if (outcome_name %in% c("legal_reasoning_share", "log_legal_reasoning_length_chars")) {
    ann_y <- -0.01
    pre_y <- -0.04
  }
  if (outcome_name == "log_legal_reasoning_length_chars") {
    ann_x <- 2
    ann_y <- -0.1
    pre_y <- -0.1
  }
  if (outcome_name == "legal_reasoning_share") {
    ann_x <- 2
    pre_y <- -0.01
  }

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

build_main_table <- function(results_list, file_path, main_note) {
  column_keys <- c(
    "legal_reasoning_share",
    "log_legal_reasoning_length_chars",
    "case_win_binary"
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
    "\\caption{Document-Level DID Estimates for Winner Versus Loser Law Firms}",
    "\\label{tab:document_level_did_main}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "Outcome & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Firm FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Cause $\\times$ Side FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Court FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Clustered SE &", paste(rep("Firm and stack", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item Note: The sample contains only current clean winner-versus-loser firms matched to `winner_vs_runnerup_case = 1` documents.",
      "Each `case_uid` contributes only one retained law-firm record: one winner if present, otherwise one loser.",
      "All columns include opponent-lawyer and plaintiff/defendant entity controls.",
      "Lawyer controls include practice years and attribute-specific year fixed effects for gender, party membership, and education.",
      "Party-type fields are omitted because they are exactly redundant with the entity indicators in this clean sample.",
      "Column 3 is estimated on decisive cases only.",
      main_note,
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_robustness_table <- function(results_list, file_path) {
  column_keys <- c(
    "legal_reasoning_share",
    "log_legal_reasoning_length_chars",
    "case_win_binary"
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
    "\\caption{Document-Level DID Robustness with Court-by-Year Fixed Effects}",
    "\\label{tab:document_level_did_court_year}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "Outcome & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Firm FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Court $\\times$ Year FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Cause $\\times$ Side FE &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Clustered SE &", paste(rep("Firm and court", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item Note: This table replaces stack-by-year fixed effects with court-by-year fixed effects.",
      "The sample and controls match the main specification, including one retained law-firm record per `case_uid`, lawyer practice-years control, and lawyer-attribute-by-year fixed effects for gender, party membership, and education.",
      "Column 3 is estimated on decisive cases only.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_attribute_table <- function(results_list, file_path) {
  column_keys <- c("legal_reasoning_share", "case_win_binary")
  row_terms <- c(
    "did_treatment",
    "did_treatment:lawyer_ccp_bin",
    "did_treatment:lawyer_high_edu",
    "did_treatment:lawyer_practice_years_std"
  )
  row_labels <- c(
    "Winner $\\times$ Post",
    "Winner $\\times$ Post $\\times$ CCP",
    "Winner $\\times$ Post $\\times$ High Edu",
    "Winner $\\times$ Post $\\times$ Practice Years (std.)"
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Suggestive Heterogeneity by Random Lawyer Attributes}",
    "\\label{tab:document_level_attribute_heterogeneity}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    "Outcome & Reasoning Share & Case Win Binary \\\\",
    "\\midrule"
  )

  for (i in seq_along(row_terms)) {
    term <- row_terms[[i]]
    coef_row <- sapply(column_keys, function(key) {
      res <- results_list[[key]][[term]]
      paste0(fmt_num(res$estimate), stars(res$p_value))
    })
    se_row <- sapply(column_keys, function(key) {
      res <- results_list[[key]][[term]]
      paste0("(", fmt_num(res$se), ")")
    })
    lines <- c(
      lines,
      paste(row_labels[[i]], "&", paste(coef_row, collapse = " & "), "\\\\"),
      paste("&", paste(se_row, collapse = " & "), "\\\\")
    )
  }

  obs_row <- sapply(column_keys, function(key) fmt_int(results_list[[key]][["did_treatment"]]$n_obs))
  r2_row <- sapply(column_keys, function(key) fmt_num(results_list[[key]][["did_treatment"]]$r2))

  lines <- c(
    lines,
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Firm FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Cause $\\times$ Side FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Court FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Case Controls &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Lawyer-Linked Sample &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item Note: These interaction estimates are suggestive heterogeneity, not causal mediation.",
      "Each firm is assigned one stable random lawyer from the lawyer list, and the coefficient heterogeneity is estimated within the lawyer-linked sample.",
      "High education is defined as master's or PhD training.",
      "Column 2 is estimated on decisive cases only.",
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
      y_title = "Reasoning Share",
      sample_filter = NULL
    ),
    log_legal_reasoning_length_chars = list(
      y_title = "Log Reasoning Length",
      sample_filter = NULL
    ),
    case_win_binary = list(
      y_title = "Case Win Probability",
      sample_filter = quote(case_decisive == 1)
    )
  )

  main_results <- list()
  robust_results <- list()
  diagnostics_rows <- list()

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

    diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.table(
      outcome = outcome_name,
      spec = "main",
      estimate = main_coef$estimate,
      se = main_coef$se,
      p_value = main_coef$p_value,
      pretrend_p = main_pre$p_value,
      n_obs = main_coef$n_obs,
      r2 = main_coef$r2
    )
    diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.table(
      outcome = outcome_name,
      spec = "court_year",
      estimate = robust_coef$estimate,
      se = robust_coef$se,
      p_value = robust_coef$p_value,
      pretrend_p = robust_pre$p_value,
      n_obs = robust_coef$n_obs,
      r2 = robust_coef$r2
    )

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
  }

  build_main_table(
    results_list = main_results,
    file_path = file.path(table_dir, build_output_name("document_level_did_main_table", "tex")),
    main_note = "The main specification absorbs firm fixed effects, stack-by-year fixed effects, cause-by-side fixed effects, and court fixed effects, with two-way clustering by firm and stack."
  )

  build_robustness_table(
    results_list = robust_results,
    file_path = file.path(table_dir, build_output_name("document_level_did_court_year_robustness_table", "tex"))
  )

  attribute_results <- list()
  for (outcome_name in c("legal_reasoning_share", "case_win_binary")) {
    sample_filter <- if (outcome_name == "case_win_binary") quote(case_decisive == 1) else NULL
    model <- estimate_attribute_model(dt, outcome_name, sample_filter = sample_filter)
    attribute_results[[outcome_name]] <- list(
      did_treatment = extract_main_coef(model, "did_treatment"),
      `did_treatment:lawyer_ccp_bin` = extract_main_coef(model, "did_treatment:lawyer_ccp_bin"),
      `did_treatment:lawyer_high_edu` = extract_main_coef(model, "did_treatment:lawyer_high_edu"),
      `did_treatment:lawyer_practice_years_std` = extract_main_coef(model, "did_treatment:lawyer_practice_years_std")
    )
  }

  build_attribute_table(
    results_list = attribute_results,
    file_path = file.path(table_dir, build_output_name("document_level_attribute_heterogeneity_table", "tex"))
  )

  diagnostics <- rbindlist(diagnostics_rows, use.names = TRUE)
  fwrite(
    diagnostics,
    file = file.path(table_dir, build_output_name("document_level_result_diagnostics", "csv"))
  )
}

main()
