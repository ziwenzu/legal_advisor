#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
input_file <- Sys.getenv(
  "DOCUMENT_DDD_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "document_level_winner_vs_loser_ddd.csv")
)
table_dir <- file.path(root_dir, "output", "tables")
output_tag <- Sys.getenv("DOCUMENT_DDD_OUTPUT_TAG", unset = "")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

build_output_name <- function(stem, ext) {
  if (nzchar(output_tag)) {
    sprintf("%s_%s.%s", output_tag, stem, ext)
  } else {
    sprintf("%s.%s", stem, ext)
  }
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(x) {
  if (length(x) == 0 || is.na(x)) return("NA")
  if (x < 0.001) return("<0.001")
  sprintf("%.3f", x)
}

stars_plain <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("")
  if (p_value < 0.01) return("***")
  if (p_value < 0.05) return("**")
  if (p_value < 0.10) return("*")
  ""
}

stars_tex <- function(p_value) {
  star <- stars_plain(p_value)
  if (!nzchar(star)) return("")
  sprintf("$^{%s}$", star)
}

read_document_panel <- function(path) {
  dt <- fread(path)

  dt[, court_key := fifelse(is.na(court_match_key) | court_match_key == "", NA_character_, court_match_key)]
  dt <- dt[!is.na(court_key)]

  dt[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  dt[, court_year_fe := sprintf("%s__%s", court_key, year)]
  dt[, cause_side_fe := sprintf("%s__%s", cause, side)]

  dt[, plaintiff_party_is_entity := as.integer(plaintiff_party_is_entity)]
  dt[, defendant_party_is_entity := as.integer(defendant_party_is_entity)]
  dt[, opponent_has_lawyer := as.integer(opponent_has_lawyer)]
  dt[, case_decisive := as.integer(case_decisive)]
  dt[, prior_admin_gov_exposure := as.integer(prior_admin_gov_exposure)]
  dt[, has_pre_admin_civil_case_in_court := as.integer(has_pre_admin_civil_case_in_court)]

  # Main-table sample: keep all unexposed rows, but require exposed pairs to
  # have pre-admin civil business in that same court.
  dt[, exposed_pair_has_pre_civil_support := as.integer(
    prior_admin_gov_exposure == 0L | has_pre_admin_civil_case_in_court == 1L
  )]
  dt[, ddd_binary := did_treatment * prior_admin_gov_exposure]

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

build_formula <- function(outcome_name) {
  rhs_terms <- paste(
    "did_treatment",
    "prior_admin_gov_exposure",
    "ddd_binary",
    control_rhs,
    sep = " + "
  )
  fe_terms <- paste(
    "firm_id",
    "stack_year_fe",
    "court_year_fe",
    "cause_side_fe",
    "year_gender_fe",
    "year_ccp_fe",
    "year_edu_fe",
    sep = " + "
  )
  as.formula(sprintf("%s ~ %s | %s", outcome_name, rhs_terms, fe_terms))
}

build_sample_filter <- function(base_filter = NULL) {
  if (is.null(base_filter)) {
    quote(exposed_pair_has_pre_civil_support == 1L)
  } else {
    bquote((.(base_filter)) & exposed_pair_has_pre_civil_support == 1L)
  }
}

estimate_ddd_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  keep_mask <- !is.na(work_dt[[outcome_name]])
  if (!is.null(sample_filter)) {
    keep_mask <- keep_mask & work_dt[, eval(sample_filter)]
  }
  work_dt <- work_dt[keep_mask]

  feols(build_formula(outcome_name), data = work_dt, cluster = ~ firm_id + court_key)
}

extract_terms <- function(model, terms) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  out <- data.table(term = terms)
  out <- merge(out, ct, by = "term", all.x = TRUE, sort = FALSE)
  out[, n_obs := nobs(model)]
  out[, r2 := fitstat(model, "r2")[[1]]]
  out[, estimate := Estimate]
  out[, std_error := `Std. Error`]
  out[, p_value := `Pr(>|t|)`]
  out[, c("Estimate", "Std. Error", "Pr(>|t|)") := NULL]
  out[]
}

run_models <- function(dt) {
  specs <- list(
    list(
      outcome = "legal_reasoning_share",
      column_id = 1L,
      column_label = "Reasoning Share",
      sample_label = "All documents",
      base_filter = NULL
    ),
    list(
      outcome = "log_legal_reasoning_length_chars",
      column_id = 2L,
      column_label = "log(Reasoning Length + 1)",
      sample_label = "All documents",
      base_filter = NULL
    ),
    list(
      outcome = "case_win_binary",
      column_id = 3L,
      column_label = "Case Win Binary",
      sample_label = "Decisive cases",
      base_filter = quote(case_decisive == 1L)
    )
  )

  term_labels <- c(
    did_treatment = "Winner $\\\\times$ Post",
    prior_admin_gov_exposure = "Previously Represented Gov't",
    ddd_binary = "Winner $\\\\times$ Post $\\\\times$ Previously Represented Gov't"
  )

  results <- list()
  for (spec in specs) {
    model <- estimate_ddd_model(
      dt = dt,
      outcome_name = spec$outcome,
      sample_filter = build_sample_filter(spec$base_filter)
    )
    term_dt <- extract_terms(model, names(term_labels))
    term_dt[, column_id := spec$column_id]
    term_dt[, column_label := spec$column_label]
    term_dt[, sample_label := spec$sample_label]
    term_dt[, outcome := spec$outcome]
    term_dt[, term_label := term_labels[term]]
    results[[length(results) + 1L]] <- term_dt
  }

  rbindlist(results, fill = TRUE)
}

run_fee_winrate_model <- function(dt) {
  term_labels <- c(
    did_treatment = "Winner $\\\\times$ Post",
    prior_admin_gov_exposure = "Previously Represented Gov't",
    ddd_binary = "Winner $\\\\times$ Post $\\\\times$ Previously Represented Gov't"
  )
  model <- estimate_ddd_model(
    dt = dt,
    outcome_name = "case_win_rate_fee",
    sample_filter = build_sample_filter(quote(case_decisive == 1L & !is.na(case_win_rate_fee)))
  )
  term_dt <- extract_terms(model, names(term_labels))
  term_dt[, column_id := 1L]
  term_dt[, column_label := "Case Fee Win Rate"]
  term_dt[, sample_label := "Decisive cases with fee share"]
  term_dt[, outcome := "case_win_rate_fee"]
  term_dt[, term_label := term_labels[term]]
  term_dt[]
}

write_latex_table <- function(results) {
  term_order <- c("did_treatment", "prior_admin_gov_exposure", "ddd_binary")
  tab <- copy(results)
  tab[, term_order := match(term, term_order)]
  setorder(tab, term_order, column_id)

  coef_row <- function(term_name) {
    block <- tab[term == term_name][order(column_id)]
    coef_cells <- vapply(
      seq_len(nrow(block)),
      function(i) paste0(fmt_num(block$estimate[i]), stars_tex(block$p_value[i])),
      character(1)
    )
    se_cells <- vapply(
      seq_len(nrow(block)),
      function(i) paste0("(", fmt_num(block$std_error[i]), ")"),
      character(1)
    )
    list(
      coef = paste0(block$term_label[1], " & ", paste(coef_cells, collapse = " & "), " \\\\"),
      se = paste0(" & ", paste(se_cells, collapse = " & "), " \\\\")
    )
  }

  row_did <- coef_row("did_treatment")
  row_exp <- coef_row("prior_admin_gov_exposure")
  row_ddd <- coef_row("ddd_binary")

  col_block <- tab[term == "ddd_binary"][order(column_id)]
  n_cells <- vapply(seq_len(nrow(col_block)), function(i) fmt_int(col_block$n_obs[i]), character(1))
  r2_cells <- vapply(seq_len(nrow(col_block)), function(i) fmt_num(col_block$r2[i]), character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Document-Level Strict DDD Estimates}",
    "\\label{tab:document_level_strict_ddd_main}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "Outcome & Reasoning Share & log(Reasoning Length + 1) & Case Win Binary \\\\",
    "\\midrule",
    row_did$coef,
    row_did$se,
    "\\addlinespace",
    row_exp$coef,
    row_exp$se,
    "\\addlinespace",
    row_ddd$coef,
    row_ddd$se,
    "\\addlinespace",
    paste0("Observations & ", paste(n_cells, collapse = " & "), " \\\\"),
    paste0("$R^2$ & ", paste(r2_cells, collapse = " & "), " \\\\"),
    "Sample & All documents & All documents & Decisive cases \\\\",
    "Firm FE & Yes & Yes & Yes \\\\",
    "Stack $\\\\times$ Year FE & Yes & Yes & Yes \\\\",
    "Court $\\\\times$ Year FE & Yes & Yes & Yes \\\\",
    "Cause $\\\\times$ Side FE & Yes & Yes & Yes \\\\",
    "Case Controls & Yes & Yes & Yes \\\\",
    "Lawyer-Year FE bins & Yes & Yes & Yes \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste0(
      "\\item Note: The sample keeps all unexposed rows and restricts exposed firm-court pairs to those with at least one civil case in that same court before the first observed government-side administrative appearance there. ",
      "All columns use the raw winner-vs-runner-up document sample and include firm FE, stack $\\\\times$ year FE, court $\\\\times$ year FE, cause $\\\\times$ side FE, case controls, and lawyer-year attribute bins. ",
      "Standard errors are two-way clustered by firm and cleaned court. ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, build_output_name("document_level_strict_ddd_main_table", "tex"))
  writeLines(lines, out_path, useBytes = TRUE)
}

write_fee_appendix_table <- function(results) {
  term_order <- c("did_treatment", "prior_admin_gov_exposure", "ddd_binary")
  tab <- copy(results)
  tab[, term_order := match(term, term_order)]
  setorder(tab, term_order)

  coef_row <- function(term_name) {
    row <- tab[term == term_name]
    list(
      coef = paste0(row$term_label[1], " & ", fmt_num(row$estimate[1]), stars_tex(row$p_value[1]), " \\\\"),
      se = paste0(" & (", fmt_num(row$std_error[1]), ") \\\\")
    )
  }

  row_did <- coef_row("did_treatment")
  row_exp <- coef_row("prior_admin_gov_exposure")
  row_ddd <- coef_row("ddd_binary")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Document-Level Strict DDD with Fee-Based Win Rate}",
    "\\label{tab:document_level_strict_ddd_fee_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lc}",
    "\\toprule",
    " & Case Fee Win Rate \\\\",
    "\\midrule",
    row_did$coef,
    row_did$se,
    "\\addlinespace",
    row_exp$coef,
    row_exp$se,
    "\\addlinespace",
    row_ddd$coef,
    row_ddd$se,
    "\\addlinespace",
    paste0("Observations & ", fmt_int(tab$n_obs[1]), " \\\\"),
    paste0("$R^2$ & ", fmt_num(tab$r2[1]), " \\\\"),
    "Sample & Decisive cases with fee share \\\\",
    "Firm FE & Yes \\\\",
    "Stack $\\\\times$ Year FE & Yes \\\\",
    "Court $\\\\times$ Year FE & Yes \\\\",
    "Cause $\\\\times$ Side FE & Yes \\\\",
    "Case Controls & Yes \\\\",
    "Lawyer-Year FE bins & Yes \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste0(
      "\\item Note: This appendix table replaces the binary win/loss outcome with `case_win_rate_fee`, the represented side's fee-based win-rate measure constructed from the SQL `shoulifeiyuangaobizhong` field. ",
      "The estimates use the raw winner-vs-runner-up document sample. ",
      "The sample keeps all unexposed rows and restricts exposed firm-court pairs to those with at least one civil case in that same court before the first observed government-side administrative appearance there. ",
      "Standard errors are two-way clustered by firm and cleaned court. ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, build_output_name("document_level_strict_ddd_fee_winrate_appendix_table", "tex"))
  writeLines(lines, out_path, useBytes = TRUE)
}

main <- function() {
  dt <- read_document_panel(input_file)
  results <- run_models(dt)
  fee_results <- run_fee_winrate_model(dt)
  write_latex_table(results)
  write_fee_appendix_table(fee_results)
}

main()
