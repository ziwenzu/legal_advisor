#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
input_file <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
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
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
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

  dt[, ddd_support_row := as.integer(
    prior_admin_gov_exposure == 0L | has_pre_admin_civil_case_in_court == 1L
  )]
  dt[, treated_by_prior := treated_firm * prior_admin_gov_exposure]
  dt[, post_by_prior := post * prior_admin_gov_exposure]
  dt[, ddd_binary := did_treatment * prior_admin_gov_exposure]

  dt[, lawyer_gender := as.integer(lawyer_gender)]
  dt[, lawyer_edu := as.integer(lawyer_edu)]

  dt[, lawyer_female := as.integer(lawyer_gender == 1L)]
  dt[, lawyer_ccp_bin := fifelse(lawyer_ccp == 1, 1L, 0L, na = NA_integer_)]
  dt[, lawyer_high_edu := as.integer(lawyer_edu >= 4L)]
  dt[, lawyer_gender_group := fifelse(is.na(lawyer_female), "unknown", fifelse(lawyer_female == 1L, "female", "male"))]
  dt[, lawyer_ccp_group := fifelse(is.na(lawyer_ccp_bin), "unknown", fifelse(lawyer_ccp_bin == 1L, "ccp", "nonccp"))]
  dt[, lawyer_edu_group := fifelse(is.na(lawyer_high_edu), "unknown", fifelse(lawyer_high_edu == 1L, "highedu", "baselineedu"))]
  if (!any(dt$lawyer_female == 1L, na.rm = TRUE)) {
    stop("lawyer_female contains no female observations after UTF-8 decoding")
  }

  practice_mean <- mean(dt$lawyer_practice_years, na.rm = TRUE)
  practice_sd <- sd(dt$lawyer_practice_years, na.rm = TRUE)
  dt[, lawyer_practice_years_std := (lawyer_practice_years - practice_mean) / practice_sd]
  dt[, lawyer_practice_years_obs := as.integer(!is.na(lawyer_practice_years))]
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
  "lawyer_practice_years_obs",
  sep = " + "
)

build_formula <- function(outcome_name) {
  rhs_terms <- paste(
    "did_treatment",
    "prior_admin_gov_exposure",
    "treated_by_prior",
    "post_by_prior",
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
    quote(ddd_support_row == 1L)
  } else {
    bquote((.(base_filter)) & ddd_support_row == 1L)
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
    did_treatment = "Winner $\\times$ Post",
    prior_admin_gov_exposure = "Previously Represented Gov't",
    ddd_binary = "Winner $\\times$ Post $\\times$ Previously Represented Gov't"
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
    did_treatment = "Winner $\\times$ Post",
    prior_admin_gov_exposure = "Previously Represented Gov't",
    ddd_binary = "Winner $\\times$ Post $\\times$ Previously Represented Gov't"
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
  n_cols <- nrow(col_block)
  yes_row <- paste(rep("Yes", n_cols), collapse = " & ")

  sample_cells <- vapply(
    seq_len(n_cols),
    function(i) col_block$sample_label[i],
    character(1)
  )

  outcome_cells <- vapply(
    seq_len(n_cols),
    function(i) col_block$column_label[i],
    character(1)
  )
  num_cells <- paste(sprintf("(%d)", seq_len(n_cols)), collapse = " & ")
  align_str <- paste0("l", paste(rep("c", n_cols), collapse = ""))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Document-Level Strict Triple-Difference Estimates}",
    "\\label{tab:document_level_strict_ddd_main}",
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{%s}", align_str),
    "\\toprule",
    paste(" &", num_cells, "\\\\"),
    paste("Outcome &", paste(outcome_cells, collapse = " & "), "\\\\"),
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
    paste("Case Controls &", yes_row, "\\\\"),
    paste("Lawyer Controls &", yes_row, "\\\\"),
    paste("Firm Fixed Effects &", yes_row, "\\\\"),
    paste("Stack $\\times$ Year Fixed Effects &", yes_row, "\\\\"),
    paste("Court $\\times$ Year Fixed Effects &", yes_row, "\\\\"),
    paste("Cause $\\times$ Side Fixed Effects &", yes_row, "\\\\"),
    paste("Year $\\times$ Gender Fixed Effects &", yes_row, "\\\\"),
    paste("Year $\\times$ Party Membership Fixed Effects &", yes_row, "\\\\"),
    paste("Year $\\times$ Education Fixed Effects &", yes_row, "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Triple-difference (DDD) coefficients on the interaction Winner $\\times$ Post $\\times$ Previously Represented Gov't.",
      "All two-way interactions among Winner, Post, and the prior-exposure indicator enter the regression; Winner and Post main effects are absorbed by firm and stack-by-year fixed effects.",
      "Reasoning Share is the share of the judgment text devoted to legal reasoning; log(Reasoning Length + 1) is the natural log of one plus the character count of the reasoning section; Case Win Binary indicates the represented side prevailing among decisive cases; Case Fee Win Rate is the fee-based win rate in decisive cases with observed fee allocation.",
      "The sample retains firm-court-case rows where the firm either has no prior government-side administrative appearance in that court or already handled civil cases there before any such appearance, so identification of the triple interaction comes from variation in prior administrative exposure within stack-by-year and court-by-year cells.",
      "Case controls: opposing-counsel presence and plaintiff/defendant entity status. Lawyer controls: standardized practice years and a missing-practice-years indicator.",
      "Year-by-gender, year-by-party-membership, and year-by-education fixed effects absorb time-varying lawyer-composition shocks.",
      "Standard errors clustered by firm and court.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, build_output_name("document_level_strict_ddd_main_table", "tex"))
  writeLines(lines, out_path, useBytes = TRUE)
}

main <- function() {
  dt <- read_document_panel(input_file)
  results <- run_models(dt)
  fee_results <- run_fee_winrate_model(dt)
  fee_results[, column_id := 4L]
  combined <- rbindlist(list(results, fee_results), fill = TRUE)
  write_latex_table(combined)
}

main()
