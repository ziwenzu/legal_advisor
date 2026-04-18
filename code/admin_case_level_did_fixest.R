#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
input_file <- Sys.getenv(
  "ADMIN_CASE_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "admin_case_level.csv")
)
table_dir <- file.path(root_dir, "output", "tables")
figure_dir <- file.path(root_dir, "output", "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

stars <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p_value) {
  if (length(p_value) == 0 || is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

read_admin_panel <- function(path) {
  if (grepl("\\.parquet$", path)) {
    arrow_avail <- requireNamespace("arrow", quietly = TRUE)
    if (!arrow_avail) {
      stop("Reading parquet requires the 'arrow' package; install it or pass the CSV file via ADMIN_CASE_INPUT_FILE.")
    }
    dt <- as.data.table(arrow::read_parquet(path))
  } else {
    dt <- fread(path)
  }

  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, court_id := .GRP, by = court_std]
  dt[, court_year_fe := sprintf("%s__%s", court_id, year)]
  dt[, cause_year_fe := sprintf("%s__%s", cause_group, year)]

  dt[, treated_city := as.integer(treated_city)]
  dt[, post := as.integer(post)]
  dt[, did_treatment := as.integer(did_treatment)]

  for (col in c("government_has_lawyer", "opponent_has_lawyer",
                "plaintiff_is_entity", "appealed", "petitioned", "government_win",
                "withdraw_case", "end_case")) {
    if (col %in% names(dt)) {
      dt[, (col) := as.integer(get(col))]
    }
  }

  dt[, event_time_window := fifelse(
    is.na(event_time) | event_time < -3 | event_time > 5,
    NA_integer_,
    as.integer(event_time)
  )]

  setorder(dt, city_id, year, case_no)
  dt[]
}

build_formula <- function(outcome_name, extra_rhs = character(0)) {
  rhs_terms <- c(
    "did_treatment",
    "plaintiff_is_entity",
    extra_rhs
  )
  fe_terms <- "court_id + year + cause_group"
  as.formula(sprintf("%s ~ %s | %s", outcome_name, paste(rhs_terms, collapse = " + "), fe_terms))
}

estimate_admin_did <- function(dt, outcome_name, extra_rhs = character(0)) {
  formula_obj <- build_formula(outcome_name, extra_rhs)
  feols(formula_obj, data = dt, cluster = ~ city_id + court_id)
}

extract_did_coef <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  did_row <- ct[term == "did_treatment"]
  pull <- function(term_candidates) {
    row <- ct[term %in% term_candidates]
    if (nrow(row) == 0) return(list(est = NA_real_, se = NA_real_, p = NA_real_))
    list(est = row[["Estimate"]][1], se = row[["Std. Error"]][1], p = row[["Pr(>|t|)"]][1])
  }
  gov_lvl <- pull("government_has_lawyer")
  gov_post <- pull(c("government_has_lawyer:post", "post:government_has_lawyer"))
  opp_lvl <- pull("opponent_has_lawyer")
  opp_post <- pull(c("opponent_has_lawyer:post", "post:opponent_has_lawyer"))
  list(
    estimate = did_row[["Estimate"]],
    se = did_row[["Std. Error"]],
    p_value = did_row[["Pr(>|t|)"]],
    gov_level_estimate = gov_lvl$est,
    gov_level_se = gov_lvl$se,
    gov_level_p = gov_lvl$p,
    gov_post_estimate = gov_post$est,
    gov_post_se = gov_post$se,
    gov_post_p = gov_post$p,
    opp_level_estimate = opp_lvl$est,
    opp_level_se = opp_lvl$se,
    opp_level_p = opp_lvl$p,
    opp_post_estimate = opp_post$est,
    opp_post_se = opp_post$se,
    opp_post_p = opp_post$p,
    n_obs = nobs(model),
    r2 = fitstat(model, "r2")[[1]]
  )
}

build_appendix_table <- function(results_list, file_path) {
  spec_keys <- c(
    "government_win__baseline",
    "government_win__gov_counsel",
    "government_win__gov_counsel_post",
    "government_win__both"
  )

  cell_with_se <- function(est, se, p) {
    if (is.na(est)) return(c("--", "--"))
    c(paste0(fmt_num(est), stars(p)), paste0("(", fmt_num(se), ")"))
  }

  coef_row <- sapply(spec_keys, function(key) {
    res <- results_list[[key]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(spec_keys, function(key) {
    res <- results_list[[key]]
    paste0("(", fmt_num(res$se), ")")
  })

  gov_lvl_cells <- lapply(spec_keys, function(key) {
    res <- results_list[[key]]
    cell_with_se(res$gov_level_estimate, res$gov_level_se, res$gov_level_p)
  })
  gov_post_cells <- lapply(spec_keys, function(key) {
    res <- results_list[[key]]
    cell_with_se(res$gov_post_estimate, res$gov_post_se, res$gov_post_p)
  })
  opp_lvl_cells <- lapply(spec_keys, function(key) {
    res <- results_list[[key]]
    cell_with_se(res$opp_level_estimate, res$opp_level_se, res$opp_level_p)
  })
  opp_post_cells <- lapply(spec_keys, function(key) {
    res <- results_list[[key]]
    cell_with_se(res$opp_post_estimate, res$opp_post_se, res$opp_post_p)
  })

  obs_row <- sapply(spec_keys, function(key) fmt_int(results_list[[key]]$n_obs))
  r2_row <- sapply(spec_keys, function(key) fmt_num(results_list[[key]]$r2))

  gov_counsel_yes <- c("", "Yes", "Yes", "Yes")
  gov_counsel_post_yes <- c("", "", "Yes", "Yes")
  opp_counsel_yes <- c("", "", "", "Yes")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Disentangling Pre-existing and New Government Counsel in Administrative Wins}",
    "\\label{tab:admin_case_level_lawyer_specs}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) \\\\",
    "Outcome & Government Win & Government Win & Government Win & Government Win \\\\",
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Government has counsel &",
          paste(sapply(gov_lvl_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(gov_lvl_cells, `[`, 2), collapse = " & "), "\\\\"),
    paste("Government counsel $\\times$ Post &",
          paste(sapply(gov_post_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(gov_post_cells, `[`, 2), collapse = " & "), "\\\\"),
    paste("Opposing counsel &",
          paste(sapply(opp_lvl_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(opp_lvl_cells, `[`, 2), collapse = " & "), "\\\\"),
    paste("Opposing counsel $\\times$ Post &",
          paste(sapply(opp_post_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(opp_post_cells, `[`, 2), collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Government counsel control &", paste(gov_counsel_yes, collapse = " & "), "\\\\"),
    paste("Government counsel $\\times$ Post control &", paste(gov_counsel_post_yes, collapse = " & "), "\\\\"),
    paste("Opposing counsel controls &", paste(opp_counsel_yes, collapse = " & "), "\\\\"),
    paste("Court FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Cause-Group FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients from administrative case-level linear-probability regressions on the indicator for the government prevailing in the case.",
      "Column 1 reports the baseline procurement effect.",
      "Column 2 adds the level dummy for whether the government party arrived with counsel: this absorbs the long-standing fact that some pre-procurement governments already retained lawyers, so the Treatment $\\times$ Post coefficient now isolates effects beyond pure counsel presence.",
      "Column 3 further interacts that counsel dummy with the post-treatment indicator, separating the pre-procurement counsel premium from the additional gain when a city procures legal services on top of existing counsel.",
      "Column 4 adds the parallel level and post interaction for opposing counsel, so the table jointly reports the four lawyer-related channels.",
      "All specifications condition on whether the plaintiff is an organizational entity and include fixed effects for court, year, and cause group.",
      "Two-way cluster-robust standard errors at the city and court levels are in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_plaintiff_heterogeneity_table <- function(results_list, file_path) {
  col_keys <- c(
    "entity_baseline", "entity_with_opp",
    "individual_baseline", "individual_with_opp"
  )

  coef_row <- sapply(col_keys, function(k) {
    res <- results_list[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(col_keys, function(k) {
    res <- results_list[[k]]
    paste0("(", fmt_num(res$se), ")")
  })

  opp_lvl_cells <- lapply(col_keys, function(k) {
    res <- results_list[[k]]
    if (is.na(res$opp_level_estimate)) {
      c("--", "--")
    } else {
      c(paste0(fmt_num(res$opp_level_estimate), stars(res$opp_level_p)),
        paste0("(", fmt_num(res$opp_level_se), ")"))
    }
  })

  obs_row <- sapply(col_keys, function(k) fmt_int(results_list[[k]]$n_obs))
  r2_row <- sapply(col_keys, function(k) fmt_num(results_list[[k]]$r2))
  opp_yes <- c("", "Yes", "", "Yes")
  sample_row <- c("Entity plaintiff", "Entity plaintiff", "Individual plaintiff", "Individual plaintiff")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Heterogeneity in Procurement Effects by Plaintiff Type}",
    "\\label{tab:admin_plaintiff_heterogeneity}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) \\\\",
    paste("Subsample &", paste(sample_row, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Opposing counsel &",
          paste(sapply(opp_lvl_cells, `[`, 1), collapse = " & "), "\\\\"),
    paste("&", paste(sapply(opp_lvl_cells, `[`, 2), collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Opposing counsel control &", paste(opp_yes, collapse = " & "), "\\\\"),
    paste("Court FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Cause-Group FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients from administrative case-level linear-probability regressions on the indicator that the government, as defendant, prevails in the case.",
      "Columns 1--2 estimate the procurement effect on the subset of cases brought by an organizational entity plaintiff;",
      "columns 3--4 estimate it on the complementary subset of cases brought by individual plaintiffs.",
      "Even-numbered columns add an indicator for whether the opposing party appears with counsel.",
      "Adversarial counsel response means opposing parties are more likely to retain counsel after procurement, so this control absorbs part of the post-treatment lawyer presence; once it is included, the procurement coefficient on government wins moves up because the dampening role of opposing counsel is partialled out.",
      "All specifications include fixed effects for court, year, and cause group.",
      "Two-way cluster-robust standard errors at the city and court levels are in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  dt <- read_admin_panel(input_file)

  spec_table <- list(
    list(name = "government_win__baseline", outcome = "government_win", extra = character(0)),
    list(name = "government_win__gov_counsel", outcome = "government_win", extra = "government_has_lawyer"),
    list(name = "government_win__gov_counsel_post",
         outcome = "government_win",
         extra = c("government_has_lawyer", "government_has_lawyer:post")),
    list(name = "government_win__both",
         outcome = "government_win",
         extra = c(
           "government_has_lawyer",
           "government_has_lawyer:post",
           "opponent_has_lawyer",
           "opponent_has_lawyer:post"
         ))
  )

  results_list <- list()
  for (spec in spec_table) {
    model <- estimate_admin_did(dt, spec$outcome, extra_rhs = spec$extra)
    results_list[[spec$name]] <- extract_did_coef(model)
  }

  build_appendix_table(
    results_list = results_list,
    file_path = file.path(table_dir, "admin_case_level_lawyer_specs_appendix_table.tex")
  )

  plaintiff_specs <- list(
    list(name = "entity_baseline", subset = quote(plaintiff_is_entity == 1L), extra = character(0)),
    list(name = "entity_with_opp", subset = quote(plaintiff_is_entity == 1L), extra = "opponent_has_lawyer"),
    list(name = "individual_baseline", subset = quote(plaintiff_is_entity == 0L), extra = character(0)),
    list(name = "individual_with_opp", subset = quote(plaintiff_is_entity == 0L), extra = "opponent_has_lawyer")
  )
  plaintiff_results <- list()
  for (spec in plaintiff_specs) {
    sub_dt <- dt[eval(spec$subset)]
    model <- estimate_admin_did(sub_dt, "government_win", extra_rhs = spec$extra)
    plaintiff_results[[spec$name]] <- extract_did_coef(model)
  }
  build_plaintiff_heterogeneity_table(
    results_list = plaintiff_results,
    file_path = file.path(table_dir, "admin_plaintiff_heterogeneity_appendix_table.tex")
  )
}

main()
