#!/usr/bin/env Rscript
# family_wise_pvalues.R
#
# Family-wise adjusted p-values across the headline outcomes in
# three families:
#   (i)   city-year:  government_win_rate, appeal_rate, admin_case_n
#   (ii)  document:   legal_reasoning_share, log_legal_reasoning_length_chars,
#                     case_win_binary, case_win_rate_fee
#   (iii) firm-year:  civil_win_rate_mean, avg_filing_to_hearing_days,
#                     civil_win_rate_fee_mean, enterprise_share, log_firm_size
#
# For each family we compute the analytic p-value, the Bonferroni-Holm
# step-down adjusted p-value (Holm 1979), and -- when feasible -- the
# Romano-Wolf (2005) step-down p-value via wild cluster bootstrap-t
# with fwildclusterboot. Where the bootstrap is not feasible the
# Romano-Wolf cell is omitted from the table.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(fwildclusterboot)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
city_path <- file.path(root_dir, "data", "city_year_panel.csv")
doc_path <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
firm_path <- file.path(root_dir, "data", "firm_level.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

N_BOOT <- 999L
SEED <- 1234L

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
fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) return("$<0.001$")
  sprintf("%.3f", p)
}

romano_wolf <- function(t_orig, t_boot) {
  K <- length(t_orig)
  ord <- order(-abs(t_orig))
  t_o <- abs(t_orig[ord])
  t_b <- abs(t_boot[ord, , drop = FALSE])
  p_adj <- numeric(K)
  for (k in seq_len(K)) {
    max_t <- apply(t_b[k:K, , drop = FALSE], 2, max)
    p_raw <- mean(max_t >= t_o[k])
    p_adj[k] <- if (k == 1) p_raw else max(p_raw, p_adj[k - 1])
  }
  out <- numeric(K)
  out[ord] <- p_adj
  out
}

# --- City-year family ---
cy_panel <- fread(city_path)
cy_panel[, city_id := .GRP, by = .(province, city)]
preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

cy_outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
cy_models <- lapply(cy_outcomes, function(o) {
  rhs <- paste(c("treatment", preferred_controls(o)), collapse = " + ")
  feols(as.formula(sprintf("%s ~ %s | city_id + year", o, rhs)),
        data = cy_panel, cluster = ~ city_id, fixef.rm = "none")
})
cy_t_orig <- sapply(cy_models, function(m) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  row[["Estimate"]] / row[["Std. Error"]]
})
cy_p_analytic <- sapply(cy_models, function(m) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  row[["Pr(>|t|)"]]
})
cy_t_boot <- matrix(NA_real_, nrow = length(cy_models), ncol = N_BOOT)
for (k in seq_along(cy_models)) {
  set.seed(SEED + k)
  bt <- boottest(cy_models[[k]],
                 param = "treatment",
                 clustid = "city_id",
                 B = N_BOOT,
                 type = "rademacher",
                 impose_null = TRUE)
  cy_t_boot[k, ] <- as.numeric(bt$t_boot)
}
cy_p_rw <- romano_wolf(cy_t_orig, cy_t_boot)
cy_p_holm <- p.adjust(cy_p_analytic, method = "holm")

# --- Document family ---
doc <- fread(doc_path)
doc[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
doc[, cause_side_fe := sprintf("%s__%s", cause, side)]
doc[, lawyer_practice_years_obs := as.integer(!is.na(lawyer_practice_years))]
practice_mean <- mean(doc$lawyer_practice_years, na.rm = TRUE)
practice_sd <- sd(doc$lawyer_practice_years, na.rm = TRUE)
doc[, lawyer_practice_years_std := (lawyer_practice_years - practice_mean) / practice_sd]
doc[is.na(lawyer_practice_years_std), lawyer_practice_years_std := 0]
doc[, year_gender_fe := sprintf("%s__%d", year, lawyer_gender)]

doc_specs <- list(
  list(label = "legal_reasoning_share", outcome = "legal_reasoning_share",
       sample_filter = NULL),
  list(label = "log_legal_reasoning_length_chars", outcome = "log_legal_reasoning_length_chars",
       sample_filter = NULL),
  list(label = "case_win_binary", outcome = "case_win_binary",
       sample_filter = quote(case_decisive == 1L)),
  list(label = "case_win_rate_fee", outcome = "case_win_rate_fee",
       sample_filter = quote(case_decisive == 1L & !is.na(case_win_rate_fee)))
)

doc_t_orig <- numeric(length(doc_specs))
doc_p_analytic <- numeric(length(doc_specs))
for (i in seq_along(doc_specs)) {
  spec <- doc_specs[[i]]
  work <- if (is.null(spec$sample_filter)) doc[!is.na(get(spec$outcome))]
          else doc[!is.na(get(spec$outcome)) & eval(spec$sample_filter)]
  rhs <- "did_treatment + opponent_has_lawyer + plaintiff_party_is_entity + defendant_party_is_entity + lawyer_practice_years_std + lawyer_practice_years_obs"
  f <- as.formula(sprintf("%s ~ %s | firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe", spec$outcome, rhs))
  m <- feols(f, data = work, cluster = ~ firm_id + stack_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  doc_t_orig[i] <- row[["Estimate"]] / row[["Std. Error"]]
  doc_p_analytic[i] <- row[["Pr(>|t|)"]]
}
doc_p_holm <- p.adjust(doc_p_analytic, method = "holm")

# --- Firm-year family ---
fm <- fread(firm_path)
fm[, stack_firm_fe := sprintf("%s__%s", stack_id, firm_id)]
fm[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
fm[, event_time_window := fifelse(event_time < -5, NA_real_,
                          fifelse(event_time > 5, NA_real_, event_time))]
fm[civil_case_n > 0, enterprise_share := enterprise_case_n / civil_case_n]
fm[firm_size > 0, log_firm_size := log(firm_size)]

firm_specs <- list(
  list(outcome = "civil_win_rate_mean", filter = quote(civil_decisive_case_n > 0)),
  list(outcome = "avg_filing_to_hearing_days", filter = quote(civil_case_n > 0)),
  list(outcome = "civil_win_rate_fee_mean", filter = quote(civil_fee_decisive_case_n > 0)),
  list(outcome = "enterprise_share", filter = quote(civil_case_n > 0)),
  list(outcome = "log_firm_size", filter = quote(!is.na(log_firm_size)))
)
firm_t_orig <- numeric(length(firm_specs))
firm_p_analytic <- numeric(length(firm_specs))
for (i in seq_along(firm_specs)) {
  spec <- firm_specs[[i]]
  work <- fm[!is.na(get(spec$outcome)) & !is.na(event_time_window) &
             eval(spec$filter)]
  m <- feols(as.formula(sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", spec$outcome)),
             data = work, cluster = ~ stack_id + firm_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  firm_t_orig[i] <- row[["Estimate"]] / row[["Std. Error"]]
  firm_p_analytic[i] <- row[["Pr(>|t|)"]]
}
firm_p_holm <- p.adjust(firm_p_analytic, method = "holm")

# --- Build table ---
fmt_p_star <- function(p_raw, p_holm, p_rw = NULL) {
  rw_cell <- if (is.null(p_rw)) "" else paste0(fmt_p(p_rw), stars(p_rw))
  paste(
    fmt_p(p_raw), "&",
    paste0(fmt_p(p_holm), stars(p_holm)), "&",
    rw_cell
  )
}

cy_labels <- c("Government Win Rate", "Appeal Rate", "Administrative Cases")
doc_labels <- c("Reasoning Share", "log(Reasoning Length + 1)",
                "Case Win Binary", "Case Fee Win Rate")
firm_labels <- c("Civil Win Rate", "Average Hearing Days", "Civil Fee Win Rate",
                 "Enterprise Share", "Log Firm Size")

build_block <- function(panel_label, labels, p_analytic, p_holm, p_rw) {
  rows <- character(0)
  rows <- c(rows,
            sprintf("\\multicolumn{4}{l}{\\textit{%s}} \\\\", panel_label),
            "\\addlinespace")
  for (k in seq_along(labels)) {
    rows <- c(rows, paste(
      labels[k], "&",
      fmt_p_star(p_analytic[k], p_holm[k],
                 if (is.null(p_rw)) NULL else p_rw[k]),
      "\\\\"
    ))
  }
  rows
}

lines <- c(
  "\\begin{table}[!htbp]",
  "\\setlength{\\abovecaptionskip}{0pt}",
  "\\centering",
  "\\caption{Family-Wise Adjusted $p$-values across the Headline Outcomes}",
  "\\label{tab:family_wise_pvalues_appendix}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & Analytic $p$ & Bonferroni-Holm $p$ & Romano-Wolf $p$ \\\\",
  "\\midrule",
  build_block("Family A. City-year", cy_labels,
              cy_p_analytic, cy_p_holm, cy_p_rw),
  "\\addlinespace",
  build_block("Family B. Document-level", doc_labels,
              doc_p_analytic, doc_p_holm, NULL),
  "\\addlinespace",
  build_block("Family C. Firm-year", firm_labels,
              firm_p_analytic, firm_p_holm, NULL),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  sprintf(paste(
    "\\item \\textit{Note:} Each row corresponds to one Treatment $\\times$ Post (or Winner $\\times$ Post) coefficient from the headline specification.",
    "Analytic $p$ is the cluster-robust two-sided $p$-value from the regression.",
    "Bonferroni-Holm $p$ adjusts within each family by the step-down method of Holm (1979).",
    "Romano-Wolf $p$ is the free step-down adjustment of Romano and Wolf (2005) using %d wild cluster bootstrap-$t$ draws per hypothesis (Cameron, Gelbach, and Miller 2008) clustered at the city level."
  ), N_BOOT),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

out <- file.path(table_dir, "family_wise_pvalues_appendix_table.tex")
writeLines(lines, out)
cat("Wrote", out, "\n")
