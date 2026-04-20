#!/usr/bin/env Rscript
# city_year_randomization_inference.R
#
# (a) Permutation inference for the city-year TWFE estimator on the
#     three headline outcomes: randomly reshuffle first_treat_year
#     across cities 1000 times, recompute the TWFE coefficient, and
#     report the empirical p-value (share of permutation draws with
#     |coef| at least as large as observed).
# (b) Wild cluster bootstrap-t (Cameron-Gelbach-Miller 2008) for the
#     same three TWFE coefficients, using fwildclusterboot.

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
table_dir <- file.path(root_dir, "output", "tables")
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

N_PERM <- 1000L
SEED <- 4242L

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
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("$<0.001$")
  sprintf("%.3f", p)
}

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

read_panel <- function() {
  dt <- fread(city_path)
  dt[, city_id := .GRP, by = .(province, city)]
  dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  dt[, first_treat_year := if (any(treatment == 1L)) min(year[treatment == 1L]) else NA_integer_,
     by = city_id]
  dt
}

twfe_coef <- function(panel, outcome, treat_var = "treatment") {
  rhs <- paste(c(treat_var, preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = panel, cluster = ~ city_id, warn = FALSE, notes = FALSE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == treat_var]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m), model = m)
}

permute_once <- function(panel, outcome, ever_table) {
  shuffled <- copy(ever_table)
  shuffled[, first_treat_year := sample(first_treat_year)]
  pp <- merge(panel[, .(city_id, year)], shuffled, by = "city_id", sort = FALSE)
  pp[, treatment_perm := as.integer(!is.na(first_treat_year) & year >= first_treat_year)]
  pp <- merge(pp, panel[, .SD, .SDcols = c("city_id", "year", outcome,
                                            preferred_controls(outcome))],
              by = c("city_id", "year"))
  rhs <- paste(c("treatment_perm", preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = pp, warn = FALSE, notes = FALSE)
  unname(coef(m)["treatment_perm"])
}

wild_boot <- function(model_obj, panel, outcome) {
  set.seed(SEED)
  bt <- boottest(model_obj,
                 param = "treatment",
                 clustid = "city_id",
                 B = 9999L,
                 type = "rademacher")
  list(p_value = bt$p_val,
       conf_lo = bt$conf_int[1],
       conf_hi = bt$conf_int[2])
}

main <- function() {
  panel <- read_panel()
  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate", "Administrative Cases")
  ever_table <- unique(panel[, .(city_id, first_treat_year)])

  set.seed(SEED)
  results <- list()
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    base <- twfe_coef(panel, o)
    perm_draws <- numeric(N_PERM)
    for (b in seq_len(N_PERM)) {
      perm_draws[b] <- permute_once(panel, o, ever_table)
    }
    p_perm <- mean(abs(perm_draws) >= abs(base$estimate), na.rm = TRUE)
    wb <- wild_boot(base$model, panel, o)
    results[[o]] <- list(
      estimate = base$estimate,
      se = base$se,
      p_analytic = base$p_value,
      p_perm = p_perm,
      p_wild = wb$p_value,
      wild_lo = wb$conf_lo,
      wild_hi = wb$conf_hi,
      perm_draws = perm_draws,
      n_obs = base$n_obs,
      label = outcome_labels[i]
    )
  }

  pdf(file.path(figure_dir, "city_year_permutation_distribution.pdf"),
      width = 9.0, height = 3.4, family = "serif")
  op <- par(mfrow = c(1, 3), bty = "l", mar = c(4.5, 4.0, 2.5, 1.0), las = 1)
  for (o in outcomes) {
    r <- results[[o]]
    h <- hist(r$perm_draws, breaks = 40, col = "gray85", border = "white",
              main = r$label, xlab = "Permutation TWFE coefficient", ylab = "Frequency")
    abline(v = r$estimate, col = "black", lwd = 2)
    legend("topright",
           legend = c(sprintf("Observed = %.3f", r$estimate),
                      sprintf("p_perm = %.3f", r$p_perm)),
           bty = "n", cex = 0.85)
  }
  par(op); dev.off()
  cat("Wrote", file.path(figure_dir, "city_year_permutation_distribution.pdf"), "\n")

  rows <- character(0)
  for (o in outcomes) {
    r <- results[[o]]
    rows <- c(rows, paste(
      r$label, "&",
      fmt_num(r$estimate), "&",
      paste0("(", fmt_num(r$se), ")"), "&",
      fmt_p(r$p_analytic), "&",
      fmt_p(r$p_perm), "&",
      fmt_p(r$p_wild), "&",
      sprintf("[%s, %s]", fmt_num(r$wild_lo), fmt_num(r$wild_hi)),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Permutation Inference and Wild Cluster Bootstrap for City-Year TWFE Estimates}",
    "\\label{tab:city_year_randomization_inference_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    "Outcome & TWFE coef. & SE & Analytic $p$ & Permutation $p$ & Wild $p$ & 95\\% Wild CI \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} TWFE estimates of Treatment $\\times$ Post on the city-year panel.",
      "Analytic $p$-values use city-clustered standard errors.",
      "Permutation $p$-values randomly reshuffle each city's first treatment year (or never-treated status) across cities while holding the empirical adoption-year distribution fixed; %d permutations.",
      "Wild $p$ and 95\\%% Wild CI are from the wild cluster bootstrap-$t$ of Cameron, Gelbach, and Miller (2008) with Rademacher weights and 9,999 draws, clustered by city, computed with \\texttt{fwildclusterboot}."
    ), N_PERM),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_randomization_inference_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
