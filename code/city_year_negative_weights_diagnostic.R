#!/usr/bin/env Rscript
# city_year_negative_weights_diagnostic.R
#
# de Chaisemartin and D'Haultfoeuille (2020) negative-weights
# diagnostic for the city-year TWFE specification on the three
# headline outcomes. Reports the share of negative weights, sum of
# negative weights, sum of positive weights, and the implied
# minimum standard deviation of the homogeneous-effect alternative.

suppressPackageStartupMessages({
  library(data.table)
  library(TwoWayFEWeights)
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
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x, digits = 4) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}
fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

main <- function() {
  panel <- fread(city_path)
  panel[, city_id := .GRP, by = .(province, city)]

  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate", "Administrative Cases")

  rows <- character(0)
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    res <- twowayfeweights(
      data = as.data.frame(panel),
      Y = o,
      G = "city_id",
      T = "year",
      D = "treatment",
      type = "feTR",
      summary_measures = TRUE
    )
    n_pos <- res$nr_plus
    n_neg <- res$nr_minus
    sum_pos <- res$sum_plus
    sum_neg <- res$sum_minus
    sigma_min <- if (!is.null(res$sensibility)) res$sensibility else NA_real_
    rows <- c(rows, paste(
      outcome_labels[i], "&",
      fmt_int(n_pos), "&",
      fmt_int(n_neg), "&",
      fmt_num(sum_pos), "&",
      fmt_num(sum_neg), "&",
      fmt_num(sigma_min),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{de Chaisemartin and D'Haultfoeuille Negative-Weights Diagnostic for the City-Year TWFE Specification}",
    "\\label{tab:city_year_negative_weights_diagnostic_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Outcome & Positive ATT(g,t) & Negative ATT(g,t) & $\\sum w^{+}$ & $\\sum w^{-}$ & $\\underline{\\sigma}_{fe}$ \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Diagnostic from de Chaisemartin and D'Haultfoeuille (2020) decomposes the city-year TWFE coefficient on $\\text{treatment}$ into a weighted sum of group-by-time average treatment effects $\\text{ATT}(g,t)$.",
      "Positive ATT(g,t) and Negative ATT(g,t) report the number of (group, time) cells receiving positive and negative weights; $\\sum w^{+}$ and $\\sum w^{-}$ report the sum of positive and negative weights.",
      "$\\underline{\\sigma}_{fe}$ is the smallest standard deviation of treatment-effect heterogeneity under which the TWFE estimator could have the opposite sign from the average treatment effect.",
      "Computed with the \\texttt{TwoWayFEWeights} R package on the city-year panel without covariates."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_negative_weights_diagnostic_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
