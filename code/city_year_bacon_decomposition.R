#!/usr/bin/env Rscript
# city_year_bacon_decomposition.R
#
# Goodman-Bacon (2021) decomposition of the city-year TWFE coefficient
# into the underlying 2x2 comparisons (treated-vs-untreated,
# earlier-vs-later treated, later-vs-earlier treated, treated-vs-already-treated)
# for each of the three headline outcomes. Reports a table of weight
# shares and average effects per comparison type, plus a scatter
# figure with one panel per outcome.

suppressPackageStartupMessages({
  library(data.table)
  library(bacondecomp)
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

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

main <- function() {
  panel <- fread(city_path)
  panel[, city_id := .GRP, by = .(province, city)]
  panel <- as.data.frame(panel)

  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate",
                      "Administrative Cases")

  bacon_results <- list()
  for (o in outcomes) {
    res <- bacon(as.formula(sprintf("%s ~ treatment", o)),
                 data = panel, id_var = "city_id", time_var = "year")
    bacon_results[[o]] <- as.data.table(res)
  }

  type_palette <- c(
    "Earlier vs Later Treated"   = "#1B9E77",
    "Later vs Earlier Treated"   = "#D95F02",
    "Treated vs Untreated"       = "#7570B3",
    "Treated vs Already Treated" = "#E7298A",
    "Both Treated"               = "#66A61E"
  )
  type_pchs <- c(
    "Earlier vs Later Treated"   = 16,
    "Later vs Earlier Treated"   = 17,
    "Treated vs Untreated"       = 15,
    "Treated vs Already Treated" = 18,
    "Both Treated"               = 8
  )
  fallback_palette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                        "#66A61E", "#E6AB02", "#A6761D", "#666666")
  fallback_pchs <- c(16, 17, 15, 18, 8, 4, 3, 6)
  resolve_color <- function(t) if (t %in% names(type_palette)) type_palette[[t]] else NA_character_
  resolve_pch <- function(t) if (t %in% names(type_pchs)) type_pchs[[t]] else NA_integer_

  all_types <- unique(unlist(lapply(bacon_results, function(bd) {
    type_col <- if ("type" %in% names(bd)) "type" else "subgroup"
    as.character(unique(bd[[type_col]]))
  })))
  for (k in seq_along(all_types)) {
    t <- all_types[k]
    if (is.na(resolve_color(t))) {
      type_palette[[t]] <- fallback_palette[(k - 1) %% length(fallback_palette) + 1]
      type_pchs[[t]]   <- fallback_pchs[(k - 1) %% length(fallback_pchs) + 1]
    }
  }

  pdf(file.path(figure_dir, "city_year_bacon_decomposition.pdf"),
      width = 9.6, height = 3.6, family = "serif")
  op <- par(mfrow = c(1, 3), bty = "l", mar = c(4.5, 4.4, 2.5, 1.0), las = 1)
  on.exit({par(op); dev.off()}, add = TRUE)
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    bd <- bacon_results[[o]]
    type_col <- if ("type" %in% names(bd)) "type" else "subgroup"
    types <- as.character(unique(bd[[type_col]]))
    plot(bd$weight, bd$estimate, type = "n",
         xlab = "Weight in TWFE",
         ylab = paste("2x2 ATE:", outcome_labels[i]),
         main = outcome_labels[i])
    abline(h = 0, col = "gray70", lty = 2)
    for (k in seq_along(types)) {
      t <- types[k]
      sub <- bd[bd[[type_col]] == t]
      points(sub$weight, sub$estimate,
             pch = type_pchs[[t]], col = type_palette[[t]],
             bg = type_palette[[t]], cex = 1.3)
    }
    if (i == 1L) {
      legend("topright", legend = types,
             pch = sapply(types, function(t) type_pchs[[t]]),
             col = sapply(types, function(t) type_palette[[t]]),
             pt.bg = sapply(types, function(t) type_palette[[t]]),
             cex = 0.78, bty = "n")
    }
  }
  cat("Wrote", file.path(figure_dir, "city_year_bacon_decomposition.pdf"), "\n")

  rows <- character(0)
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    bd <- bacon_results[[o]]
    type_col <- if ("type" %in% names(bd)) "type" else "subgroup"
    summary_dt <- bd[, .(weight_sum = sum(weight),
                         avg_estimate = sum(weight * estimate) / sum(weight),
                         n_pairs = .N),
                     by = type_col]
    twfe_implied <- bd[, sum(weight * estimate)]
    for (k in seq_len(nrow(summary_dt))) {
      rows <- c(rows, paste(
        if (k == 1L) outcome_labels[i] else "",
        "&", summary_dt[[type_col]][k],
        "&", summary_dt$n_pairs[k],
        "&", fmt_num(summary_dt$weight_sum[k]),
        "&", fmt_num(summary_dt$avg_estimate[k]),
        "\\\\"
      ))
    }
    rows <- c(rows, paste(
      "&", "\\textit{Implied TWFE coef.}", "& &", fmt_num(1.0), "&", fmt_num(twfe_implied), "\\\\"
    ))
    rows <- c(rows, "\\addlinespace")
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Goodman-Bacon Decomposition of City-Year TWFE Coefficients}",
    "\\label{tab:city_year_bacon_decomposition_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{llccc}",
    "\\toprule",
    "Outcome & Comparison type & 2x2 pairs & $\\sum$ weight & Weighted avg.\\ effect \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Goodman-Bacon (2021) decomposition of the city-year TWFE coefficient on $\\text{treatment}$ into a weighted sum of 2-by-2 difference-in-differences comparisons.",
      "Weights are non-negative and sum to one. The implied TWFE coefficient row is the weighted average of all comparison effects.",
      "Computed without controls using \\texttt{bacondecomp::bacon} on the 2,256 city-year observations."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_bacon_decomposition_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
