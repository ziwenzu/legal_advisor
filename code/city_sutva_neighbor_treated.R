#!/usr/bin/env Rscript
# city_sutva_neighbor_treated.R
#
# SUTVA / spillover diagnostic at the city-year level. Constructs
# share_neighbor_treated = (number of other treated cities in the
# same province up to year t) / (number of other cities in the
# province), and adds it to the city-year TWFE for each of the
# three headline outcomes. If the procurement effect operates
# partly through within-province spillovers, the own-city
# Treatment x Post coefficient should attenuate when the spillover
# share is included, and the spillover coefficient itself should
# be informative.

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
city_path <- file.path(root_dir, "data", "city_year_panel.csv")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

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
fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

main <- function() {
  panel <- fread(city_path)
  panel[, city_id := .GRP, by = .(province, city)]
  prov_size <- panel[, .(prov_n = uniqueN(city_id)), by = province]
  panel <- merge(panel, prov_size, by = "province", sort = FALSE)
  setorder(panel, province, city, year)
  panel[, treated_in_province := sum(treatment), by = .(province, year)]
  panel[, share_neighbor_treated := pmax(0, (treated_in_province - treatment)) /
          pmax(1, prov_n - 1)]

  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate", "Administrative Cases")

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")
  est <- function(outcome, with_neighbor) {
    rhs_terms <- c("treatment", preferred_controls(outcome))
    if (with_neighbor) rhs_terms <- c(rhs_terms, "share_neighbor_treated")
    rhs <- paste(rhs_terms, collapse = " + ")
    m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
               data = panel, cluster = ~ city_id)
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    list(
      own = ct[term == "treatment"],
      neighbor = if (with_neighbor) ct[term == "share_neighbor_treated"] else NULL,
      n_obs = nobs(m),
      r2 = fitstat(m, "r2")[[1]]
    )
  }

  rows <- character(0)
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    base <- est(o, FALSE)
    spil <- est(o, TRUE)
    own_base <- list(estimate = base$own[["Estimate"]], se = base$own[["Std. Error"]],
                     p_value = base$own[["Pr(>|t|)"]])
    own_spil <- list(estimate = spil$own[["Estimate"]], se = spil$own[["Std. Error"]],
                     p_value = spil$own[["Pr(>|t|)"]])
    nei <- list(estimate = spil$neighbor[["Estimate"]],
                se = spil$neighbor[["Std. Error"]],
                p_value = spil$neighbor[["Pr(>|t|)"]])
    rows <- c(rows, paste(
      outcome_labels[i], "&",
      fmt_cell(own_base), "&", fmt_se(own_base), "&",
      fmt_cell(own_spil), "&", fmt_se(own_spil), "&",
      fmt_cell(nei), "&", fmt_se(nei), "&",
      fmt_int(base$n_obs),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{SUTVA Diagnostic: Spillovers from Other Treated Cities in the Same Province}",
    "\\label{tab:city_sutva_neighbor_treated}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Own Treatment $\\times$ Post} & \\multicolumn{2}{c}{Own Treatment $\\times$ Post} & \\multicolumn{2}{c}{Share neighbour treated} & \\\\",
    " & \\multicolumn{2}{c}{(no neighbour control)} & \\multicolumn{2}{c}{(with neighbour control)} & & & \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    "Outcome & Estimate & SE & Estimate & SE & Estimate & SE & Observations \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each row reports the city-year procurement effect on one outcome before and after adding a same-province neighbour-treated share to the right-hand side.",
      "Share neighbour treated equals the count of other cities in the same province whose procurement contract is in force in year $t$, divided by the number of other cities in the province.",
      "All specifications include city and year fixed effects and the standard city-year controls (log population, log GDP, log registered lawyers; log court caseload added only for the government-win-rate specification), clustered by city.",
      "A coefficient on the spillover share that absorbs a meaningful share of the own coefficient is consistent with within-province spillovers and a SUTVA violation across cities.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_sutva_neighbor_treated_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
