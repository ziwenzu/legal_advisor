#!/usr/bin/env Rscript
# admin_mechanism_plaintiff_entry.R
#
# Plaintiff selection / chilling-effect channel for the city-year
# administrative panel. For each city-year cell, computes the
# share of cases brought by (a) entity plaintiffs, (b) individual
# plaintiffs, (c) non-local plaintiffs, (d) entity-and-non-local
# plaintiffs, and (e) the case count subdivided by entity status,
# and runs the standard city-year TWFE on each derived outcome.
# A flat overall case count combined with shifts in plaintiff
# composition is consistent with selection at the plaintiff
# margin rather than uniform deterrence.

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
admin_path <- file.path(root_dir, "data", "admin_case_level.csv")
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

main <- function() {
  admin <- fread(admin_path)
  city <- fread(city_path)
  agg <- admin[
    ,
    .(
      n_cases = .N,
      entity_share = mean(plaintiff_is_entity),
      individual_share = mean(plaintiff_is_entity == 0L),
      non_local_share = mean(non_local_plaintiff),
      entity_nonlocal_share = mean(plaintiff_is_entity == 1L & non_local_plaintiff == 1L),
      n_entity = sum(plaintiff_is_entity == 1L),
      n_individual = sum(plaintiff_is_entity == 0L),
      gov_win_entity = mean(government_win[plaintiff_is_entity == 1L]),
      gov_win_individual = mean(government_win[plaintiff_is_entity == 0L])
    ),
    by = .(province, city, year)
  ]
  panel <- city[agg, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_id := .GRP, by = .(province, city)]

  est <- function(outcome) {
    m <- feols(
      as.formula(sprintf("%s ~ treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n | city_id + year", outcome)),
      data = panel, cluster = ~ city_id
    )
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "treatment"]
    list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
         p_value = row[["Pr(>|t|)"]], n_obs = nobs(m))
  }

  outcomes <- list(
    list(label = "Entity plaintiff share", col = "entity_share"),
    list(label = "Individual plaintiff share", col = "individual_share"),
    list(label = "Non-local plaintiff share", col = "non_local_share"),
    list(label = "Entity \\& non-local plaintiff share", col = "entity_nonlocal_share"),
    list(label = "Entity-plaintiff case count", col = "n_entity"),
    list(label = "Individual-plaintiff case count", col = "n_individual"),
    list(label = "Gov.\\ win rate (entity plaintiffs)", col = "gov_win_entity"),
    list(label = "Gov.\\ win rate (individual plaintiffs)", col = "gov_win_individual")
  )

  rows <- character(0)
  for (spec in outcomes) {
    sub <- panel[!is.na(get(spec$col))]
    m <- feols(
      as.formula(sprintf("%s ~ treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n | city_id + year", spec$col)),
      data = sub, cluster = ~ city_id
    )
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    row <- ct[term == "treatment"]
    rows <- c(rows, paste(
      spec$label, "&",
      paste0(fmt_num(row[["Estimate"]]), stars(row[["Pr(>|t|)"]])), "&",
      paste0("(", fmt_num(row[["Std. Error"]]), ")"), "&",
      fmt_int(nobs(m)),
      "\\\\"
    ))
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Plaintiff Selection: Procurement Effects on Plaintiff Composition and Subgroup Win Rates}",
    "\\label{tab:admin_mechanism_plaintiff_entry}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    "Outcome & Coefficient & SE & Observations \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each row reports Treatment $\\times$ Post from a city-year two-way fixed-effects regression with city and year fixed effects and the standard city-year controls (log population, log GDP, log registered lawyers, log court caseload), clustered by city.",
      "Outcomes are computed by aggregating administrative cases to the city-year level: shares are within-cell means; case counts are within-cell sums; subgroup win rates are within-cell means of the government-win indicator restricted to the indicated plaintiff subgroup.",
      "City-year cells with zero cases in a subgroup are dropped for the corresponding subgroup-conditional outcome.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "admin_mechanism_plaintiff_entry_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
