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

read_panel <- function() {
  city <- fread(city_path)
  city[, city_id := .GRP, by = .(province, city)]
  city[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  city[, province_id := .GRP, by = province]
  city[]
}

estimate <- function(panel, outcome, fe_terms = "city_id + year") {
  rhs <- "treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n"
  f <- as.formula(sprintf("%s ~ %s | %s", outcome, rhs, fe_terms))
  m <- feols(f, data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m), r2 = fitstat(m, "r2")[[1]])
}

main <- function() {
  panel <- read_panel()
  treated_provinces <- unique(panel[ever_treated == 1L, province])
  in_province <- panel[province %in% treated_provinces]

  outcomes <- list(
    list(key = "government_win_rate", label = "Gov.\\ Win Rate"),
    list(key = "appeal_rate", label = "Appeal Rate"),
    list(key = "admin_case_n", label = "Admin.\\ Cases")
  )

  rows <- vector("list", length(outcomes))
  for (i in seq_along(outcomes)) {
    spec <- outcomes[[i]]
    headline <- estimate(panel, spec$key, fe_terms = "city_id + year")
    sample_only <- estimate(in_province, spec$key, fe_terms = "city_id + year")
    province_year <- estimate(in_province, spec$key, fe_terms = "city_id + province_id^year")
    rows[[i]] <- list(
      label = spec$label,
      headline = headline,
      sample = sample_only,
      pyear = province_year
    )
  }

  body <- vapply(rows, function(r) {
    paste(
      r$label, "&",
      paste0(fmt_num(r$headline$estimate), stars(r$headline$p_value)), "&",
      paste0("(", fmt_num(r$headline$se), ")"), "&",
      paste0(fmt_num(r$sample$estimate), stars(r$sample$p_value)), "&",
      paste0("(", fmt_num(r$sample$se), ")"), "&",
      paste0(fmt_num(r$pyear$estimate), stars(r$pyear$p_value)), "&",
      paste0("(", fmt_num(r$pyear$se), ")"),
      "\\\\"
    )
  }, character(1))

  obs_row <- vapply(rows, function(r) {
    paste(
      "Observations &", fmt_int(r$headline$n_obs), "& --",
      "&", fmt_int(r$sample$n_obs), "& --",
      "&", fmt_int(r$pyear$n_obs), "& -- \\\\"
    )
  }, character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Same-Province Donor-Pool Placebo for City-Year Administrative Estimates}",
    "\\label{tab:admin_within_province_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Headline} & \\multicolumn{2}{c}{Same-province sample} & \\multicolumn{2}{c}{+ Province $\\times$ Year Fixed Effects} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
    "Outcome & Coefficient & SE & Coefficient & SE & Coefficient & SE \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each row reports Treatment $\\times$ Post from city-year two-way fixed-effects regressions for one outcome.",
      "Headline columns reproduce the main city-year table.",
      "Same-province sample columns restrict the never-treated control group to cities in provinces that contain at least one procurement-adopting city; specification keeps city and year fixed effects.",
      "+ Province $\\times$ Year Fixed Effects columns add province $\\times$ year fixed effects on the same restricted sample, identifying the procurement effect within province-year cells.",
      "City-year controls: log population, log GDP, log registered lawyers, log court caseload.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out_path <- file.path(table_dir, "admin_within_province_placebo_appendix_table.tex")
  writeLines(lines, out_path)
  cat("Wrote", out_path, "\n")
}

main()
