#!/usr/bin/env Rscript
# city_year_alternative_estimators.R
#
# Compares the Sun-Abraham (2021) interaction-weighted estimator against
# the TWFE and Callaway-Sant'Anna baselines from the main city-year
# table, on the three city-year outcomes (Government Win Rate, Appeal
# Rate, Log(1 + administrative cases)).

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did)
})

# --- locale guard for portable UTF-8 reads (RStudio + Rscript safe) ---
local({
  utf8_locales <- c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")
  for (loc in utf8_locales) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG = loc, LC_ALL = loc, LC_CTYPE = loc, LC_COLLATE = loc)
      break
    }
  }
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
setFixest_nthreads(4)

NEVER_TREATED_SENTINEL <- 0

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
  if (outcome == "government_win_rate") controls <- c(controls, "log_published_document_n")
  controls
}

read_panel <- function() {
  dt <- fread(city_path, encoding = "UTF-8")
  dt[, city_id := .GRP, by = .(province, city)]
  dt[
    ,
    first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else NEVER_TREATED_SENTINEL,
    by = city_id
  ]
  dt[, first_treat_year := as.numeric(first_treat_year)]
  dt
}

est_twfe <- function(panel, outcome) {
  rhs <- paste(c("treatment", preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = panel, cluster = ~ city_id)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  row <- ct[term == "treatment"]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m))
}

est_cs <- function(panel, outcome) {
  ctrl_f <- as.formula(paste("~", paste(preferred_controls(outcome), collapse = " + ")))
  set.seed(2026L)
  obj <- att_gt(yname = outcome, tname = "year", idname = "city_id",
                gname = "first_treat_year", xformla = ctrl_f,
                data = as.data.frame(panel),
                panel = TRUE, allow_unbalanced_panel = FALSE,
                control_group = "nevertreated", anticipation = 0,
                bstrap = TRUE, cband = TRUE, biters = 1000,
                clustervars = "city_id", est_method = "reg",
                base_period = "varying", print_details = FALSE)
  ov <- aggte(obj, type = "simple")
  list(estimate = ov$overall.att, se = ov$overall.se,
       p_value = 2 * pnorm(abs(ov$overall.att / ov$overall.se), lower.tail = FALSE),
       n_obs = nrow(panel))
}

est_sa <- function(panel, outcome) {
  pp <- copy(panel)
  pp[, sa_cohort := fifelse(first_treat_year == 0, 10000L, as.integer(first_treat_year))]
  rhs <- paste(c("sunab(sa_cohort, year)", preferred_controls(outcome)), collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = pp, cluster = ~ city_id)
  agg <- summary(m, agg = "att")
  ct <- as.data.table(coeftable(agg), keep.rownames = "term")
  row <- ct[grepl("ATT|att", term)][1]
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]],
       p_value = row[["Pr(>|t|)"]], n_obs = nobs(m))
}

main <- function() {
  panel <- read_panel()
  outcomes <- c("government_win_rate", "appeal_rate", "log_admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate", "Log(1 + administrative cases)")

  rows <- list()
  for (o in outcomes) {
    rows[[paste(o, "twfe", sep = "_")]] <- est_twfe(panel, o)
    rows[[paste(o, "cs", sep = "_")]] <- est_cs(panel, o)
    rows[[paste(o, "sa", sep = "_")]] <- est_sa(panel, o)
  }

  fmt_cell <- function(r) paste0(fmt_num(r$estimate), stars(r$p_value))
  fmt_se <- function(r) paste0("(", fmt_num(r$se), ")")

  estimators <- c("twfe", "cs", "sa")
  est_labels <- c("TWFE", "CS", "SA")
  ordered_keys <- unlist(lapply(outcomes, function(o) paste(o, estimators, sep = "_")))

  coef_row <- sapply(ordered_keys, function(k) fmt_cell(rows[[k]]))
  se_row <- sapply(ordered_keys, function(k) fmt_se(rows[[k]]))
  obs_row <- sapply(ordered_keys, function(k) fmt_int(rows[[k]]$n_obs))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Alternative Staggered DID Estimators for the City-Year Outcomes}",
    "\\label{tab:city_year_alternative_estimators_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{l*{9}{c}}",
    "\\toprule",
    " & \\multicolumn{3}{c}{Government Win Rate} & \\multicolumn{3}{c}{Appeal Rate} & \\multicolumn{3}{c}{Log(1 + administrative cases)} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}",
    paste(" &", paste(rep(est_labels, length(outcomes)), collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each block of three columns reports the city-year procurement effect from one staggered-DID estimator.",
      "TWFE is the two-way fixed-effects estimator; CS is Callaway and Sant'Anna (2021) with never-treated cities as the comparison group; SA is the Sun and Abraham (2021) interaction-weighted estimator.",
      "All three estimators control for log population, log GDP, and log registered lawyers; log published decisions enters only for the government-win-rate specification.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_alternative_estimators_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
