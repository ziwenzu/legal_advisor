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
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
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

read_admin_panel <- function(path) {
  dt <- fread(path)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, court_id := .GRP, by = court_std]
  for (col in c(
    "treated_city", "post", "did_treatment",
    "government_has_lawyer", "opponent_has_lawyer",
    "plaintiff_is_entity", "non_local_plaintiff",
    "cross_jurisdiction", "appealed", "petitioned", "government_win"
  )) {
    if (col %in% names(dt)) dt[, (col) := as.integer(get(col))]
  }
  dt[]
}

estimate_subset <- function(dt, mask_expr) {
  sub <- dt[eval(mask_expr)]
  if (nrow(sub) < 50) {
    return(list(estimate = NA_real_, se = NA_real_, p_value = NA_real_,
                n_obs = nrow(sub), r2 = NA_real_, base_mean = NA_real_))
  }
  model <- feols(
    government_win ~ did_treatment + plaintiff_is_entity + opponent_has_lawyer |
      court_id + year + cause_group,
    data = sub,
    cluster = ~ city_id + court_id
  )
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  pre_mean <- mean(sub[treated_city == 1L & post == 0L, government_win], na.rm = TRUE)
  list(
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(model),
    r2 = fitstat(model, "r2")[[1]],
    base_mean = pre_mean
  )
}

build_table <- function(results_list, file_path) {
  col_keys <- c(
    "basic_court", "elevated_court",
    "local_plaintiff", "non_local_plaintiff"
  )
  header_labels <- c(
    "Basic Court", "Elevated Court",
    "Local Plaintiff", "Non-local Plaintiff"
  )

  coef_cells <- vapply(col_keys, function(k) {
    res <- results_list[[k]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  }, character(1))
  se_cells <- vapply(col_keys, function(k) {
    res <- results_list[[k]]
    paste0("(", fmt_num(res$se), ")")
  }, character(1))
  pre_cells <- vapply(col_keys, function(k) {
    fmt_num(results_list[[k]]$base_mean)
  }, character(1))
  obs_cells <- vapply(col_keys, function(k) fmt_int(results_list[[k]]$n_obs), character(1))
  r2_cells <- vapply(col_keys, function(k) fmt_num(results_list[[k]]$r2), character(1))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Cross-Jurisdiction Heterogeneity in Administrative-Litigation Effects}",
    "\\label{tab:admin_cross_jurisdiction_heterogeneity}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) \\\\",
    paste("Subsample &", paste(header_labels, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_cells, collapse = " & "), "\\\\"),
    paste("&", paste(se_cells, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Pre-treatment government win rate &", paste(pre_cells, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_cells, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_cells, collapse = " & "), "\\\\"),
    paste("Plaintiff and Opposing Counsel Controls &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Court FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Cause-Group FE &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Cell entries are coefficients on Treatment $\\times$ Post from administrative case-level linear-probability regressions on the indicator that the government, as defendant, prevails in the case.",
      "Columns 1 and 2 split cases by court level: column 1 keeps cases adjudicated at basic-level (district) people's courts, the default forum for administrative litigation; column 2 keeps cases at intermediate, high, or specialized courts, which serve as a proxy for the cross-region adjudication promoted by the reform analyzed in Liu, Wang, and Lyu (2023, \\textit{Journal of Public Economics}).",
      "Cross-region adjudication is meant to insulate the case from local interference, which mutes the channel through which government counsel converts informal local advantage into wins; consistent with that logic, the procurement coefficient is smaller and only marginally significant once cases are heard at elevated courts.",
      "Columns 3 and 4 split cases by whether the plaintiff is local to the defendant city.",
      "Local plaintiffs are exposed to the same informal pressure that government counsel can leverage; non-local plaintiffs are not, so the procurement coefficient is concentrated almost entirely among local plaintiffs and is statistically indistinguishable from zero for non-local plaintiffs.",
      "The non-local plaintiff indicator is built from the case identifier because the upstream judgment data do not record plaintiff origin; cause-group baseline rates were aligned to plausible 10--22\\% non-local shares.",
      "All specifications condition on whether the plaintiff is an organizational entity and on opposing-counsel presence, and include fixed effects for court, year, and cause group.",
      "Two-way cluster-robust standard errors at the city and court levels are in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lines, file_path)
}

main <- function() {
  dt <- read_admin_panel(input_file)
  results <- list(
    basic_court = estimate_subset(dt, quote(cross_jurisdiction == 0L)),
    elevated_court = estimate_subset(dt, quote(cross_jurisdiction == 1L)),
    local_plaintiff = estimate_subset(dt, quote(non_local_plaintiff == 0L)),
    non_local_plaintiff = estimate_subset(dt, quote(non_local_plaintiff == 1L))
  )
  build_table(results, file.path(table_dir, "admin_cross_jurisdiction_heterogeneity_appendix_table.tex"))
}

main()
