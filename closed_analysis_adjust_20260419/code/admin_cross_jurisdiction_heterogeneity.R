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
input_file <- file.path(root_dir, "data", "admin_case_level.csv")
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
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.001) return("$<0.001$")
  sprintf("%.3f", p)
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

coef_difference_pvalue <- function(res_a, res_b) {
  if (is.na(res_a$estimate) || is.na(res_b$estimate)) return(NA_real_)
  diff <- res_a$estimate - res_b$estimate
  se_diff <- sqrt(res_a$se^2 + res_b$se^2)
  if (!is.finite(se_diff) || se_diff <= 0) return(NA_real_)
  z <- diff / se_diff
  2 * pnorm(abs(z), lower.tail = FALSE)
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

  p_court <- coef_difference_pvalue(results_list$basic_court, results_list$elevated_court)
  p_plaintiff <- coef_difference_pvalue(results_list$local_plaintiff, results_list$non_local_plaintiff)

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
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
    paste0(
      "Coefficient equality test ($p$) & ",
      "\\multicolumn{2}{c}{", fmt_p(p_court), "} & ",
      "\\multicolumn{2}{c}{", fmt_p(p_plaintiff), "} \\\\"
    ),
    "\\addlinespace",
    paste("Pre-treatment government win rate &", paste(pre_cells, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_cells, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_cells, collapse = " & "), "\\\\"),
    paste("Plaintiff entity &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Opposing counsel &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Court Fixed Effects &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Year Fixed Effects &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    paste("Cause-Group Fixed Effects &", paste(rep("Yes", 4), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Linear-probability coefficients on Treatment $\\times$ Post with the government-win indicator as the outcome, estimated on the indicated case sub-sample.",
      "Columns 1--2 split by court level: basic-level (district) people's courts versus intermediate, high, or specialized courts; the latter serve as a proxy for the cross-region adjudication arrangement of Liu, Wang, and Lyu (2023, \\textit{Journal of Public Economics}).",
      "Columns 3--4 split by whether the plaintiff is local to the defendant city.",
      "The Coefficient equality test reports the two-sided $p$-value for $H_0$: column 1 coefficient = column 2 coefficient (and analogously for columns 3 vs 4) using the $z$-statistic computed from city- and court-clustered standard errors, treating the two sub-samples as independent and ignoring any residual within-city dependence across the split samples.",
      "Standard errors clustered by city and court.",
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
