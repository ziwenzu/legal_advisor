#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
admin_file <- Sys.getenv(
  "ADMIN_CASE_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "admin_case_level.csv")
)
city_file <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
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

build_court_level_table <- function(admin_dt, city_dt, file_path) {
  cy_keys <- city_dt[, .(province, city, year, treatment,
                         log_population_10k, log_gdp,
                         log_registered_lawyers, log_court_caseload_n)]

  admin_dt[, court_level_grouped := fifelse(
    court_level %in% c("intermediate", "high", "specialized"),
    "intermediate_plus", "basic"
  )]
  agg <- admin_dt[
    ,
    .(
      gov_win_rate = mean(government_win, na.rm = TRUE),
      case_n = .N
    ),
    by = .(province, city, year, court_level_grouped)
  ]
  panel <- agg[cy_keys, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_name := sprintf("%s_%s", province, city)]
  panel[, city_id := .GRP, by = city_name]

  estimate_for <- function(level_key) {
    sub <- panel[court_level_grouped == level_key & case_n >= 1]
    if (nrow(sub) < 10) {
      return(list(estimate = NA_real_, se = NA_real_, p_value = NA_real_,
                  n_obs = 0L, r2 = NA_real_))
    }
    model <- feols(
      gov_win_rate ~ treatment + log_population_10k + log_gdp +
        log_registered_lawyers + log_court_caseload_n |
        city_id + year,
      data = sub,
      cluster = ~ city_id
    )
    ct <- as.data.table(coeftable(model), keep.rownames = "term")
    row <- ct[term == "treatment"]
    list(
      estimate = row[["Estimate"]],
      se = row[["Std. Error"]],
      p_value = row[["Pr(>|t|)"]],
      n_obs = nobs(model),
      r2 = fitstat(model, "r2")[[1]]
    )
  }

  basic <- estimate_for("basic")
  inter <- estimate_for("intermediate_plus")

  coef_cells <- c(
    paste0(fmt_num(basic$estimate), stars(basic$p_value)),
    paste0(fmt_num(inter$estimate), stars(inter$p_value))
  )
  se_cells <- c(
    paste0("(", fmt_num(basic$se), ")"),
    paste0("(", fmt_num(inter$se), ")")
  )
  obs_cells <- c(fmt_int(basic$n_obs), fmt_int(inter$n_obs))
  r2_cells <- c(fmt_num(basic$r2), fmt_num(inter$r2))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Effect of Legal Counsel Procurement by Court Level}",
    "\\label{tab:admin_by_court_level}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    " & (1) & (2) \\\\",
    "Court Level & Basic People's Courts & Intermediate and Above \\\\",
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_cells, collapse = " & "), "\\\\"),
    paste("&", paste(se_cells, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_cells, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_cells, collapse = " & "), "\\\\"),
    paste("City-Year Controls &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("City FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Each column reports the coefficient on Treatment $\\times$ Post from a separate two-way fixed-effects regression on a (city $\\times$ year) panel restricted to cases heard at the indicated level of court.",
      "Court level is parsed from the full court name: column 1 keeps cases adjudicated by basic-level (district) people's courts; column 2 keeps cases at intermediate, high, and specialized courts.",
      "The dependent variable is the within-city-year share of administrative cases at that court level in which the government prevailed.",
      "All specifications include city and year fixed effects, log population, log GDP, log registered lawyers, and log court caseload.",
      "Cluster-robust standard errors by city appear in parentheses.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lines, con = file_path)
}

build_balance_table <- function(city_dt, file_path) {
  city_dt <- copy(city_dt)
  city_dt[, city_name := sprintf("%s_%s", province, city)]
  city_dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_name]
  city_dt[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(year[treatment == 1L]), NA_integer_),
    by = city_name
  ]

  treated <- city_dt[
    ever_treated == 1L &
      !is.na(first_treat_year) &
      year < first_treat_year
  ]
  untreated <- city_dt[ever_treated == 0L]

  vars <- c(
    "government_win_rate",
    "appeal_rate",
    "petition_rate",
    "admin_case_n",
    "gov_lawyer_share",
    "opp_lawyer_share",
    "log_population_10k",
    "log_gdp",
    "log_registered_lawyers",
    "log_court_caseload_n"
  )
  var_labels <- c(
    government_win_rate = "Government Win Rate",
    appeal_rate = "Appeal Rate",
    petition_rate = "Petition Rate",
    admin_case_n = "Administrative Case Numbers",
    gov_lawyer_share = "Government Counsel Share",
    opp_lawyer_share = "Opposing Counsel Share",
    log_population_10k = "Log Population (10k)",
    log_gdp = "Log GDP",
    log_registered_lawyers = "Log Registered Lawyers",
    log_court_caseload_n = "Log Court Caseload"
  )

  rows <- lapply(vars, function(v) {
    t_vals <- treated[[v]]
    c_vals <- untreated[[v]]
    t_mean <- mean(t_vals, na.rm = TRUE)
    c_mean <- mean(c_vals, na.rm = TRUE)
    pooled_sd <- sqrt(((var(t_vals, na.rm = TRUE) + var(c_vals, na.rm = TRUE)) / 2))
    nd <- (t_mean - c_mean) / pooled_sd
    test <- tryCatch(
      t.test(t_vals, c_vals, var.equal = FALSE),
      error = function(e) NULL
    )
    p_val <- if (is.null(test)) NA_real_ else test$p.value
    data.table(
      var = v,
      label = var_labels[[v]],
      treated_mean = t_mean,
      control_mean = c_mean,
      diff = t_mean - c_mean,
      norm_diff = nd,
      p_value = p_val
    )
  })
  bal <- rbindlist(rows)

  fmt_pretty <- function(x, var_name) {
    if (var_name == "admin_case_n") fmt_int(x) else fmt_num(x)
  }

  body_lines <- vapply(seq_len(nrow(bal)), function(i) {
    row <- bal[i]
    paste(
      row$label,
      "&",
      fmt_pretty(row$treated_mean, row$var),
      "&",
      fmt_pretty(row$control_mean, row$var),
      "&",
      fmt_pretty(row$diff, row$var),
      "&",
      fmt_num(row$norm_diff),
      "&",
      paste0(fmt_num(row$p_value), stars(row$p_value)),
      "\\\\"
    )
  }, character(1))

  n_treated_cy <- nrow(treated)
  n_treated_cities <- length(unique(treated$city_name))
  n_control_cy <- nrow(untreated)
  n_control_cities <- length(unique(untreated$city_name))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Pre-Procurement Balance Between Treated and Never-Treated Cities}",
    "\\label{tab:admin_pre_balance}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Variable & Treated Mean & Control Mean & Difference & Normalized Diff. & $p$-value \\\\",
    "\\midrule",
    body_lines,
    "\\addlinespace",
    paste0("City-Year Observations & ", fmt_int(n_treated_cy), " & ", fmt_int(n_control_cy),
           " & -- & -- & -- \\\\"),
    paste0("Distinct Cities & ", fmt_int(n_treated_cities), " & ", fmt_int(n_control_cities),
           " & -- & -- & -- \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "The Treated columns pool all city-year observations from cities that eventually adopt legal-counsel procurement, restricted to years strictly before that city's first procurement year.",
      "The Control columns pool all city-year observations from cities that never adopt procurement during the 2014--2020 sample window.",
      "Difference is Treated minus Control.",
      "Normalized Difference divides the raw difference by the pooled cross-group standard deviation.",
      "$p$-value is from a two-sample $t$-test allowing unequal variances.",
      "Government Counsel Share and Opposing Counsel Share are the within-city-year shares of administrative cases in which the government, respectively the opposing party, appears with counsel.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  admin_dt <- fread(admin_file)
  city_dt <- fread(city_file)

  build_court_level_table(
    admin_dt = admin_dt,
    city_dt = city_dt,
    file_path = file.path(table_dir, "admin_case_by_court_level_appendix_table.tex")
  )
  build_balance_table(
    city_dt = city_dt,
    file_path = file.path(table_dir, "admin_pre_procurement_balance_appendix_table.tex")
  )
}

main()
