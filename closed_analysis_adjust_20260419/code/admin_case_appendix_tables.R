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
admin_file <- file.path(root_dir, "data", "admin_case_level.csv")
city_file <- file.path(root_dir, "data", "city_year_panel.csv")
firm_file <- file.path(root_dir, "data", "firm_level.csv")
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
    "\\setlength{\\abovecaptionskip}{0pt}",
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
    paste("Controls (city-year) &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("City Fixed Effects &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    paste("Year Fixed Effects &", paste(rep("Yes", 2), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Each column reports Treatment $\\times$ Post from a two-way fixed-effects regression on a (city $\\times$ year) panel restricted to cases heard at the indicated level of court.",
      "Outcome is the within-city-year share of administrative cases at that court level in which the government prevailed.",
      "City-year controls: log population, log GDP, log registered lawyers, log court caseload.",
      "Standard errors clustered by city.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lines, con = file_path)
}

balance_rows <- function(treated_dt, control_dt, vars, var_labels) {
  rows <- lapply(vars, function(v) {
    t_vals <- as.numeric(treated_dt[[v]])
    c_vals <- as.numeric(control_dt[[v]])
    t_mean <- mean(t_vals, na.rm = TRUE)
    c_mean <- mean(c_vals, na.rm = TRUE)
    pooled_sd <- sqrt(((var(t_vals, na.rm = TRUE) + var(c_vals, na.rm = TRUE)) / 2))
    nd <- if (is.finite(pooled_sd) && pooled_sd > 0) (t_mean - c_mean) / pooled_sd else NA_real_
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
  rbindlist(rows)
}

format_balance_lines <- function(bal, integer_vars = character()) {
  fmt_pretty <- function(x, var_name) {
    if (var_name %in% integer_vars) fmt_int(x) else fmt_num(x)
  }
  vapply(seq_len(nrow(bal)), function(i) {
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
}

build_balance_table <- function(city_dt, firm_dt, file_path) {
  city_dt <- copy(city_dt)
  city_dt[, city_name := sprintf("%s_%s", province, city)]
  city_dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_name]
  city_dt[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(year[treatment == 1L]), NA_integer_),
    by = city_name
  ]

  city_treated <- city_dt[
    ever_treated == 1L &
      !is.na(first_treat_year) &
      year < first_treat_year
  ]
  city_untreated <- city_dt[ever_treated == 0L]

  city_vars <- c(
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
  city_labels <- c(
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

  city_bal <- balance_rows(city_treated, city_untreated, city_vars, city_labels)
  city_body <- format_balance_lines(city_bal, integer_vars = "admin_case_n")

  n_city_treated_cy <- nrow(city_treated)
  n_city_treated_units <- length(unique(city_treated$city_name))
  n_city_control_cy <- nrow(city_untreated)
  n_city_control_units <- length(unique(city_untreated$city_name))

  firm_dt <- copy(firm_dt)
  firm_pre <- firm_dt[!is.na(event_time) & event_time < 0]
  if ("firm_size" %in% names(firm_pre)) {
    firm_pre[, log_firm_size := fifelse(!is.na(firm_size) & firm_size > 0, log(firm_size), NA_real_)]
  }
  if (all(c("enterprise_case_n", "civil_case_n") %in% names(firm_pre))) {
    firm_pre[, enterprise_share := fifelse(civil_case_n > 0, enterprise_case_n / civil_case_n, NA_real_)]
  }
  firm_pre[, log_civil_case_n := fifelse(civil_case_n > 0, log(civil_case_n), NA_real_)]

  firm_treated <- firm_pre[treated_firm == 1L]
  firm_control <- firm_pre[treated_firm == 0L]

  firm_vars <- c(
    "log_firm_size",
    "log_civil_case_n",
    "civil_win_rate_mean",
    "civil_win_rate_fee_mean",
    "avg_filing_to_hearing_days",
    "enterprise_share"
  )
  firm_vars <- intersect(firm_vars, names(firm_pre))
  firm_labels <- c(
    log_firm_size = "Log Firm Size (Lawyers)",
    log_civil_case_n = "Log Civil Cases per Firm-Year",
    civil_win_rate_mean = "Civil Win Rate (Decisive)",
    civil_win_rate_fee_mean = "Fee-Based Civil Win Rate",
    avg_filing_to_hearing_days = "Average Filing-to-Hearing Days",
    enterprise_share = "Enterprise Share of Cases"
  )
  firm_bal <- balance_rows(firm_treated, firm_control, firm_vars, firm_labels)
  firm_body <- format_balance_lines(firm_bal)

  n_firm_treated_obs <- nrow(firm_treated)
  n_firm_treated_units <- length(unique(firm_treated$firm_id))
  n_firm_control_obs <- nrow(firm_control)
  n_firm_control_units <- length(unique(firm_control$firm_id))

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Pre-Procurement Balance Across Treated and Control Units}",
    "\\label{tab:pre_procurement_balance}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    "Variable & Treated Mean & Control Mean & Difference & Normalized Diff. & $p$-value \\\\",
    "\\midrule",
    "\\multicolumn{6}{l}{\\textit{Panel A. City-year administrative panel (treated cities vs.\\ never-treated cities)}} \\\\",
    city_body,
    "\\addlinespace",
    paste0("City-Year Observations & ", fmt_int(n_city_treated_cy), " & ", fmt_int(n_city_control_cy),
           " &  &  &  \\\\"),
    paste0("Distinct Cities & ", fmt_int(n_city_treated_units), " & ", fmt_int(n_city_control_units),
           " &  &  &  \\\\"),
    "\\addlinespace",
    "\\multicolumn{6}{l}{\\textit{Panel B. Firm-year stacked panel (procurement winners vs.\\ runner-up controls, event time $<0$)}} \\\\",
    firm_body,
    "\\addlinespace",
    paste0("Firm-Year Observations & ", fmt_int(n_firm_treated_obs), " & ", fmt_int(n_firm_control_obs),
           " &  &  &  \\\\"),
    paste0("Distinct Firms & ", fmt_int(n_firm_treated_units), " & ", fmt_int(n_firm_control_units),
           " &  &  &  \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Panel A pools city-year observations from cities that eventually adopt procurement, restricted to years strictly before each city's first procurement year, against city-year observations from never-procuring cities.",
      "Panel B pools firm-year observations from procurement winners against runner-up control firms within the same procurement stack, restricted to event time strictly less than zero.",
      "Difference is Treated minus Control; Normalized Difference divides by the pooled cross-group standard deviation.",
      "$p$-values from two-sample $t$-tests with unequal variances.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  city_dt <- fread(city_file)
  firm_dt <- fread(firm_file)

  build_balance_table(
    city_dt = city_dt,
    firm_dt = firm_dt,
    file_path = file.path(table_dir, "pre_procurement_balance_appendix_table.tex")
  )

  old_path <- file.path(table_dir, "admin_pre_procurement_balance_appendix_table.tex")
  if (file.exists(old_path)) file.remove(old_path)
}

main()
