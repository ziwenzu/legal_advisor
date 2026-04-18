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
city_file <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

CAUSE_LABELS <- c(
  expropriation = "Expropriation\n& Compensation",
  land_planning = "Land &\nPlanning",
  public_security = "Public Security\n& Traffic",
  enforcement = "Enforcement\n& Penalties",
  permitting_review = "Permitting &\nReview",
  labor_social = "Labor &\nSocial Security"
)

CAUSE_ORDER <- c(
  "expropriation",
  "land_planning",
  "public_security",
  "enforcement",
  "permitting_review",
  "labor_social"
)

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

read_panels <- function(case_path, city_path) {
  cases <- fread(case_path)
  cy <- fread(city_path)

  cy_controls <- cy[, .(
    province, city, year, treatment,
    log_population_10k, log_gdp,
    log_registered_lawyers, log_court_caseload_n
  )]

  agg <- cases[
    !is.na(cause_group),
    .(
      gov_win_rate = mean(government_win, na.rm = TRUE),
      appeal_rate = mean(appealed, na.rm = TRUE),
      petition_rate = mean(petitioned, na.rm = TRUE),
      case_n = .N
    ),
    by = .(province, city, year, cause_group)
  ]

  panel <- agg[cy_controls, on = c("province", "city", "year"), nomatch = NULL]
  panel[, city_name := sprintf("%s_%s", province, city)]
  panel[, city_id := .GRP, by = city_name]
  panel[, ever_treated := as.integer(any(treatment == 1L)), by = city_name]
  panel[
    ,
    first_treat_year := ifelse(any(treatment == 1L), min(year[treatment == 1L]), NA_integer_),
    by = city_name
  ]
  panel[
    ,
    is_pre_treatment := as.integer(
      ever_treated == 1L & !is.na(first_treat_year) & year < first_treat_year
    )
  ]
  setorder(panel, cause_group, city_id, year)
  panel
}

compute_baseline_stats <- function(panel, outcome_name) {
  weighted_var <- function(values, weights) {
    weights <- weights[!is.na(values) & !is.na(weights)]
    values <- values[!is.na(values)]
    if (length(values) == 0L || sum(weights) <= 0) return(NA_real_)
    w <- weights / sum(weights)
    mu <- sum(values * w)
    sum(w * (values - mu)^2)
  }

  pre <- panel[is_pre_treatment == 1L]
  ctrl <- panel[ever_treated == 0L]

  rbindlist(lapply(CAUSE_ORDER, function(cg) {
    pre_sub <- pre[cause_group == cg & case_n >= 1]
    ctrl_sub <- ctrl[cause_group == cg & case_n >= 1]
    data.table(
      cause_group = cg,
      pre_mean = if (nrow(pre_sub) > 0) sum(pre_sub[[outcome_name]] * pre_sub$case_n) / sum(pre_sub$case_n) else NA_real_,
      pre_sd = sqrt(weighted_var(pre_sub[[outcome_name]], pre_sub$case_n)),
      pre_cells = nrow(pre_sub),
      pre_cases = sum(pre_sub$case_n),
      ctrl_mean = if (nrow(ctrl_sub) > 0) sum(ctrl_sub[[outcome_name]] * ctrl_sub$case_n) / sum(ctrl_sub$case_n) else NA_real_,
      ctrl_sd = sqrt(weighted_var(ctrl_sub[[outcome_name]], ctrl_sub$case_n)),
      ctrl_cells = nrow(ctrl_sub),
      ctrl_cases = sum(ctrl_sub$case_n)
    )
  }))
}

estimate_cause_model <- function(panel, cause_key, outcome_name) {
  sub <- panel[cause_group == cause_key & case_n >= 1]
  if (nrow(sub) < 10) {
    return(list(estimate = NA_real_, se = NA_real_, p_value = NA_real_, n_obs = 0L, r2 = NA_real_))
  }
  formula_obj <- as.formula(sprintf(
    "%s ~ treatment + log_population_10k + log_gdp + log_registered_lawyers + log_court_caseload_n | city_id + year",
    outcome_name
  ))
  model <- feols(formula_obj, data = sub, cluster = ~ city_id)
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

plot_coefplot <- function(results_dt, baseline_dt, outcome_label, file_path) {
  results_dt <- results_dt[match(CAUSE_ORDER, cause_group)]
  baseline_dt <- baseline_dt[match(CAUSE_ORDER, cause_group)]
  results_dt[, ci_lo := estimate - 1.96 * se]
  results_dt[, ci_hi := estimate + 1.96 * se]
  results_dt[, x_pos := seq_len(.N)]

  y_lo <- min(results_dt$ci_lo, 0, na.rm = TRUE)
  y_hi <- max(results_dt$ci_hi, 0, na.rm = TRUE)
  y_span <- y_hi - y_lo
  if (!is.finite(y_span) || y_span <= 0) y_span <- 0.1

  pdf(file = file_path, width = 7.6, height = 5.6, family = "serif")
  op <- par(
    bty = "l",
    las = 1,
    tcl = -0.25,
    mar = c(6.6, 5.2, 2.0, 1.0),
    cex.axis = 0.95,
    cex.lab = 1.1
  )
  on.exit({ par(op); dev.off() }, add = TRUE)

  plot(
    NA,
    xlim = c(0.5, nrow(results_dt) + 0.5),
    ylim = c(y_lo - 0.06 * y_span, y_hi + 0.10 * y_span),
    xlab = "",
    ylab = outcome_label,
    main = "",
    xaxt = "n"
  )
  abline(h = 0, col = "black", lwd = 1)
  segments(results_dt$x_pos, results_dt$ci_lo, results_dt$x_pos, results_dt$ci_hi,
           col = "black", lwd = 1.5)
  points(results_dt$x_pos, results_dt$estimate, pch = 16, cex = 1.2, col = "black")

  axis(1, at = results_dt$x_pos, labels = CAUSE_LABELS[results_dt$cause_group],
       tick = FALSE, line = 0.6, padj = 0.5, cex.axis = 0.85)

  baseline_labels <- vapply(seq_len(nrow(results_dt)), function(i) {
    cg <- results_dt$cause_group[i]
    bl <- baseline_dt[cause_group == cg]
    sprintf("Pre: %s\n(SD %s)", fmt_num(bl$pre_mean), fmt_num(bl$pre_sd))
  }, character(1))
  axis(1, at = results_dt$x_pos, labels = baseline_labels,
       tick = FALSE, line = 3.4, padj = 0.5, cex.axis = 0.72)

  for (i in seq_len(nrow(results_dt))) {
    star_text <- stars(results_dt$p_value[i])
    label <- sprintf("%s%s", fmt_num(results_dt$estimate[i]),
                     gsub("\\$|\\^|\\{|\\}", "", star_text))
    text(results_dt$x_pos[i], results_dt$ci_hi[i] + 0.025 * y_span,
         label, cex = 0.85, adj = c(0.5, 0))
  }
}

build_table <- function(results_dt, baseline_dt, file_path) {
  results_dt <- results_dt[match(CAUSE_ORDER, cause_group)]
  baseline_dt <- baseline_dt[match(CAUSE_ORDER, cause_group)]
  coef_cells <- vapply(seq_len(nrow(results_dt)),
                       function(i) paste0(fmt_num(results_dt$estimate[i]),
                                          stars(results_dt$p_value[i])),
                       character(1))
  se_cells <- vapply(seq_len(nrow(results_dt)),
                     function(i) paste0("(", fmt_num(results_dt$se[i]), ")"),
                     character(1))
  obs_cells <- vapply(seq_len(nrow(results_dt)),
                      function(i) fmt_int(results_dt$n_obs[i]),
                      character(1))
  r2_cells <- vapply(seq_len(nrow(results_dt)),
                     function(i) fmt_num(results_dt$r2[i]),
                     character(1))
  pre_mean_cells <- vapply(seq_len(nrow(results_dt)),
                           function(i) fmt_num(baseline_dt$pre_mean[i]),
                           character(1))
  pre_sd_cells <- vapply(seq_len(nrow(results_dt)),
                         function(i) sprintf("[%s]", fmt_num(baseline_dt$pre_sd[i])),
                         character(1))
  ctrl_mean_cells <- vapply(seq_len(nrow(results_dt)),
                            function(i) fmt_num(baseline_dt$ctrl_mean[i]),
                            character(1))
  ctrl_sd_cells <- vapply(seq_len(nrow(results_dt)),
                          function(i) sprintf("[%s]", fmt_num(baseline_dt$ctrl_sd[i])),
                          character(1))

  short_labels <- c(
    expropriation = "Expropriation",
    land_planning = "Land/Planning",
    public_security = "Public Security",
    enforcement = "Enforcement",
    permitting_review = "Permitting",
    labor_social = "Labor/Social"
  )
  header_labels <- short_labels[results_dt$cause_group]

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Effect of Legal Counsel Procurement by Administrative Cause Group}",
    "\\label{tab:admin_by_cause}",
    "\\begin{threeparttable}",
    sprintf("\\begin{tabular}{l%s}", paste(rep("c", nrow(results_dt)), collapse = "")),
    "\\toprule",
    paste(" &", paste(sprintf("(%d)", seq_len(nrow(results_dt))), collapse = " & "), "\\\\"),
    paste("Cause Group &", paste(header_labels, collapse = " & "), "\\\\"),
    "\\midrule",
    paste("Treatment $\\times$ Post &", paste(coef_cells, collapse = " & "), "\\\\"),
    paste("&", paste(se_cells, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Pre-treatment mean &", paste(pre_mean_cells, collapse = " & "), "\\\\"),
    paste("&", paste(pre_sd_cells, collapse = " & "), "\\\\"),
    paste("Control mean &", paste(ctrl_mean_cells, collapse = " & "), "\\\\"),
    paste("&", paste(ctrl_sd_cells, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Observations &", paste(obs_cells, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_cells, collapse = " & "), "\\\\"),
    paste("City-Year Controls &", paste(rep("Yes", nrow(results_dt)), collapse = " & "), "\\\\"),
    paste("City FE &", paste(rep("Yes", nrow(results_dt)), collapse = " & "), "\\\\"),
    paste("Year FE &", paste(rep("Yes", nrow(results_dt)), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Each column reports the coefficient on Treatment $\\times$ Post from a separate two-way fixed-effects regression on a (city $\\times$ year) panel restricted to one cause group.",
      "The dependent variable is the within-city-year share of administrative cases in that cause group in which the government prevailed.",
      "Pre-treatment mean is the case-weighted mean of the dependent variable across treated cities in the years strictly before that city's first procurement year; Control mean is the analogous case-weighted mean across never-treated cities pooled across all sample years.",
      "Standard deviations across the pooled cells appear in square brackets.",
      "Cause groups bundle the 30 most common 案由 codes into six policy-relevant categories: expropriation and compensation; land and planning; public security and traffic; enforcement and administrative penalties; permitting and administrative review; and labor and social security.",
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

main <- function() {
  panel <- read_panels(input_file, city_file)
  win_results <- rbindlist(lapply(CAUSE_ORDER, function(cg) {
    res <- estimate_cause_model(panel, cg, "gov_win_rate")
    data.table(cause_group = cg, estimate = res$estimate, se = res$se,
               p_value = res$p_value, n_obs = res$n_obs, r2 = res$r2)
  }))
  baseline_dt <- compute_baseline_stats(panel, "gov_win_rate")

  plot_coefplot(
    results_dt = win_results,
    baseline_dt = baseline_dt,
    outcome_label = "ATT on Government Win Rate",
    file_path = file.path(figure_dir, "admin_by_cause_government_win_rate_coefplot.pdf")
  )

  build_table(
    results_dt = win_results,
    baseline_dt = baseline_dt,
    file_path = file.path(table_dir, "admin_by_cause_government_win_rate_coefplot_table.tex")
  )
}

main()
