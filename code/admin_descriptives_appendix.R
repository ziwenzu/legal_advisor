#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
city_path <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
admin_path <- file.path(root_dir, "data", "output data", "admin_case_level.csv")
firm_path <- file.path(root_dir, "data", "output data", "firm_level.csv")
table_dir <- file.path(root_dir, "output", "tables")
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("--")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

stat_row <- function(label, x, digits = 3) {
  x <- as.numeric(x)
  paste(
    label, "&",
    fmt_int(sum(!is.na(x))), "&",
    fmt_num(mean(x, na.rm = TRUE), digits), "&",
    fmt_num(sd(x, na.rm = TRUE), digits), "&",
    fmt_num(quantile(x, 0.10, na.rm = TRUE), digits), "&",
    fmt_num(median(x, na.rm = TRUE), digits), "&",
    fmt_num(quantile(x, 0.90, na.rm = TRUE), digits),
    "\\\\"
  )
}

write_summary_table <- function(file_path) {
  city <- fread(city_path)
  admin <- fread(admin_path)
  firm <- fread(firm_path)

  panel_a <- c(
    stat_row("Government win rate", city$government_win_rate),
    stat_row("Appeal rate", city$appeal_rate),
    stat_row("Petition rate", city$petition_rate),
    stat_row("Government counsel share", city$gov_lawyer_share),
    stat_row("Opposing counsel share", city$opp_lawyer_share),
    stat_row("Administrative cases", city$admin_case_n, digits = 0),
    stat_row("Log population (10k)", city$log_population_10k),
    stat_row("Log GDP", city$log_gdp),
    stat_row("Log registered lawyers", city$log_registered_lawyers),
    stat_row("Log court caseload", city$log_court_caseload_n)
  )

  panel_b <- c(
    stat_row("Government win", admin$government_win),
    stat_row("Appealed", admin$appealed),
    stat_row("Petitioned", admin$petitioned),
    stat_row("Government has counsel", admin$government_has_lawyer),
    stat_row("Opposing party has counsel", admin$opponent_has_lawyer),
    stat_row("Plaintiff is entity", admin$plaintiff_is_entity),
    stat_row("Non-local plaintiff", admin$non_local_plaintiff),
    stat_row("Cross-jurisdiction adjudication", admin$cross_jurisdiction),
    stat_row("Case duration (days)", admin$duration_days, digits = 0),
    stat_row("Log case duration", admin$log_duration_days)
  )

  panel_c <- c(
    stat_row("Civil cases per firm-year", firm$civil_case_n, digits = 0),
    stat_row("Decisive civil cases", firm$civil_decisive_case_n, digits = 0),
    stat_row("Civil win rate (decisive)", firm$civil_win_rate_mean),
    stat_row("Fee-based win rate", firm$civil_win_rate_fee_mean),
    stat_row("Average filing-to-hearing days", firm$avg_filing_to_hearing_days, digits = 1),
    stat_row("Enterprise-client cases", firm$enterprise_case_n, digits = 0),
    stat_row("Personal-client cases", firm$personal_case_n, digits = 0)
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\caption{Descriptive Statistics for the Three Analytical Panels}",
    "\\label{tab:summary_statistics_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    "Variable & N & Mean & SD & 10th pctile & Median & 90th pctile \\\\",
    "\\midrule",
    "\\multicolumn{7}{l}{\\textit{Panel A. City-year administrative panel}} \\\\",
    panel_a,
    "\\addlinespace",
    "\\multicolumn{7}{l}{\\textit{Panel B. Administrative case-level panel}} \\\\",
    panel_b,
    "\\addlinespace",
    "\\multicolumn{7}{l}{\\textit{Panel C. Firm-year stacked panel}} \\\\",
    panel_c,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Notes:}",
      "Panel A summarises the city-year administrative panel used for the headline regressions.",
      "Panel B summarises the administrative case-level panel used in the lawyer-presence specifications, plaintiff and cross-jurisdiction heterogeneity tables, and the by-cause coefplot.",
      "Panel C summarises the firm-year stacked panel of procurement winners and matched runner-up firms used in the firm-level civil-litigation analysis.",
      "All variables are pooled over the 2014--2020 sample window."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, file_path)
  cat("Wrote", file_path, "\n")
}

draw_adoption_timeline <- function(file_path) {
  city <- fread(city_path)
  city[, ever_treated := as.integer(any(treatment == 1L)),
       by = .(province, city)]
  city[ever_treated == 1L,
       first_treat_year := min(year[treatment == 1L]),
       by = .(province, city)]
  first <- unique(city[ever_treated == 1L, .(province, city, first_treat_year)])
  by_year <- first[, .(new_adopters = .N), by = first_treat_year]
  setorder(by_year, first_treat_year)
  by_year[, cumulative := cumsum(new_adopters)]

  pdf(file = file_path, width = 7.4, height = 4.8, family = "serif")
  op <- par(
    bty = "l",
    las = 1,
    tcl = -0.25,
    mar = c(4.6, 5.0, 2.0, 4.6),
    cex.axis = 0.95,
    cex.lab = 1.05
  )
  on.exit({ par(op); dev.off() }, add = TRUE)

  bars_x <- by_year$first_treat_year
  bars_y <- by_year$new_adopters
  cum_y <- by_year$cumulative
  y_bar_max <- max(bars_y) * 1.15
  y_cum_max <- max(cum_y) * 1.10

  plot(
    NA,
    xlim = c(min(bars_x) - 0.5, max(bars_x) + 0.5),
    ylim = c(0, y_bar_max),
    xlab = "Procurement Adoption Year",
    ylab = "New Cities Adopting in Year",
    main = "",
    xaxt = "n"
  )
  axis(1, at = bars_x)
  rect(bars_x - 0.35, 0, bars_x + 0.35, bars_y,
       col = "gray80", border = "gray40")

  par(new = TRUE)
  plot(
    bars_x, cum_y,
    type = "b", pch = 16, lty = 1, lwd = 2,
    xlim = c(min(bars_x) - 0.5, max(bars_x) + 0.5),
    ylim = c(0, y_cum_max),
    axes = FALSE,
    xlab = "", ylab = "",
    main = ""
  )
  axis(4)
  mtext("Cumulative Cities Adopted", side = 4, line = 3, las = 0, cex = 1.05)

  legend(
    "topleft",
    legend = c("New adopters (left axis)", "Cumulative (right axis)"),
    pch = c(15, 16),
    pt.cex = c(1.4, 1.1),
    col = c("gray70", "black"),
    bty = "n",
    cex = 0.85
  )
}

main <- function() {
  write_summary_table(file.path(table_dir, "summary_statistics_appendix_table.tex"))
  draw_adoption_timeline(file.path(figure_dir, "procurement_adoption_timeline.pdf"))
}

main()
