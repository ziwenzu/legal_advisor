#!/usr/bin/env Rscript
# city_year_honest_did.R
#
# Rambachan and Roth (2023) honest parallel-trends bounds for the
# three city-year outcomes. For each outcome we estimate a TWFE
# event-study, extract the event-time vcov, and compute the SDRD
# (smoothness restriction) identified set for the post-period ATT
# at three values of M = 0, 0.5, 1.0 times the largest absolute
# pre-period coefficient.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(HonestDiD)
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
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

NEVER_TREATED_SENTINEL <- 0
PRE_PERIODS <- c(-5, -4, -3, -2)
POST_PERIODS <- c(0, 1, 2, 3, 4, 5)

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

preferred_controls <- function(outcome) {
  controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
  if (outcome == "government_win_rate") controls <- c(controls, "log_court_caseload_n")
  controls
}

read_panel <- function() {
  dt <- fread(city_path)
  dt[, city_id := .GRP, by = .(province, city)]
  dt[, ever_treated := as.integer(any(treatment == 1L)), by = city_id]
  dt[
    ,
    first_treat_year := if (any(treatment == 1L)) min(year[treatment == 1L]) else NEVER_TREATED_SENTINEL,
    by = city_id
  ]
  dt[, rel_time := fifelse(ever_treated == 1L, year - first_treat_year, NA_real_)]
  dt
}

estimate_event_vcov <- function(panel, outcome) {
  rhs <- paste(c("i(rel_time, ever_treated, ref = -1)", preferred_controls(outcome)),
               collapse = " + ")
  m <- feols(as.formula(sprintf("%s ~ %s | city_id + year", outcome, rhs)),
             data = panel, cluster = ~ city_id)
  coefs <- coef(m)
  vc <- vcov(m, attr = FALSE)
  ev_pat <- "^rel_time::-?\\d+:ever_treated$"
  ev_idx <- grep(ev_pat, names(coefs))
  ev_names <- names(coefs)[ev_idx]
  rel_times <- as.integer(sub(":ever_treated$", "",
                              sub("^rel_time::", "", ev_names)))
  beta <- coefs[ev_idx]
  vc_e <- vc[ev_idx, ev_idx, drop = FALSE]
  ord <- order(rel_times)
  beta <- beta[ord]
  vc_e <- vc_e[ord, ord, drop = FALSE]
  rel_times <- rel_times[ord]
  list(beta = beta, vc = vc_e, rel_times = rel_times)
}

run_honest <- function(beta, vc, rel_times, l_vec, m_grid) {
  num_pre <- sum(rel_times < 0)
  num_post <- sum(rel_times > 0) + 1
  if (length(l_vec) != num_post) stop("l_vec length != num_post")
  do.call(rbind, lapply(m_grid, function(M) {
    res <- createSensitivityResults(
      betahat = beta, sigma = vc,
      numPrePeriods = num_pre, numPostPeriods = num_post,
      method = "C-LF", Mvec = M,
      l_vec = matrix(l_vec, ncol = 1), monotonicityDirection = NULL,
      biasDirection = NULL, alpha = 0.05
    )
    data.table(M = M, lb = res$lb[1], ub = res$ub[1])
  }))
}

plot_bounds <- function(bounds_dt, original_est, original_se, outcome_label, file_path) {
  ci_lo <- original_est - 1.96 * original_se
  ci_hi <- original_est + 1.96 * original_se
  bounds_dt <- copy(bounds_dt)
  bounds_dt[, label := sprintf("M=%.2f", M)]
  y_min <- min(c(bounds_dt$lb, ci_lo)) - 0.05 * abs(min(c(bounds_dt$lb, ci_lo)) - max(c(bounds_dt$ub, ci_hi)))
  y_max <- max(c(bounds_dt$ub, ci_hi)) + 0.05 * abs(min(c(bounds_dt$lb, ci_lo)) - max(c(bounds_dt$ub, ci_hi)))
  if (!is.finite(y_min) || !is.finite(y_max) || y_min == y_max) {
    y_min <- min(bounds_dt$lb) - 1; y_max <- max(bounds_dt$ub) + 1
  }
  pdf(file_path, width = 7.0, height = 4.6, family = "serif")
  op <- par(bty = "l", las = 1, mar = c(4.5, 5.0, 2.0, 1.0))
  on.exit({par(op); dev.off()}, add = TRUE)
  x_vals <- c(0, seq_len(nrow(bounds_dt)))
  plot(NA, xlim = c(-0.5, nrow(bounds_dt) + 0.5),
       ylim = c(y_min, y_max),
       xlab = "Parallel-Trends Restriction",
       ylab = paste("Identified Set for ATT:", outcome_label),
       xaxt = "n")
  axis(1, at = x_vals,
       labels = c("OLS\n(95% CI)", bounds_dt$label))
  abline(h = 0, col = "gray60", lty = 2)
  segments(0, ci_lo, 0, ci_hi, col = "black", lwd = 2)
  points(0, original_est, pch = 16, cex = 1.2, col = "black")
  for (i in seq_len(nrow(bounds_dt))) {
    segments(i, bounds_dt$lb[i], i, bounds_dt$ub[i], col = "black", lwd = 2)
    points(i, bounds_dt$lb[i], pch = "-", cex = 2, col = "black")
    points(i, bounds_dt$ub[i], pch = "-", cex = 2, col = "black")
  }
}

main <- function() {
  panel <- read_panel()
  outcomes <- c("government_win_rate", "appeal_rate", "admin_case_n")
  outcome_labels <- c("Government Win Rate", "Appeal Rate",
                      "Administrative Case Numbers")
  l_vec <- c(1, rep(1, length(POST_PERIODS) - 1)) / length(POST_PERIODS)

  table_rows <- character(0)
  for (i in seq_along(outcomes)) {
    o <- outcomes[i]
    ev <- estimate_event_vcov(panel, o)
    keep <- ev$rel_times %in% c(PRE_PERIODS, POST_PERIODS)
    beta_used <- ev$beta[keep]
    vc_used <- ev$vc[keep, keep, drop = FALSE]
    rt_used <- ev$rel_times[keep]
    max_pre_abs <- max(abs(beta_used[rt_used %in% PRE_PERIODS]))
    m_grid <- c(0, 0.5, 1.0) * max_pre_abs
    bounds <- run_honest(beta_used, vc_used, rt_used, l_vec, m_grid)
    avg_post_beta <- mean(beta_used[rt_used %in% POST_PERIODS])
    l_mat <- matrix(l_vec, ncol = 1)
    post_idx <- which(rt_used %in% POST_PERIODS)
    avg_post_var <- as.numeric(t(l_mat) %*% vc_used[post_idx, post_idx] %*% l_mat)
    avg_post_se <- sqrt(avg_post_var)
    fig_path <- file.path(figure_dir,
                          sprintf("honest_did_%s.pdf", o))
    plot_bounds(bounds, avg_post_beta, avg_post_se, outcome_labels[i], fig_path)
    cat("Wrote", fig_path, "\n")
    for (k in seq_len(nrow(bounds))) {
      table_rows <- c(table_rows, paste(
        outcome_labels[i], "&",
        fmt_num(bounds$M[k]), "&",
        sprintf("[%s, %s]", fmt_num(bounds$lb[k]), fmt_num(bounds$ub[k])),
        "\\\\"
      ))
    }
    table_rows <- c(table_rows, "\\addlinespace")
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Honest Parallel-Trends Bounds (Rambachan and Roth 2023) for City-Year Outcomes}",
    "\\label{tab:city_year_honest_did_appendix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Outcome & $M$ & 95\\% Identified Set for Average Post ATT \\\\",
    "\\midrule",
    table_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} 95\\% identified sets for the average post-procurement ATT under the smoothness restriction of Rambachan and Roth (2023).",
      "$M$ bounds the maximum permissible deviation from a linear pre-trend in adjacent periods, expressed in the same units as the outcome.",
      "Reported $M$ values are $\\{0, 0.5, 1.0\\}$ times the largest absolute pre-period event-study coefficient from the OLS event study (with city and year fixed effects, the standard city covariates, clustered by city).",
      "Computed with the \\texttt{HonestDiD} R package using the conditional least-favorable critical value."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "city_year_honest_did_appendix_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
