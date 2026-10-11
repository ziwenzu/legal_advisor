#!/usr/bin/env Rscript
# firm_fect_carryover_exit.R
#
# For cities with multiple initial sample awards (2 or 3 stacks in the
# firm-year panel), this legacy analytical clock switches the winning
# initial-award spell off when the city's next initial sample award starts.
# It excludes constructed renewals, actual contract durations, and coverage
# gaps, and does not verify loss of affiliation. The binary clock is retained
# without changing treatment coding or estimates and runs the fect package's
#  (i) period-wise ATT relative to treatment exit (treat == 0
#      after the successor award), and
#  (ii) the carry-over test, which probes whether the post-exit
#       outcomes differ from zero after the initial-award spell ends.

suppressPackageStartupMessages({
  library(data.table)
  library(fect)
  library(ggplot2)
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
firm_path <- file.path(root_dir, "data", "firm_level.csv")
table_dir <- file.path(root_dir, "output", "tables")
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}
fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

main <- function() {
  fm <- fread(firm_path, encoding = "UTF-8")

  city_stacks <- unique(fm[, .(province, city, award_id, event_year)])
  contracts_per_city <- city_stacks[, .(n_stacks = uniqueN(award_id)),
                                     by = .(province, city)]
  exit_cities <- contracts_per_city[n_stacks >= 2L, .(province, city, n_stacks)]
  if (nrow(exit_cities) == 0L) {
    cat("No multi-contract cities; nothing to do.\n")
    return(invisible(NULL))
  }

  fm_exit <- merge(fm, exit_cities[, .(province, city)],
                   by = c("province", "city"))
  fm_exit[, contract_active := as.integer(treated_firm == 1L &
                                           event_time >= 0L)]
  # End every observed winning spell at its own successor award, including the
  # second winner in cities with three contracts.
  contract_periods <- city_stacks[order(event_year), .(
    award_id = award_id,
    stack_next = shift(event_year, type = "lead")
  ), by = .(province, city)]
  fm_exit <- merge(fm_exit, contract_periods[, .(award_id, stack_next)], by = "award_id",
                   all.x = TRUE)
  fm_exit[, lost_contract := as.integer(treated_firm == 1L &
                                         !is.na(stack_next) &
                                         year >= stack_next)]
  fm_exit[, treat := as.integer(contract_active == 1L & lost_contract == 0L)]
  stopifnot(fm_exit[treated_firm == 1L & !is.na(stack_next) & year >= stack_next,
                   all(treat == 0L)])

  fm_exit[, unit_id := sprintf("%s__%s", award_id, firm_id)]
  fm_exit[, time_id := year]

  # Outcomes: each entry has the column name (used for filtering and as the
  # row label inside the LaTeX table), the sample-restriction filter, an
  # `analytical` figure title and y-axis label that we want appearing on the
  # PDFs (so the publication-facing figures do not show variable code names
  # like `civil_win_rate_mean`), a y-axis short tag for the table, and the
  # natural unit (probability vs. days) so we can format numbers correctly.
  outcomes <- list(
    list(label = "civil_win_rate_mean",
         table_label = "Civil case win rate (decisive cases)",
         filter = quote(civil_decisive_case_n > 0),
         fig_title = "civil case win rate (decisive cases)",
         y_lab = "ATT on win rate (probability units)",
         unit = "prob"),
    list(label = "avg_filing_to_hearing_days",
         table_label = "Average filing-to-hearing time (days)",
         filter = quote(civil_case_n > 0),
         fig_title = "average filing-to-hearing time",
         y_lab = "ATT on filing-to-hearing time (days)",
         unit = "days"),
    list(label = "civil_win_rate_fee_mean",
         table_label = "Fee-based civil win rate (decisive fee cases)",
         filter = quote(civil_fee_decisive_case_n > 0),
         fig_title = "fee-based civil win rate (decisive fee cases)",
         y_lab = "ATT on fee-based win rate (probability units)",
         unit = "prob")
  )

  fmt_with_unit <- function(v, unit, digits = 3) {
    if (length(v) == 0 || is.na(v)) return("")
    if (unit == "days") sprintf(paste0("%.", max(1, digits-1), "f days"), v)
    else                sprintf(paste0("%+.", digits, "f"), v)
  }

  rows <- character(0)
  result_rows <- list()
  for (spec in outcomes) {
    sub <- as.data.frame(fm_exit[!is.na(get(spec$label)) & eval(spec$filter)])
    if (nrow(sub) < 30) next

    # Single fect call with carryoverTest = TRUE; this object exposes both
    # the period-wise ATT relative to treatment exit (for the figure) and
    # the carry-over test summary (for the table).
    fit <- tryCatch(
      fect(formula = as.formula(paste(spec$label, "~ treat")),
           data = sub,
           index = c("unit_id", "time_id"),
           force = "two-way",
           method = "fe",
           se = TRUE,
           nboots = 200L,
           parallel = FALSE,
           seed = 42L,
           CV = FALSE,
           carryoverTest = TRUE,
           carryover.period = c(1, 2)),
      error = function(e) {
        cat("fect error for", spec$label, ":", conditionMessage(e), "\n")
        NULL
      }
    )
    if (is.null(fit)) next

    att_avg <- fit$att.avg
    att_se  <- if (!is.null(fit$est.avg)) fit$est.avg[1, "S.E."]    else NA_real_
    att_p   <- if (!is.null(fit$est.avg)) fit$est.avg[1, "p.value"] else NA_real_
    co_avg  <- if (!is.null(fit$est.carryover)) fit$est.carryover[1, "ATT.carryover"] else NA_real_
    co_se   <- if (!is.null(fit$est.carryover)) fit$est.carryover[1, "S.E."]          else NA_real_
    co_p    <- if (!is.null(fit$est.carryover)) fit$est.carryover[1, "p.value"]       else NA_real_
    result_rows[[length(result_rows) + 1L]] <- data.frame(
      outcome = spec$label,
      active_estimate = att_avg,
      active_se = att_se,
      active_p_value = att_p,
      carryover_estimate = co_avg,
      carryover_se = co_se,
      carryover_p_value = co_p,
      n_obs = nrow(sub)
    )

    # fect numbers the last active year as raw off-period 0 and the first
    # untreated year as raw off-period 1. Subtract one for calendar event time:
    # -1 = last active year; 0 = first untreated year; +1 = following year.
    # carryover.period = c(1, 2) tests calendar event times 0 and +1.
    # A flat zero post-exit signals that the effect dissipates;
    # a sustained positive level is consistent with persistence, but does not
    # distinguish expertise from continuing relationships or selective exit.
    est_off <- as.data.frame(fit$est.att.off)
    est_off$raw_off_period <- as.integer(rownames(est_off))
    est_off$time <- est_off$raw_off_period - 1L
    est_off$post <- est_off$time >= 0L
    plot_data <- est_off[!is.na(est_off$ATT.OFF), ]

    support_column <- intersect(c("count.off", "count"), names(est_off))
    off_export <- data.frame(
      outcome = spec$label,
      raw_off_period = est_off$raw_off_period,
      event_time_since_exit = est_off$time,
      estimate = est_off$ATT.OFF,
      standard_error = est_off$S.E.,
      p_value = est_off$p.value,
      ci_lower = est_off$CI.lower,
      ci_upper = est_off$CI.upper,
      n_firm_year_cells = if (length(support_column)) est_off[[support_column[1]]] else fit$count.off,
      in_carryover_test = est_off$raw_off_period %in% c(1L, 2L)
    )
    write.csv(off_export, file.path(table_dir, sprintf("firm_fect_exit_%s.csv", spec$label)), row.names = FALSE)

    stars_active <- if (is.na(att_p)) "" else if (att_p < 0.01) "***" else
      if (att_p < 0.05) "**" else if (att_p < 0.10) "*" else ""
    stars_co <- if (is.na(co_p)) "" else if (co_p < 0.01) "***" else
      if (co_p < 0.05) "**" else if (co_p < 0.10) "*" else ""

    annot_label <- sprintf(
      paste0(
        "Initial-award spell ATT : %s%s  (SE %s, p = %s)",
        "\nCarry-over ATT (successor year + next): %s%s  (SE %s, p = %s)"
      ),
      fmt_with_unit(att_avg, spec$unit), stars_active,
      fmt_with_unit(att_se, spec$unit), if (is.na(att_p)) "--" else sprintf("%.3f", att_p),
      fmt_with_unit(co_avg, spec$unit), stars_co,
      fmt_with_unit(co_se, spec$unit), if (is.na(co_p)) "--" else (if (co_p < 0.001) "< 0.001" else sprintf("%.3f", co_p))
    )

    fig_title <- sprintf("Initial-award spell and successor award: %s", spec$fig_title)
    fig_subtitle <- sprintf(
      "Period-wise ATT relative to successor award (%d cities with multiple initial awards; %s firm-year obs.)",
      nrow(exit_cities), format(nrow(sub), big.mark = ",")
    )
    fig_caption <- "fect estimator (Liu, Wang and Xu 2024). 95% CIs from 200 bootstrap replications.\nEvent time 0 is the next initial sample award; shading marks that year and the following year.\nThis legacy clock excludes constructed renewals and actual contract durations or coverage gaps. It does not verify loss of affiliation."

    p_obj <- ggplot(plot_data, aes(x = time, y = ATT.OFF)) +
      annotate("rect", xmin = -0.5, xmax = 1.5,
               ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      geom_vline(xintercept = -0.5, linetype = "solid", color = "#707070", alpha = 0.5) +
      geom_ribbon(aes(ymin = CI.lower, ymax = CI.upper),
                  fill = "#D9D9D9", alpha = 0.18) +
      geom_line(color = "#202020", linewidth = 0.6) +
      geom_pointrange(aes(ymin = CI.lower, ymax = CI.upper),
                      color = "#202020", size = 0.45, linewidth = 0.5) +
      scale_x_continuous(breaks = unique(plot_data$time)) +
      labs(
        title = NULL,
        subtitle = NULL,
        x = "Years relative to successor initial award",
        y = spec$y_lab,
        caption = paste(unlist(lapply(
          strsplit(paste(annot_label, fig_subtitle, fig_caption, sep = "\n"), "\n")[[1]],
          strwrap, width = 100)), collapse = "\n")
      ) +
      theme_classic(base_size = 10, base_family = "sans") +
      theme(plot.title = element_text(face = "bold", size = 12),
            plot.subtitle = element_text(size = 9.5, color = "grey30"),
            plot.caption = element_text(size = 9, lineheight = 1.5, color = "grey25", hjust = 0,
                                        margin = margin(t = 12)),
            plot.caption.position = "plot",
            axis.title.x = element_text(margin = margin(t = 9)),
            plot.margin = margin(8, 12, 8, 8),
            panel.grid = element_blank())

    fig_path <- file.path(figure_dir, sprintf("firm_fect_exit_%s.pdf", spec$label))
    pdf(fig_path, width = 7.4, height = 5.8, family = "Helvetica")
    print(p_obj)
    dev.off()
    cat("Wrote", fig_path, "\n")

    rows <- c(rows, paste(
      spec$table_label, "&",
      paste0(fmt_num(att_avg), if (stars_active != "") sprintf("$^{%s}$", stars_active) else ""),
      "&", paste0("(", fmt_num(att_se), ")"), "&",
      paste0(fmt_num(co_avg), if (stars_co != "") sprintf("$^{%s}$", stars_co) else ""),
      "&", paste0("(", fmt_num(co_se), ")"), "&",
      fmt_num(co_p, 3), "&",
      fmt_int(nrow(sub)),
      "\\\\"
    ))
  }
  result_export <- if (length(result_rows)) do.call(rbind, result_rows) else data.frame(
    outcome = character(), active_estimate = numeric(), active_se = numeric(), active_p_value = numeric(),
    carryover_estimate = numeric(), carryover_se = numeric(), carryover_p_value = numeric(), n_obs = integer()
  )
  write.csv(result_export, file.path(table_dir, "firm_fect_carryover_exit_results.csv"), row.names = FALSE)
  if (length(rows) == 0L) rows <- "\\multicolumn{6}{c}{Insufficient sample for fect estimation} \\\\"

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Initial-Award Spells and Carry-over after a Successor Award}",
    "\\label{tab:firm_fect_carryover_exit}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Initial-award spell (treat = 1)} & \\multicolumn{2}{c}{First two successor-award years} & & \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Outcome & ATT & SE & ATT & SE & Carry-over $p$ & N \\\\",
    "\\midrule",
    rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf(paste(
      "\\item \\textit{Note:} Counterfactual imputation with unit and year fixed effects and 200 bootstrap replications in %d cities with multiple initial sample awards (%d firm-year rows before outcome restrictions).",
      "The initial-award spell ends at the city's next initial sample award; the last spell is coded on through the observation period. This legacy clock excludes constructed renewals, actual contract durations, and coverage gaps, and does not verify loss of affiliation.",
      "The post-successor estimate averages the successor-award calendar year (event time 0) and the following year (event time +1); the carry-over test evaluates whether this average equals zero.",
      "These correspond to fect raw off-periods 1 and 2. Annual support counts are firm-year cells contributing to each event period.",
      "Selection into successive awards and continuing relationships can also produce persistence, so this comparison does not isolate a causal effect of losing affiliation.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ), nrow(exit_cities), nrow(fm_exit)),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  out <- file.path(table_dir, "firm_fect_carryover_exit_table.tex")
  writeLines(lines, out)
  cat("Wrote", out, "\n")
}

main()
