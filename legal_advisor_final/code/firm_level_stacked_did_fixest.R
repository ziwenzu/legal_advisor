#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
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
source(file.path(root_dir,"code","analysis_helpers.R"))
input_file <- file.path(root_dir, "data", "firm_level.csv")
figure_dir <- file.path(root_dir, "output", "figures")
table_dir <- file.path(root_dir, "output", "tables")
output_tag <- Sys.getenv("FIRM_LEVEL_OUTPUT_TAG", unset = "")
control_note <- Sys.getenv(
  "FIRM_LEVEL_CONTROL_NOTE",
  unset = "comparison firms sampled within the same stack"
)

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

build_output_name <- function(stem, ext) {
  if (nzchar(output_tag)) {
    sprintf("%s_%s.%s", output_tag, stem, ext)
  } else {
    sprintf("%s.%s", stem, ext)
  }
}

stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("$^{***}$")
  if (p_value < 0.05) return("$^{**}$")
  if (p_value < 0.10) return("$^{*}$")
  ""
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_int <- function(x) {
  if (is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("<0.001")
  sprintf("%.3f", p_value)
}

single_pre_expr <- function(p_value) {
  bquote("Pre-period joint test: " * italic(p) * " = " * .(fmt_p(p_value)))
}

read_firm_panel <- function(path) {
  dt <- fread(path, encoding = "UTF-8")

  dt[, stack_firm_fe := sprintf("%s__%s", award_id, firm_id)]
  dt[, stack_year_fe := sprintf("%s__%s", award_id, year)]

  dt[, event_time_window := fifelse(event_time < -5, NA_real_, fifelse(event_time > 5, NA_real_, event_time))]

  if (all(c("enterprise_case_n", "personal_case_n", "civil_case_n") %in% names(dt))) {
    dt[, enterprise_share := fifelse(civil_case_n > 0, enterprise_case_n / civil_case_n, NA_real_)]
    dt[, personal_share := fifelse(civil_case_n > 0, personal_case_n / civil_case_n, NA_real_)]
  }

  if ("firm_size" %in% names(dt)) {
    dt[, log_firm_size := fifelse(!is.na(firm_size) & firm_size > 0, log(firm_size), NA_real_)]
  }

  setorder(dt, award_id, firm_id, year)
  dt[]
}

estimate_main_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  base_filter <- quote(!is.na(event_time_window))
  if (is.null(sample_filter)) {
    work_dt <- work_dt[eval(base_filter)]
  } else {
    work_dt <- work_dt[eval(base_filter) & eval(sample_filter)]
  }

  formula_obj <- as.formula(
    sprintf("%s ~ did_treatment | stack_firm_fe + stack_year_fe", outcome_name)
  )

  feols(
    formula_obj,
    data = work_dt,
    cluster = ~ award_id + firm_id
  )
}

estimate_event_model <- function(dt, outcome_name, sample_filter = NULL) {
  work_dt <- copy(dt)

  base_filter <- quote(!is.na(event_time_window))
  if (is.null(sample_filter)) {
    work_dt <- work_dt[eval(base_filter)]
  } else {
    work_dt <- work_dt[eval(base_filter) & eval(sample_filter)]
  }

  formula_obj <- as.formula(
    sprintf("%s ~ i(event_time_window, treated_firm, ref = -1) | stack_firm_fe + stack_year_fe", outcome_name)
  )

  feols(
    formula_obj,
    data = work_dt,
    cluster = ~ award_id + firm_id
  )
}

extract_main_coef <- function(model) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[term == "did_treatment"]
  list(
    estimate = row[["Estimate"]],
    se = row[["Std. Error"]],
    p_value = row[["Pr(>|t|)"]],
    n_obs = nobs(model),
    r2 = fitstat(model, "r2")
  )
}

extract_event_dt <- function(model) {
  ip <- iplot(model, only.params = TRUE)
  dt <- as.data.table(ip$prms)
  dt[
    ,
    .(
      event_time = as.numeric(estimate_names),
      estimate,
      ci_lo = fifelse(is_ref, 0, ci_low),
      ci_hi = fifelse(is_ref, 0, ci_high),
      is_ref
    )
  ][order(event_time)]
}

extract_pretest <- function(model, pre_periods = c(-5, -4, -3, -2)) {
  term_names <- sprintf("event_time_window::%s:treated_firm", pre_periods)
  present_terms <- intersect(names(coef(model)), term_names)

  if (length(present_terms) == 0) {
    return(list(stat = NA_real_, p_value = NA_real_, df1 = NA_real_))
  }

  escaped_terms <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", present_terms)
  keep_pattern <- paste0("^(", paste(escaped_terms, collapse = "|"), ")$")
  test_obj <- suppressMessages(wald(model, keep = keep_pattern, print = FALSE))

  list(
    stat = unname(test_obj$stat),
    p_value = unname(test_obj$p),
    df1 = unname(test_obj$df1)
  )
}

plot_event_study <- function(event_dt, outcome_label, y_title, main_effect, main_se, pre_p, file_path) {
  sb_plot_event(event_dt,y_title,"Years since the contract",file_path,main_effect=main_effect,main_se=main_se,pre_p=pre_p)
}

plot_client_mix_event <- function(event_dt, ate, se, pre_p, file_path, y_label="Enterprise share of cases") {
  sb_plot_event(event_dt,y_label,"Years since the contract",file_path,main_effect=ate,main_se=se,pre_p=pre_p)
}

build_main_table <- function(results_list, file_path) {
  column_keys <- c(
    "civil_win_rate_mean",
    "avg_filing_to_hearing_days",
    "civil_win_rate_fee_mean"
  )

  coef_row <- sapply(column_keys, function(key) {
    res <- results_list[[key]]
    paste0(fmt_num(res$estimate), stars(res$p_value))
  })
  se_row <- sapply(column_keys, function(key) {
    res <- results_list[[key]]
    paste0("(", fmt_num(res$se), ")")
  })
  obs_row <- sapply(column_keys, function(key) fmt_int(results_list[[key]]$n_obs))
  r2_row <- sapply(column_keys, function(key) fmt_num(results_list[[key]]$r2))

  sample_row <- c(
    "Decisive civil cases",
    "Civil cases",
    "Decisive cases with fee share"
  )

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Stacked Difference-in-Differences Estimates for Firm-Year Civil Litigation Outcomes}",
    "\\label{tab:firm_level_stacked_did}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lccc}",
    "\\toprule",
    " & (1) & (2) & (3) \\\\",
    "Outcome & Civil Win Rate & Average Hearing Days & Fee-Based Win Rate \\\\",
    "\\midrule",
    paste("Winner $\\times$ Post &", paste(coef_row, collapse = " & "), "\\\\"),
    paste("&", paste(se_row, collapse = " & "), "\\\\"),
    "\\addlinespace",
    paste("Sample &", paste(sample_row, collapse = " & "), "\\\\"),
    paste("Observations &", paste(obs_row, collapse = " & "), "\\\\"),
    paste("$R^2$ &", paste(r2_row, collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Firm Fixed Effects &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    paste("Stack $\\times$ Year Fixed Effects &", paste(rep("Yes", 3), collapse = " & "), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Stacked difference-in-differences (DID) coefficients on Winner $\\times$ Post.",
      sprintf("Treatment group: procurement winners; control group: %s.", control_note),
      "Estimation samples differ by column because each outcome is defined on firm-year cells with a positive denominator: column 1 keeps cells with at least one decisive civil case; column 2 keeps cells with at least one civil case and a non-missing filing-to-hearing duration; column 3 keeps cells with at least one decisive case for which the fee allocation is observed.",
      "All firm-year specifications are estimated on the event-time window [-5, 5], matching the firm-level event-study figures.",
      "Because outcomes are already collapsed to the firm-year level, the stacked DID does not add case controls.",
      "Standard errors clustered by stack and firm.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

build_mechanism_table <- function(results_list, file_path) {
  res <- results_list[["enterprise_share"]]
  coef <- paste0(fmt_num(res$estimate), stars(res$p_value))
  se <- paste0("(", fmt_num(res$se), ")")

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    "\\caption{Procurement Wins Reallocate Firm Caseload Toward Enterprise Clients}",
    "\\label{tab:firm_level_client_mix}",
    "\\begin{threeparttable}",
    "\\begin{tabular}{lc}",
    "\\toprule",
    " & (1) \\\\",
    "Outcome & Enterprise share \\\\",
    "\\midrule",
    paste0("Winner $\\times$ Post & ", coef, " \\\\"),
    paste0("& ", se, " \\\\"),
    "\\addlinespace",
    paste0("Observations & ", fmt_int(res$n_obs), " \\\\"),
    paste0("$R^2$ & ", fmt_num(res$r2), " \\\\"),
    "Stack $\\times$ Firm Fixed Effects & Yes \\\\",
    "Stack $\\times$ Year Fixed Effects & Yes \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    paste(
      "\\item \\textit{Note:} Stacked difference-in-differences (DID) coefficient on Winner $\\times$ Post.",
      sprintf("Treatment group: procurement winners; control group: %s.", control_note),
      "Outcome is the within-firm-year share of civil cases representing enterprise clients; the personal-client share is the mechanical complement and is omitted.",
      "Standard errors clustered by stack and firm.",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )

  writeLines(lines, con = file_path)
}

main <- function() {
  dt <- read_firm_panel(input_file)
  event_exports <- list()
  save_event <- function(model, outcome) {
    cells <- extract_event_dt(model)
    terms <- as.data.table(coeftable(model), keep.rownames = "term")
    names <- sprintf("event_time_window::%s:treated_firm", cells$event_time)
    cells[, `:=`(se = terms[["Std. Error"]][match(names, terms$term)],
                 p_value = terms[["Pr(>|t|)"]][match(names, terms$term)], outcome = outcome)]
    eligible <- dt[!is.na(event_time_window) & !is.na(get(outcome))]
    support <- eligible[, .(eligible_firm_years=.N,
      eligible_winner_firms=uniqueN(firm_id[treated_firm==1L]),
      eligible_comparison_firms=uniqueN(firm_id[treated_firm==0L]),
      eligible_winner_stacks=uniqueN(award_id[treated_firm==1L])),
      by=.(event_time=event_time_window)]
    event_exports[[outcome]] <<- merge(cells,support,by="event_time",all.x=TRUE)
  }

  model_specs <- list(
    civil_win_rate_mean = list(
      label = "Civil Win Rate",
      y_title = "Firm Win Rate (Decisive Civil Cases)",
      sample_filter = quote(civil_decisive_case_n > 0)
    ),
    avg_filing_to_hearing_days = list(
      label = "Average Hearing Time",
      y_title = "Average Filing-to-Hearing Days",
      sample_filter = quote(civil_case_n > 0)
    )
  )

  results_list <- list()

  for (outcome_name in names(model_specs)) {
    spec <- model_specs[[outcome_name]]

    main_model <- estimate_main_model(
      dt = dt,
      outcome_name = outcome_name,
      sample_filter = spec$sample_filter
    )

    event_model <- estimate_event_model(
      dt = dt,
      outcome_name = outcome_name,
      sample_filter = spec$sample_filter
    )
    save_event(event_model,outcome_name)

    main_coef <- extract_main_coef(main_model)
    results_list[[outcome_name]] <- main_coef
    pre_test <- extract_pretest(event_model)

    plot_event_study(
      event_dt = extract_event_dt(event_model),
      outcome_label = spec$label,
      y_title = spec$y_title,
      main_effect = main_coef$estimate,
      main_se = main_coef$se,
      pre_p = pre_test$p_value,
      file_path = file.path(
        figure_dir,
        build_output_name(sprintf("firm_level_%s_event_study", outcome_name), "pdf")
      )
    )
  }

  fee_spec <- list(
    label = "Civil Fee Win Rate",
    y_title = "Firm Fee-Based Win Rate",
    sample_filter = quote(civil_fee_decisive_case_n > 0)
  )
  fee_model <- estimate_main_model(
    dt = dt,
    outcome_name = "civil_win_rate_fee_mean",
    sample_filter = fee_spec$sample_filter
  )
  fee_event_model <- estimate_event_model(
    dt = dt,
    outcome_name = "civil_win_rate_fee_mean",
    sample_filter = fee_spec$sample_filter
  )
  fee_result <- extract_main_coef(fee_model)
  fee_pretest <- extract_pretest(fee_event_model)
  results_list[["civil_win_rate_fee_mean"]] <- fee_result

  plot_event_study(
    event_dt = extract_event_dt(fee_event_model),
    outcome_label = fee_spec$label,
    y_title = fee_spec$y_title,
    main_effect = fee_result$estimate,
    main_se = fee_result$se,
    pre_p = fee_pretest$p_value,
    file_path = file.path(
      figure_dir,
      build_output_name("firm_level_civil_fee_win_rate_event_study", "pdf")
    )
  )

  build_main_table(
    results_list = results_list,
    file_path = file.path(table_dir, build_output_name("firm_level_stacked_did_main_table", "tex"))
  )

  if (all(c("enterprise_share", "personal_share") %in% names(dt))) {
    client_mix_filter <- quote(civil_case_n > 0)
    client_mix_results <- list()

    for (mix_outcome in c("enterprise_share", "personal_share")) {
      mix_main <- estimate_main_model(
        dt = dt,
        outcome_name = mix_outcome,
        sample_filter = client_mix_filter
      )
      mix_event <- estimate_event_model(
        dt = dt,
        outcome_name = mix_outcome,
        sample_filter = client_mix_filter
      )
      client_mix_results[[mix_outcome]] <- list(
        coef = extract_main_coef(mix_main),
        event_dt = extract_event_dt(mix_event),
        pre = extract_pretest(mix_event)
      )
    }

    enterprise_pack <- client_mix_results[["enterprise_share"]]
    personal_pack <- client_mix_results[["personal_share"]]

    plot_client_mix_event(
      event_dt = enterprise_pack$event_dt,
      ate = enterprise_pack$coef$estimate,
      se = enterprise_pack$coef$se,
      pre_p = enterprise_pack$pre$p_value,
      file_path = file.path(
        figure_dir,
        build_output_name("firm_level_client_mix_event_study", "pdf")
      ),
      y_label = "Enterprise Share of Cases"
    )

    build_mechanism_table(
      results_list = list(
        enterprise_share = enterprise_pack$coef,
        personal_share = personal_pack$coef
      ),
      file_path = file.path(
        table_dir,
        build_output_name("firm_level_client_mix_mechanism_table", "tex")
      )
    )
  }

  if ("log_firm_size" %in% names(dt)) {
    size_filter <- quote(!is.na(log_firm_size))
    size_main <- estimate_main_model(dt, "log_firm_size", sample_filter = size_filter)
    size_event <- estimate_event_model(dt, "log_firm_size", sample_filter = size_filter)
    save_event(size_event,"log_firm_size")
    size_coef <- extract_main_coef(size_main)
    size_pretest <- extract_pretest(size_event)

    plot_event_study(
      event_dt = extract_event_dt(size_event),
      outcome_label = "Log Firm Size",
      y_title = "Log Firm Size (Lawyers)",
      main_effect = size_coef$estimate,
      main_se = size_coef$se,
      pre_p = size_pretest$p_value,
      file_path = file.path(
        figure_dir,
        build_output_name("firm_level_log_firm_size_event_study", "pdf")
      )
    )
  }
  if (exists("fee_event_model",inherits=FALSE)) save_event(fee_event_model,"civil_win_rate_fee_mean")
  if (exists("enterprise_pack",inherits=FALSE)) {
    client_event <- estimate_event_model(dt,"enterprise_share",sample_filter=quote(civil_case_n>0))
    save_event(client_event,"enterprise_share")
  }
  fwrite(rbindlist(event_exports,fill=TRUE),file.path(table_dir,build_output_name("firm_event_coefficients_and_support","csv")))
}

main()
