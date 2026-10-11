#!/usr/bin/env Rscript
# city_year_dose_response.R  (H2: dose-response in contract value and scope)
#
# Splits the city-year Treatment x Post effect by the value of the contract in
# force (terciles of the tender's award amount) and by its scope (number of
# municipal bodies covered, above vs below the median), using the tender
# attributes file. Larger and broader contracts should buy more capacity.
#
# Output: output/tables/city_year_dose_response_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)

cy <- fread(file.path(dirs$data, "city_year_panel.csv"), encoding = "UTF-8")
sup <- fread(file.path(dirs$data, "city_year_supplement.csv"), encoding = "UTF-8")
cy <- merge(cy, sup, by = c("province", "city", "year"))
cy[, city_id := .GRP, by = .(province, city)]
tender <- fread(file.path(dirs$data, "city_tender_attributes.csv"), encoding = "UTF-8")
cuts <- quantile(tender$contract_value_yuan, c(1/3, 2/3))
scope_med <- median(tender$contract_scope_n_bodies)
cy[, value_tercile := fifelse(is.na(active_contract_value_yuan), NA_integer_,
                       fifelse(active_contract_value_yuan <= cuts[1], 1L,
                       fifelse(active_contract_value_yuan <= cuts[2], 2L, 3L)))]
cy[, `:=`(t_v1 = as.integer(treatment == 1L & value_tercile == 1L),
          t_v2 = as.integer(treatment == 1L & value_tercile == 2L),
          t_v3 = as.integer(treatment == 1L & value_tercile == 3L),
          t_scope_hi = as.integer(treatment == 1L & active_contract_scope > scope_med),
          t_scope_lo = as.integer(treatment == 1L & active_contract_scope <= scope_med),
          t_logvalue = fifelse(treatment == 1L, log(active_contract_value_yuan) - log(90000), 0))]
cy[is.na(t_v1), `:=`(t_v1 = 0L, t_v2 = 0L, t_v3 = 0L)]
cy[is.na(t_scope_hi), `:=`(t_scope_hi = 0L, t_scope_lo = 0L)]

outcomes <- c("government_win_rate", "appeal_rate", "log_admin_case_n")
labels <- c("Government win rate", "Appeal rate", "Log(1 + administrative cases)")
ctrl <- function(o) if (o == "government_win_rate") "log_population_10k + log_gdp + log_registered_lawyers + log_published_document_n" else "log_population_10k + log_gdp + log_registered_lawyers"

fit <- function(o) {
  mv <- feols(as.formula(sprintf("%s ~ t_v1 + t_v2 + t_v3 + %s | city_id + year", o, ctrl(o))), data = cy, cluster = ~ city_id)
  ms <- feols(as.formula(sprintf("%s ~ t_scope_lo + t_scope_hi + %s | city_id + year", o, ctrl(o))), data = cy, cluster = ~ city_id)
  ml <- feols(as.formula(sprintf("%s ~ treatment + t_logvalue + %s | city_id + year", o, ctrl(o))), data = cy, cluster = ~ city_id)
  list(v1 = pull_term(mv, "t_v1"), v2 = pull_term(mv, "t_v2"), v3 = pull_term(mv, "t_v3"),
       lo = pull_term(ms, "t_scope_lo"), hi = pull_term(ms, "t_scope_hi"),
       base = pull_term(ml, "treatment"), slope = pull_term(ml, "t_logvalue"),
       p_v = diff_test(mv, "t_v3", "t_v1")$p_value, p_s = diff_test(ms, "t_scope_hi", "t_scope_lo")$p_value,
       n = nobs(mv))
}
res <- lapply(outcomes, fit)
cell <- function(key, which) sapply(res, function(r) coef_cell(r[[key]]$estimate, r[[key]]$se, r[[key]]$p_value)[which])
header <- c(" & (1) & (2) & (3) \\\\", paste("Outcome &", paste(labels, collapse = " & "), "\\\\"))
body <- c(
  "\\multicolumn{4}{l}{\\textit{Panel A. By tercile of the contract value in force}} \\\\",
  paste(sprintf("Treatment $\\times$ Post, bottom tercile ($\\le$ %s yuan) &", fmt_int(cuts[1])), paste(cell("v1", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("v1", 2), collapse = " & "), "\\\\"),
  paste("Treatment $\\times$ Post, middle tercile &", paste(cell("v2", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("v2", 2), collapse = " & "), "\\\\"),
  paste(sprintf("Treatment $\\times$ Post, top tercile ($>$ %s yuan) &", fmt_int(cuts[2])), paste(cell("v3", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("v3", 2), collapse = " & "), "\\\\"),
  paste("Top $=$ bottom, $p$ &", paste(sapply(res, function(r) fmt_p(r$p_v)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{Panel B. By scope of the contract in force (bodies covered)}} \\\\",
  paste(sprintf("Treatment $\\times$ Post, scope $\\le$ %d bodies &", as.integer(scope_med)), paste(cell("lo", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("lo", 2), collapse = " & "), "\\\\"),
  paste(sprintf("Treatment $\\times$ Post, scope $>$ %d bodies &", as.integer(scope_med)), paste(cell("hi", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("hi", 2), collapse = " & "), "\\\\"),
  paste("High $=$ low, $p$ &", paste(sapply(res, function(r) fmt_p(r$p_s)), collapse = " & "), "\\\\"),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{Panel C. Linear in log contract value}} \\\\",
  paste("Treatment $\\times$ Post (at 90,000 yuan) &", paste(cell("base", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("base", 2), collapse = " & "), "\\\\"),
  paste("Treatment $\\times$ Post $\\times$ log(value / 90,000) &", paste(cell("slope", 1), collapse = " & "), "\\\\"),
  paste("&", paste(cell("slope", 2), collapse = " & "), "\\\\"),
  "\\addlinespace",
  paste("Observations &", paste(sapply(res, function(r) fmt_int(r$n)), collapse = " & "), "\\\\"),
  "City controls & Yes & Yes & Yes \\\\",
  "City / Year FE & Yes & Yes & Yes \\\\"
)
note <- paste(
  "Dose-response in the city-year TWFE specification. The treatment indicator is split by the award amount (Panel A, terciles across tenders) and the number of municipal bodies covered (Panel B) of the contract in force in that city-year, and interacted with log contract value centered at the corpus median (Panel C).",
  "Observed award amounts are distinguished from teaching contract attributes. Generated renewals are identified in the tender ledger; each city-year uses its contract in force.",
  "Standard errors clustered by city."
)
write_tpt_table(file.path(dirs$tables, "city_year_dose_response_table.tex"),
                "Dose-Response: Contract Value and Scope",
                "tab:city_year_dose_response", "lccc", header, body, note)
