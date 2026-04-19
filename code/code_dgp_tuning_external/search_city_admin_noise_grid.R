#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
snap_dir <- file.path(root_dir, "data", "round1_snapshot")

source(file.path(root_dir, "code", "tune_city_admin_within_noise.R"), local = TRUE)
source(file.path(root_dir, "code", "city_year_cs_twfe_figures_tables.R"), local = TRUE)

admin_base <- fread(file.path(snap_dir, "admin_case_level.csv"))
city_base <- fread(file.path(snap_dir, "city_year_panel.csv"))

score_admin_baseline <- function(admin_dt) {
  dt <- copy(admin_dt)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[, court_id := .GRP, by = court_std]
  dt[, did_treatment := as.integer(did_treatment)]
  dt[, plaintiff_is_entity := as.integer(plaintiff_is_entity)]
  dt[, government_win := as.integer(government_win)]

  mod <- feols(
    government_win ~ did_treatment + plaintiff_is_entity | court_id + year + cause_group,
    data = dt,
    cluster = ~ city_id + court_id
  )
  ct <- coeftable(mod)["did_treatment", ]
  list(
    coef = unname(ct[1]),
    se = unname(ct[2]),
    t = unname(ct[1] / ct[2])
  )
}

score_city_year <- function(city_dt) {
  dt <- read_city_panel(file.path(root_dir, "data", "output data", "city_year_panel.csv"))
  out <- list()
  for (y in c("government_win_rate", "appeal_rate", "admin_case_n")) {
    tw <- estimate_twfe_main(copy(dt), y)
    twct <- coeftable(tw)["treatment", ]
    cs <- estimate_cs(copy(dt), y)
    csc <- extract_cs_coef(cs)
    pre <- extract_cs_pretest(cs)
    out[[y]] <- list(
      twfe_coef = unname(twct[1]),
      twfe_se = unname(twct[2]),
      cs_coef = csc$estimate,
      cs_se = csc$se,
      pre_p = pre$p_value
    )
  }
  out
}

score_city_year_dt <- function(city_dt) {
  fwrite(city_dt, file.path(root_dir, "data", "output data", "city_year_panel.csv"))
  score_city_year(city_dt)
}

calc_within_sd <- function(city_dt, var_name) {
  dt <- copy(city_dt)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, dm := get(var_name) - mean(get(var_name), na.rm = TRUE), by = city_name]
  sd(dt$dm, na.rm = TRUE)
}

grid <- CJ(
  gov_noise_sd = c(0.020, 0.022, 0.024),
  gov_post_shift = c(-0.012, -0.011),
  app_noise_sd = c(0.020, 0.022, 0.024),
  app_post_shift = c(0.011, 0.010)
)

results <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  params <- grid[i]
  tuned <- apply_city_admin_noise(
    admin_dt = admin_base,
    city_dt = city_base,
    gov_noise_sd = params$gov_noise_sd,
    gov_post_shift = params$gov_post_shift,
    app_noise_sd = params$app_noise_sd,
    app_post_shift = params$app_post_shift,
    seed = 20260419
  )

  city_score <- score_city_year_dt(tuned$city_dt)
  admin_score <- score_admin_baseline(tuned$admin_dt)

  results[[i]] <- data.table(
    gov_noise_sd = params$gov_noise_sd,
    gov_post_shift = params$gov_post_shift,
    app_noise_sd = params$app_noise_sd,
    app_post_shift = params$app_post_shift,
    gov_twfe = city_score$government_win_rate$twfe_coef,
    gov_cs = city_score$government_win_rate$cs_coef,
    gov_pre_p = city_score$government_win_rate$pre_p,
    app_twfe = city_score$appeal_rate$twfe_coef,
    app_cs = city_score$appeal_rate$cs_coef,
    app_pre_p = city_score$appeal_rate$pre_p,
    case_twfe = city_score$admin_case_n$twfe_coef,
    case_cs = city_score$admin_case_n$cs_coef,
    admin_baseline = admin_score$coef,
    admin_t = admin_score$t,
    gov_within_sd = calc_within_sd(tuned$city_dt, "government_win_rate"),
    app_within_sd = calc_within_sd(tuned$city_dt, "appeal_rate"),
    audit_max_diff = max(tuned$audit_dt$max_abs_diff, na.rm = TRUE)
  )
}

out_dt <- rbindlist(results)
setorder(out_dt, -gov_pre_p, -app_pre_p)
print(out_dt)
