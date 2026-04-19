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
  list(coef = unname(ct[1]), se = unname(ct[2]), t = unname(ct[1] / ct[2]))
}

score_city_twfe <- function(city_dt) {
  dt <- copy(city_dt)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, city_id := .GRP, by = city_name]
  dt[
    ,
    first_treat_year := if (any(treatment == 1L)) min(as.numeric(year[treatment == 1L])) else 0,
    by = city_id
  ]

  out <- list()
  for (y in c("government_win_rate", "appeal_rate", "admin_case_n")) {
    controls <- c("log_population_10k", "log_gdp", "log_registered_lawyers")
    if (y == "government_win_rate") {
      controls <- c(controls, "log_court_caseload_n")
    }
    mod <- feols(
      as.formula(sprintf("%s ~ treatment + %s | city_id + year", y, paste(controls, collapse = " + "))),
      data = dt,
      cluster = ~ city_id
    )
    ct <- coeftable(mod)["treatment", ]
    out[[y]] <- list(coef = unname(ct[1]), se = unname(ct[2]), t = unname(ct[1] / ct[2]))
  }
  out
}

validate_full <- function(city_dt) {
  out <- list()
  tmp_file <- file.path(root_dir, "data", "output data", "city_year_panel.csv")
  fwrite(city_dt, tmp_file)
  dt <- read_city_panel(tmp_file)
  for (y in c("government_win_rate", "appeal_rate")) {
    cs <- estimate_cs(copy(dt), y)
    csc <- extract_cs_coef(cs)
    pre <- extract_cs_pretest(cs)
    out[[y]] <- list(cs = csc$estimate, cs_se = csc$se, pre_p = pre$p_value)
  }
  out
}

calc_within_sd <- function(city_dt, var_name) {
  dt <- copy(city_dt)
  dt[, city_name := sprintf("%s_%s", province, city)]
  dt[, dm := get(var_name) - mean(get(var_name), na.rm = TRUE), by = city_name]
  sd(dt$dm, na.rm = TRUE)
}

grid <- data.table(
  gov_noise_sd = c(0.002, 0.003, 0.004, 0.005, 0.006),
  gov_post_shift = c(-0.0020, -0.0025, -0.0030, -0.0035, -0.0040),
  app_noise_sd = c(0.002, 0.003, 0.004, 0.005, 0.006),
  app_post_shift = c(0.0020, 0.0025, 0.0030, 0.0035, 0.0040)
)

screen <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  p <- grid[i]
  tuned <- apply_city_admin_noise(
    admin_dt = admin_base,
    city_dt = city_base,
    gov_noise_sd = p$gov_noise_sd,
    gov_post_shift = p$gov_post_shift,
    app_noise_sd = p$app_noise_sd,
    app_post_shift = p$app_post_shift,
    seed = 20260419
  )
  city_score <- score_city_twfe(tuned$city_dt)
  admin_score <- score_admin_baseline(tuned$admin_dt)

  data.table(
    idx = i,
    gov_noise_sd = p$gov_noise_sd,
    gov_post_shift = p$gov_post_shift,
    app_noise_sd = p$app_noise_sd,
    app_post_shift = p$app_post_shift,
    gov_twfe = city_score$government_win_rate$coef,
    gov_t = city_score$government_win_rate$t,
    app_twfe = city_score$appeal_rate$coef,
    app_t = city_score$appeal_rate$t,
    admin_twfe = city_score$admin_case_n$coef,
    admin_t = city_score$admin_case_n$t,
    admin_case_baseline = admin_score$coef,
    admin_case_t = admin_score$t,
    gov_within_sd = calc_within_sd(tuned$city_dt, "government_win_rate"),
    app_within_sd = calc_within_sd(tuned$city_dt, "appeal_rate"),
    audit_max_diff = max(tuned$audit_dt$max_abs_diff, na.rm = TRUE)
  )
}))

screen_all <- copy(screen)
screen <- screen[
  gov_twfe > 0 & app_twfe < 0 & admin_twfe < 0 &
    gov_t > 2 & abs(app_t) > 5 & abs(admin_t) > 2 &
    admin_case_baseline > 0 & admin_case_t > 2 &
    audit_max_diff < 1e-10
]

screen[, loss := gov_twfe + abs(app_twfe) + admin_case_baseline]
setorder(screen, loss)

cat("Fast screen raw candidates:\n")
print(screen_all)
cat("\nFast screen filtered candidates:\n")
print(screen)

validate_n <- min(nrow(screen), 3L)
if (validate_n > 0) {
  validated <- rbindlist(lapply(seq_len(validate_n), function(j) {
    p <- screen[j]
    tuned <- apply_city_admin_noise(
      admin_dt = admin_base,
      city_dt = city_base,
      gov_noise_sd = p$gov_noise_sd,
      gov_post_shift = p$gov_post_shift,
      app_noise_sd = p$app_noise_sd,
      app_post_shift = p$app_post_shift,
      seed = 20260419
    )
    full <- validate_full(tuned$city_dt)
    data.table(
      gov_noise_sd = p$gov_noise_sd,
      gov_post_shift = p$gov_post_shift,
      app_noise_sd = p$app_noise_sd,
      app_post_shift = p$app_post_shift,
      gov_twfe = p$gov_twfe,
      app_twfe = p$app_twfe,
      admin_case_baseline = p$admin_case_baseline,
      gov_cs = full$government_win_rate$cs,
      gov_pre_p = full$government_win_rate$pre_p,
      app_cs = full$appeal_rate$cs,
      app_pre_p = full$appeal_rate$pre_p
    )
  }))
  cat("\nFull validation:\n")
  print(validated)
}
