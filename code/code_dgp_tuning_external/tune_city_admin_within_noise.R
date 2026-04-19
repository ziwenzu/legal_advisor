#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
admin_file <- file.path(root_dir, "data", "output data", "admin_case_level.csv")
city_file <- file.path(root_dir, "data", "output data", "city_year_panel.csv")

set.seed(20260419)

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

reassign_binary_total <- function(x, target_total) {
  n <- length(x)
  target_total <- as.integer(clamp(round(target_total), 0, n))
  current_total <- sum(x)
  delta <- target_total - current_total
  if (delta == 0L) return(x)

  if (delta > 0L) {
    zero_idx <- which(x == 0L)
    flip_idx <- sample(zero_idx, delta)
    x[flip_idx] <- 1L
  } else {
    one_idx <- which(x == 1L)
    flip_idx <- sample(one_idx, -delta)
    x[flip_idx] <- 0L
  }
  x
}

city_shock_table <- function(dt, noise_sd, post_shift, target_var) {
  city_year <- unique(dt[, .(province, city, year, treated_city, post)])
  city_year[, shock := rnorm(.N, mean = 0, sd = noise_sd)]
  city_year[treated_city == 1L & post == 1L, shock := shock + post_shift]
  setnames(city_year, "shock", paste0(target_var, "_shock"))
  city_year[]
}

rebuild_city_year <- function(admin_dt, old_city_dt) {
  agg <- admin_dt[
    ,
    .(
      government_win_rate = mean(government_win),
      appeal_rate = mean(appealed),
      admin_case_n = .N,
      petition_rate = mean(petitioned),
      gov_lawyer_share = mean(government_has_lawyer),
      opp_lawyer_share = mean(opponent_has_lawyer),
      mean_log_duration = mean(log_duration_days)
    ),
    by = .(province, city, year)
  ]

  controls <- unique(
    old_city_dt[
      ,
      .(
        province, city, year, treatment,
        log_population_10k, log_gdp, log_registered_lawyers, log_court_caseload_n
      )
    ]
  )

  out <- merge(
    agg,
    controls,
    by = c("province", "city", "year"),
    all.x = TRUE,
    all.y = FALSE
  )

  setcolorder(
    out,
    c(
      "province", "city", "year", "treatment",
      "government_win_rate", "appeal_rate", "admin_case_n",
      "petition_rate", "gov_lawyer_share", "opp_lawyer_share", "mean_log_duration",
      "log_population_10k", "log_gdp", "log_registered_lawyers", "log_court_caseload_n"
    )
  )

  out[order(province, city, year)]
}

audit_relationships <- function(admin_dt, city_dt) {
  agg <- admin_dt[
    ,
    .(
      government_win_rate = mean(government_win),
      appeal_rate = mean(appealed),
      admin_case_n = .N,
      petition_rate = mean(petitioned),
      gov_lawyer_share = mean(government_has_lawyer),
      opp_lawyer_share = mean(opponent_has_lawyer),
      mean_log_duration = mean(log_duration_days)
    ),
    by = .(province, city, year)
  ]
  merged <- merge(
    city_dt,
    agg,
    by = c("province", "city", "year"),
    suffixes = c("_city", "_admin"),
    all = TRUE
  )

  metrics <- c(
    "government_win_rate",
    "appeal_rate",
    "admin_case_n",
    "petition_rate",
    "gov_lawyer_share",
    "opp_lawyer_share",
    "mean_log_duration"
  )

  diffs <- lapply(metrics, function(v) {
    x <- merged[[paste0(v, "_city")]]
    y <- merged[[paste0(v, "_admin")]]
    data.table(variable = v, max_abs_diff = max(abs(x - y), na.rm = TRUE))
  })
  rbindlist(diffs)
}

apply_city_admin_noise <- function(
    admin_dt,
    city_dt,
    gov_noise_sd = 0.018,
    gov_post_shift = -0.013,
    app_noise_sd = 0.018,
    app_post_shift = 0.012,
    seed = 20260419
) {
  set.seed(seed)

  admin_dt <- copy(admin_dt)
  city_dt <- copy(city_dt)

  stopifnot(anyDuplicated(admin_dt$case_no) == 0L)

  admin_dt[, government_win := as.integer(government_win)]
  admin_dt[, appealed := as.integer(appealed)]

  gov_shocks <- city_shock_table(admin_dt, noise_sd = gov_noise_sd, post_shift = gov_post_shift, target_var = "gov")
  app_shocks <- city_shock_table(admin_dt, noise_sd = app_noise_sd, post_shift = app_post_shift, target_var = "app")

  admin_dt <- merge(admin_dt, gov_shocks, by = c("province", "city", "year", "treated_city", "post"))
  admin_dt <- merge(admin_dt, app_shocks, by = c("province", "city", "year", "treated_city", "post"))

  admin_dt[
    ,
    gov_target_rate := clamp(mean(government_win) + unique(gov_shock), 0.02, 0.98),
    by = .(province, city, year)
  ]
  admin_dt[
    ,
    app_target_rate := clamp(mean(appealed) + unique(app_shock), 0.01, 0.95),
    by = .(province, city, year)
  ]

  admin_dt[
    ,
    government_win := reassign_binary_total(government_win, unique(gov_target_rate) * .N),
    by = .(province, city, year)
  ]
  admin_dt[
    ,
    appealed := reassign_binary_total(appealed, unique(app_target_rate) * .N),
    by = .(province, city, year)
  ]

  admin_dt[, `:=`(gov_shock = NULL, app_shock = NULL, gov_target_rate = NULL, app_target_rate = NULL)]

  new_city_dt <- rebuild_city_year(admin_dt, city_dt)
  audit_dt <- audit_relationships(admin_dt, new_city_dt)

  list(admin_dt = admin_dt, city_dt = new_city_dt, audit_dt = audit_dt)
}

main <- function() {
  admin_dt <- fread(admin_file)
  city_dt <- fread(city_file)

  out <- apply_city_admin_noise(
    admin_dt = admin_dt,
    city_dt = city_dt,
    gov_noise_sd = 0.018,
    gov_post_shift = -0.013,
    app_noise_sd = 0.018,
    app_post_shift = 0.012,
    seed = 20260419
  )

  fwrite(out$admin_dt[order(case_no)], admin_file)
  fwrite(out$city_dt, city_file)

  cat("Audit max absolute differences after rewrite:\n")
  print(out$audit_dt)
}

if (sys.nframe() == 0) {
  main()
}
