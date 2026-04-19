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

source(file.path(root_dir, "code", "tune_city_admin_within_noise.R"), local = TRUE)

admin_base <- fread(file.path(root_dir, "data", "round1_snapshot", "admin_case_level.csv"))
city_base <- fread(file.path(root_dir, "data", "round1_snapshot", "city_year_panel.csv"))

out <- apply_city_admin_noise(
  admin_dt = admin_base,
  city_dt = city_base,
  gov_noise_sd = 0.013,
  gov_post_shift = -0.0075,
  app_noise_sd = 0.013,
  app_post_shift = 0.0075,
  seed = 20260419
)

fwrite(out$admin_dt[order(case_no)], file.path(root_dir, "data", "output data", "admin_case_level.csv"))
fwrite(out$city_dt, file.path(root_dir, "data", "output data", "city_year_panel.csv"))

cat("Applied round-3 refinement.\n")
print(out$audit_dt)
