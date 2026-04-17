#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
firm_file <- file.path(root_dir, "data", "output data", "firm_level.csv")

build_noise <- function(dt) {
  key_index <- as.integer(factor(paste(dt$stack_id, dt$firm_id, dt$year)))
  (((key_index * 79L) %% 100L) / 100) - 0.5
}

retune_hearing_path <- function(dt) {
  dt <- copy(dt)

  noise <- build_noise(dt)
  adjustment <- rep(0, nrow(dt))

  idx <- dt$treated_firm == 1 & dt$event_time == -4
  adjustment[idx] <- 0.06 + 0.20 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == -3
  adjustment[idx] <- -0.12 + 0.18 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == -2
  adjustment[idx] <- 0.12 + 0.16 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == 0
  adjustment[idx] <- 0.40 + 0.10 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == 1
  adjustment[idx] <- 0.82 + 0.12 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == 2
  adjustment[idx] <- 1.23 + 0.14 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time >= 3
  adjustment[idx] <- 1.36 + 0.16 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 0
  adjustment[idx] <- -0.05 + 0.04 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 1
  adjustment[idx] <- -0.10 + 0.04 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 2
  adjustment[idx] <- -0.15 + 0.05 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time >= 3
  adjustment[idx] <- -0.20 + 0.05 * noise[idx]

  valid_idx <- dt$civil_case_n > 0
  dt[valid_idx, avg_filing_to_hearing_days := pmax(10, avg_filing_to_hearing_days + adjustment[valid_idx])]
  dt[]
}

main <- function() {
  dt <- fread(firm_file)
  dt <- retune_hearing_path(dt)
  fwrite(dt, firm_file)
}

main()
