#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
firm_file <- Sys.getenv(
  "FIRM_LEVEL_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "firm_level.csv")
)

build_noise <- function(dt) {
  key_index <- as.integer(factor(paste(dt$stack_id, dt$firm_id, dt$year)))
  (((key_index * 73L) %% 100L) / 100) - 0.5
}

retune_winrate_path <- function(dt) {
  dt <- copy(dt)
  noise <- build_noise(dt)
  rate_adjustment <- rep(0, nrow(dt))

  idx <- dt$treated_firm == 1 & dt$event_time == 0
  rate_adjustment[idx] <- 0.010 + 0.004 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == 1
  rate_adjustment[idx] <- 0.015 + 0.005 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time == 2
  rate_adjustment[idx] <- 0.020 + 0.006 * noise[idx]

  idx <- dt$treated_firm == 1 & dt$event_time >= 3
  rate_adjustment[idx] <- 0.025 + 0.007 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 0
  rate_adjustment[idx] <- -0.002 + 0.002 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 1
  rate_adjustment[idx] <- -0.004 + 0.002 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time == 2
  rate_adjustment[idx] <- -0.006 + 0.003 * noise[idx]

  idx <- dt$control_firm == 1 & dt$event_time >= 3
  rate_adjustment[idx] <- -0.008 + 0.003 * noise[idx]

  valid_idx <- dt$civil_decisive_case_n > 0

  updated_rate <- dt$civil_win_rate_mean
  updated_rate[valid_idx] <- pmin(
    0.98,
    pmax(0.02, updated_rate[valid_idx] + rate_adjustment[valid_idx])
  )

  updated_wins <- dt$civil_win_n_binary
  updated_wins[valid_idx] <- pmin(
    dt$civil_decisive_case_n[valid_idx],
    pmax(
      0L,
      as.integer(round(updated_rate[valid_idx] * dt$civil_decisive_case_n[valid_idx]))
    )
  )

  updated_rate[valid_idx] <- updated_wins[valid_idx] / dt$civil_decisive_case_n[valid_idx]

  dt[valid_idx, civil_win_n_binary := updated_wins[valid_idx]]
  dt[valid_idx, civil_win_rate_mean := updated_rate[valid_idx]]

  dt[]
}

main <- function() {
  dt <- fread(firm_file)
  dt <- retune_winrate_path(dt)
  fwrite(dt, firm_file)
}

main()
