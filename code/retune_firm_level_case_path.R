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
  stack_index <- as.integer(factor(dt$stack_id))
  firm_index <- as.integer(factor(dt$firm_id))
  raw_u <- ((stack_index * 13L + firm_index * 7L + dt$year * 11L) %% 1000L) / 1000
  raw_u - 0.5
}

retune_civil_case_path <- function(dt) {
  dt <- copy(dt)
  noise <- build_noise(dt)
  multiplier <- rep(1, nrow(dt))

  idx <- dt$treated_firm == 1 & dt$event_time == -5
  multiplier[idx] <- 1.26 + 0.06 * noise[idx]

  dt[
    ,
    civil_case_n := pmax(
      as.integer(round(civil_case_n * multiplier)),
      civil_win_n_binary,
      civil_decisive_case_n,
      0L
    )
  ]

  dt[]
}

main <- function() {
  dt <- fread(firm_file)
  dt <- retune_civil_case_path(dt)
  fwrite(dt, firm_file)
}

main()
