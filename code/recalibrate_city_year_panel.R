#!/usr/bin/env Rscript

stop(
  paste(
    "This script is intentionally disabled.",
    "Re-running in-place recalibration on the final city_year_panel.csv",
    "will stack additional shocks onto an already calibrated panel.",
    "Create a separate baseline copy before any new calibration work."
  )
)
