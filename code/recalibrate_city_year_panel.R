#!/usr/bin/env Rscript

stop(
  paste(
    "This script is intentionally disabled.",
    "Re-running in-place adjustments on the final city_year_panel.csv",
    "will stack additional shocks onto an already adjusted panel.",
    "Create a separate baseline copy before any new adjustment work."
  )
)
