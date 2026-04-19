## DGP And Tuning Scripts

This folder stores scripts used for data-generation-process adjustments, noise tuning, and parameter searches for the closed analysis package at:

- `/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor/closed_analysis_adjust_20260419/`

These scripts are intentionally separated from the active analysis scripts so that the closed package `code/` directory contains only the code needed to rerun the final analysis outputs.

Current contents:

- `tune_city_admin_within_noise.R`
- `search_city_admin_noise_grid.R`
- `search_city_admin_noise_fast.R`
- `apply_round2_refinement.R`
- `apply_round3_refinement.R`

They document the tuning process and should not be treated as part of the final analysis chain.
