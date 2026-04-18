#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
firm_file <- Sys.getenv(
  "FIRM_LEVEL_INPUT_FILE",
  unset = file.path(root_dir, "data", "output data", "firm_level.csv")
)

draw_step <- function(u, phase = c("pre", "post")) {
  phase <- match.arg(phase)

  if (phase == "post") {
    if (u < 0.12) return(0L)
    if (u < 0.52) return(1L)
    if (u < 0.84) return(2L)
    return(3L)
  }

  if (u < 0.12) return(-2L)
  if (u < 0.34) return(-1L)
  if (u < 0.64) return(0L)
  if (u < 0.88) return(1L)
  2L
}

build_firm_year_size_path <- function(firm_summary) {
  years <- 2010:2020
  out <- vector("list", nrow(firm_summary))

  for (i in seq_len(nrow(firm_summary))) {
    base_size <- max(1L, firm_summary$baseline[i])
    anchor_year <- firm_summary$min_event[i]
    first_treat_year <- firm_summary$first_contract_year[i]

    size_path <- integer(length(years))
    anchor_index <- match(anchor_year, years)
    size_path[anchor_index] <- base_size

    if (anchor_index < length(years)) {
      for (j in seq(anchor_index + 1, length(years))) {
        current_year <- years[j]
        u <- (((i * 37L + current_year * 53L) %% 100L) / 100)

        if (!is.na(first_treat_year) && current_year > first_treat_year) {
          step <- draw_step(u, phase = "post")
        } else {
          step <- draw_step(u, phase = "pre")
        }

        size_path[j] <- max(1L, size_path[j - 1L] + step)
      }
    }

    if (anchor_index > 1L) {
      for (j in seq(anchor_index - 1L, 1L)) {
        current_year <- years[j]
        u <- (((i * 41L + current_year * 47L) %% 100L) / 100)
        step <- draw_step(u, phase = "pre")
        size_path[j] <- max(1L, size_path[j + 1L] - step)
      }
    }

    out[[i]] <- data.table(
      firm_id = firm_summary$firm_id[i],
      year = years,
      firm_size = size_path
    )
  }

  rbindlist(out)
}

retune_firm_size_path <- function(dt) {
  dt <- copy(dt)

  firm_summary <- unique(
    dt[, .(firm_id, firm_size_baseline, first_contract_year, treated_firm, event_year)]
  )[
    ,
    {
      treated_years <- first_contract_year[first_contract_year > 0]
      .(
        baseline = as.integer(round(median(firm_size_baseline))),
        first_contract_year = if (length(treated_years)) as.integer(min(treated_years)) else as.integer(NA),
        min_event = as.integer(min(event_year))
      )
    },
    by = firm_id
  ]

  firm_year_path <- build_firm_year_size_path(firm_summary)

  dt <- firm_year_path[dt, on = .(firm_id, year)]

  procurement_anchor <- unique(
    dt[year == event_year, .(firm_id, year, firm_size_baseline)]
  )
  setnames(procurement_anchor, "firm_size_baseline", "anchor_size")
  dt <- procurement_anchor[dt, on = .(firm_id, year)]
  dt[!is.na(anchor_size), firm_size := anchor_size]
  dt[, anchor_size := NULL]

  dt[]
}

main <- function() {
  dt <- fread(firm_file)
  dt <- retune_firm_size_path(dt)
  fwrite(dt, firm_file)
}

main()
