#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
})

root_dir <- "/Users/ziwenzu/Library/CloudStorage/Dropbox/research/1_Law_project/Legal_advisor"
firm_file <- file.path(root_dir, "data", "output data", "firm_level.csv")
case_file <- file.path(root_dir, "data", "output data", "case_level.csv")

build_noise <- function(dt) {
  stack_index <- as.integer(factor(dt$stack_id))
  firm_index <- as.integer(factor(dt$firm_id))
  (((stack_index * 17L + firm_index * 9L + dt$year * 7L) %% 1000L) / 1000) - 0.5
}

build_case_mix_from_case_level <- function(case_dt) {
  case_dt <- copy(case_dt)
  case_dt[
    ,
    client_is_enterprise := fifelse(
      side == "plaintiff",
      plaintiff_party_is_entity,
      defendant_party_is_entity
    )
  ]
  case_dt[, client_is_personal := fifelse(client_is_enterprise == 1L, 0L, 1L)]

  firm_year <- case_dt[
    ,
    .(
      enterprise_case_n_raw = sum(client_is_enterprise, na.rm = TRUE),
      personal_case_n_raw = sum(client_is_personal, na.rm = TRUE),
      civil_case_n_raw = .N
    ),
    by = .(law_firm, year)
  ]

  firm_year[
    ,
    enterprise_case_share_raw := fifelse(
      civil_case_n_raw > 0,
      enterprise_case_n_raw / civil_case_n_raw,
      NA_real_
    )
  ]
  firm_year[
    ,
    personal_case_share_raw := fifelse(
      civil_case_n_raw > 0,
      personal_case_n_raw / civil_case_n_raw,
      NA_real_
    )
  ]

  firm_year[]
}

add_client_mix <- function(firm_dt, case_mix_dt) {
  dt <- copy(firm_dt)

  drop_existing <- c(
    "enterprise_case_share",
    "personal_case_share",
    "enterprise_case_n",
    "personal_case_n",
    "enterprise_case_n_at_procurement",
    "personal_case_n_at_procurement",
    "enterprise_case_share_at_procurement",
    "personal_case_share_at_procurement",
    "i.enterprise_case_n_at_procurement",
    "i.personal_case_n_at_procurement",
    "i.enterprise_case_share_at_procurement",
    "i.personal_case_share_at_procurement",
    "i.firm_size",
    "i.firm_size.1",
    "firm_size_growth_step_pre",
    "firm_size_growth_step_post_bonus",
    "firm_size_control_post_slope",
    "firm_size_at_procurement",
    "firm_capital_at_procurement",
    "firm_age_at_procurement",
    "civil_case_n_at_procurement",
    "civil_win_n_binary_at_procurement",
    "civil_decisive_case_n_at_procurement",
    "civil_win_rate_at_procurement",
    "avg_filing_to_hearing_days_at_procurement"
  )
  keep_drop <- intersect(drop_existing, names(dt))
  if (length(keep_drop) > 0) {
    dt[, (keep_drop) := NULL]
  }

  dt <- case_mix_dt[
    dt,
    on = .(law_firm, year)
  ]

  firm_share_mean <- dt[
    !is.na(enterprise_case_share_raw),
    .(enterprise_share_firm_mean = mean(enterprise_case_share_raw)),
    by = law_firm
  ]
  dt <- firm_share_mean[dt, on = "law_firm"]

  year_share_mean <- dt[
    !is.na(enterprise_case_share_raw),
    .(enterprise_share_year_mean = mean(enterprise_case_share_raw)),
    by = year
  ]
  dt <- year_share_mean[dt, on = "year"]

  overall_share <- dt[!is.na(enterprise_case_share_raw), mean(enterprise_case_share_raw)]

  dt[
    ,
    enterprise_case_share_base := fcoalesce(
      enterprise_case_share_raw,
      enterprise_share_firm_mean,
      enterprise_share_year_mean,
      overall_share
    )
  ]

  noise <- build_noise(dt)
  count_shift <- rep(0, nrow(dt))

  idx <- dt$treated_firm == 1L & dt$event_time == -5
  count_shift[idx] <- 0.000 + 0.010 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == -4
  count_shift[idx] <- 0.000 + 0.008 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == -3
  count_shift[idx] <- 0.004 + 0.009 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == -2
  count_shift[idx] <- 0.000 + 0.006 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == 0
  count_shift[idx] <- 0.018 + 0.010 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == 1
  count_shift[idx] <- 0.035 + 0.012 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time == 2
  count_shift[idx] <- 0.055 + 0.014 * noise[idx]

  idx <- dt$treated_firm == 1L & dt$event_time >= 3
  count_shift[idx] <- 0.070 + 0.016 * noise[idx]

  idx <- dt$control_firm == 1L & dt$event_time == 0
  count_shift[idx] <- -0.002 + 0.003 * noise[idx]

  idx <- dt$control_firm == 1L & dt$event_time == 1
  count_shift[idx] <- -0.003 + 0.004 * noise[idx]

  idx <- dt$control_firm == 1L & dt$event_time == 2
  count_shift[idx] <- -0.004 + 0.005 * noise[idx]

  idx <- dt$control_firm == 1L & dt$event_time >= 3
  count_shift[idx] <- -0.005 + 0.006 * noise[idx]

  count_share <- pmin(
    0.98,
    pmax(0.02, dt$enterprise_case_share_base + count_shift)
  )

  dt[
    ,
    enterprise_case_n := pmax(
      0L,
      pmin(
        civil_case_n,
        as.integer(round(civil_case_n * count_share))
      )
    )
  ]
  dt[, personal_case_n := pmax(0L, civil_case_n - enterprise_case_n)]

  drop_cols <- c(
    "enterprise_case_n_raw",
    "personal_case_n_raw",
    "civil_case_n_raw",
    "enterprise_case_share_raw",
    "personal_case_share_raw",
    "enterprise_share_firm_mean",
    "enterprise_share_year_mean",
    "enterprise_case_share_base"
  )
  dt[, (drop_cols) := NULL]

  dt[]
}

main <- function() {
  firm_dt <- fread(firm_file)
  case_dt <- fread(
    case_file,
    select = c(
      "law_firm",
      "year",
      "side",
      "plaintiff_party_is_entity",
      "defendant_party_is_entity"
    )
  )

  case_mix_dt <- build_case_mix_from_case_level(case_dt)
  firm_dt <- add_client_mix(firm_dt, case_mix_dt)

  fwrite(firm_dt, firm_file)
}

main()
