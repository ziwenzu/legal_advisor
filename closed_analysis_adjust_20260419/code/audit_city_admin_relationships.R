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
admin_file <- file.path(root_dir, "data", "admin_case_level.csv")
city_file <- file.path(root_dir, "data", "city_year_panel.csv")

admin_dt <- fread(admin_file)
city_dt <- fread(city_file)

overlap_years <- intersect(sort(unique(admin_dt$year)), sort(unique(city_dt$year)))
admin_cmp <- admin_dt[year %in% overlap_years]
city_cmp <- city_dt[year %in% overlap_years]

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

agg <- agg[year %in% overlap_years]

merged <- merge(
  city_cmp,
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

out <- rbindlist(lapply(metrics, function(v) {
  x <- merged[[paste0(v, "_city")]]
  y <- merged[[paste0(v, "_admin")]]
  data.table(
    variable = v,
    max_abs_diff = max(abs(x - y), na.rm = TRUE),
    all_equal = isTRUE(all.equal(x, y, tolerance = 1e-12))
  )
}))

cat("case_no_duplicates=", anyDuplicated(admin_dt$case_no), "\n")
cat("city_year_panel_key_duplicates=", city_dt[, anyDuplicated(sprintf('%s|%s|%s', province, city, year))], "\n")
cat("comparison_years=", paste(overlap_years, collapse = ","), "\n")
cat("unmatched_city_rows=", merged[, sum(is.na(government_win_rate_city))], "\n")
cat("unmatched_admin_rows=", merged[, sum(is.na(government_win_rate_admin))], "\n")
print(out)
