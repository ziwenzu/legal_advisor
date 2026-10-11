#!/usr/bin/env Rscript
# admin_lee_bounds_merits.R  (H2 vs composition: trimming bounds)
#
# The merits-only estimate conditions on a post-treatment outcome (whether the
# case reached a merits judgment). Following Lee (2009), this script bounds the
# procurement effect on government wins among cases that would reach the
# merits regardless of procurement, by trimming the outcome distribution of
# the group with the higher merits-selection rate.
#
# Implementation: a two-by-two collapse of the case panel.
#   Treated group : cases in adopting cities, post-award.
#   Comparison    : cases in never-adopting cities in the same years (the
#                   Callaway-Sant'Anna comparison), weighted to the treated
#                   post-period year distribution.
#   Step 1. Selection rates s_T and s_C = share of cases decided on the merits.
#           Trimming fraction p = (s_high - s_low) / s_high.
#   Step 2. In the group with the higher selection rate, trim a fraction p of
#           merits-decided cases from the top (government wins) for the lower
#           bound and from the bottom (government losses) for the upper bound.
#           With a binary outcome, trimming removes probability mass from one
#           tail: lower bound = (mean_high * 1 - p) / (1 - p) for wins removed,
#           and symmetrically for losses.
#   Step 3. Bounds on the merits effect = trimmed treated mean minus comparison
#           mean, net of the pre-period difference between the same groups
#           (a difference-in-differences correction for the level gap).
# Standard errors come from a city-level block bootstrap (200 draws).
#
# Output: output/tables/admin_lee_bounds_table.tex

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  normalizePath(file.path(dirname(normalizePath(sub("^--file=", "", script_arg[1]))), ".."))
}
root_dir <- get_root_dir()
source(file.path(root_dir, "code", "analysis_helpers.R"))
dirs <- sb_dirs(root_dir)
set.seed(2026L)

adm <- fread(file.path(dirs$data, "admin_case_level.csv"), encoding = "UTF-8",
             select = c("case_no", "year", "province", "city", "treated_city", "post", "event_year",
                        "withdraw_case", "end_case", "government_win"))
adm[, city_id := .GRP, by = .(province, city)]
adm[, decisive := as.integer(withdraw_case == 0L & end_case == 0L)]
# Groups: treated-post (T1), treated-pre (T0), never-treated in post years (C1) and pre years (C0)
# "Post years" for the comparison are defined by the treated post-period year mix.
post_year_w <- adm[treated_city == 1L & post == 1L, .N, by = year][, w := N / sum(N)][]
pre_year_w <- adm[treated_city == 1L & post == 0L, .N, by = year][, w := N / sum(N)][]

cell_stats <- function(d) {
  s <- mean(d$decisive)
  m <- if (any(d$decisive == 1L)) mean(d$government_win[d$decisive == 1L]) else NA_real_
  list(s = s, m = m, n = nrow(d), nd = sum(d$decisive))
}

# Year-reweighted never-treated cells
wmean_cells <- function(d, yw) {
  d <- merge(d, yw[, .(year, w)], by = "year")
  d[, w := w / .N, by = year]  # equal weight within year, year weights across
  s <- sum(d$w * d$decisive) / sum(d$w)
  dd <- d[decisive == 1L]
  m <- sum(dd$w * dd$government_win) / sum(dd$w)
  list(s = s, m = m, n = nrow(d), nd = nrow(dd))
}

lee_bounds <- function(d) {
  T1 <- cell_stats(d[treated_city == 1L & post == 1L])
  T0 <- cell_stats(d[treated_city == 1L & post == 0L])
  C1 <- wmean_cells(d[treated_city == 0L], post_year_w)
  C0 <- wmean_cells(d[treated_city == 0L], pre_year_w)
  # DID on the selection rate and the untrimmed merits mean
  sel_did <- (T1$s - T0$s) - (C1$s - C0$s)
  naive_did <- (T1$m - T0$m) - (C1$m - C0$m)
  # Which post-period group is more selected? Trim that group's merits outcomes.
  if (T1$s >= C1$s) {
    p <- (T1$s - C1$s) / T1$s
    m_hi <- T1$m
    lo_T1 <- (m_hi - p) / (1 - p); lo_T1 <- max(lo_T1, 0)   # remove wins
    hi_T1 <- min(m_hi / (1 - p), 1)                          # remove losses
    lower <- (lo_T1 - T0$m) - (C1$m - C0$m)
    upper <- (hi_T1 - T0$m) - (C1$m - C0$m)
  } else {
    p <- (C1$s - T1$s) / C1$s
    m_hi <- C1$m
    lo_C1 <- (m_hi - p) / (1 - p); lo_C1 <- max(lo_C1, 0)
    hi_C1 <- min(m_hi / (1 - p), 1)
    # Trimming wins from the comparison raises the treated-minus-comparison gap (upper bound)
    upper <- (T1$m - T0$m) - (lo_C1 - C0$m)
    lower <- (T1$m - T0$m) - (hi_C1 - C0$m)
  }
  list(p = p, sel_did = sel_did, naive = naive_did, lower = lower, upper = upper,
       sT = T1$s, sC = C1$s)
}

point <- lee_bounds(adm)

# City-block bootstrap
cities <- unique(adm[, .(city_id, treated_city)])
B <- 200L
boot <- matrix(NA_real_, B, 3)
idx_by_city <- split(seq_len(nrow(adm)), adm$city_id)
for (b in seq_len(B)) {
  draw <- sample(cities$city_id, nrow(cities), replace = TRUE)
  rows <- unlist(idx_by_city[as.character(draw)], use.names = FALSE)
  r <- lee_bounds(adm[rows])
  boot[b, ] <- c(r$naive, r$lower, r$upper)
}
se <- apply(boot, 2, sd)

header <- c(" & Estimate & Bootstrap SE \\\\")
body <- c(
  paste("Merits-selection rate, adopting cities post-award &", fmt_num(point$sT), "& \\\\"),
  paste("Merits-selection rate, never-adopting cities (same years) &", fmt_num(point$sC), "& \\\\"),
  paste("DID on the merits-selection rate &", fmt_num(point$sel_did), "& \\\\"),
  paste("Trimming fraction $p$ &", fmt_num(point$p), "& \\\\"),
  "\\addlinespace",
  paste("Untrimmed merits-only DID &", fmt_num(point$naive), "&", fmt_num(se[1]), "\\\\"),
  paste("Lee lower bound &", fmt_num(point$lower), "&", fmt_num(se[2]), "\\\\"),
  paste("Lee upper bound &", fmt_num(point$upper), "&", fmt_num(se[3]), "\\\\")
)
note <- paste(
  "Trimming bounds for the procurement effect on government wins among cases that reach a merits judgment (Lee 2009), computed on a two-by-two collapse of the case panel: adopting cities before and after the award against never-adopting cities in the same years, with the comparison cells reweighted to the treated year distribution.",
  "Because procurement lowers the share of cases that reach the merits (withdrawals rise), the comparison group is the more selected one and its merits outcomes are trimmed by the fraction $p$ from the top (wins) for the upper bound and from the bottom (losses) for the lower bound.",
  "Bounds are net of the pre-period level difference between the groups.",
  "Standard errors from 200 city-block bootstrap replications."
)
write_tpt_table(file.path(dirs$tables, "admin_lee_bounds_table.tex"),
                "Trimming Bounds for the Merits-Only Effect",
                "tab:admin_lee_bounds", "lcc", header, body, note)
