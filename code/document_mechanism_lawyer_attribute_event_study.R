#!/usr/bin/env Rscript
# document_mechanism_lawyer_attribute_event_study.R
#
# Event-study (rather than pooled moderation) of the document-level
# Winner x Post effect on legal_reasoning_share, by lawyer attribute.
# For each of (Party Membership, Female, High-experience, Master+),
# splits the sample into the two attribute strata and produces a
# two-line event-study figure with the headline overall effect
# overlaid for context. The output isolates which lawyer subgroup
# drives the firm-level effect, which a pooled moderation
# specification cannot.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
input_file <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(0)

OUTCOME <- "legal_reasoning_share"
Y_LABEL <- "Legal Reasoning Share"

read_doc <- function() {
  dt <- fread(input_file)
  dt[, stack_year_fe := sprintf("%s__%s", stack_id, year)]
  dt[, cause_side_fe := sprintf("%s__%s", cause, side)]
  dt[, event_time_window := fifelse(event_time < -5, NA_real_,
                            fifelse(event_time > 5, NA_real_, event_time))]
  dt[, lawyer_practice_years_obs := as.integer(!is.na(lawyer_practice_years))]
  practice_mean <- mean(dt$lawyer_practice_years, na.rm = TRUE)
  practice_sd <- sd(dt$lawyer_practice_years, na.rm = TRUE)
  dt[, lawyer_practice_years_std := (lawyer_practice_years - practice_mean) / practice_sd]
  dt[is.na(lawyer_practice_years_std), lawyer_practice_years_std := 0]
  dt[, lawyer_high_edu := as.integer(lawyer_edu >= 4L)]
  dt[, lawyer_seniority_high := as.integer(!is.na(lawyer_practice_years) & lawyer_practice_years >= 7L)]
  dt[, year_gender_fe := sprintf("%s__%d", year, lawyer_gender)]
  dt
}

event_study <- function(dt, mask_expr) {
  sub <- dt[!is.na(get(OUTCOME)) & !is.na(event_time_window)]
  if (!is.null(mask_expr)) sub <- sub[eval(mask_expr)]
  if (nrow(sub) < 50) return(NULL)
  m <- feols(
    as.formula(sprintf("%s ~ i(event_time_window, treated_firm, ref = -1) + opponent_has_lawyer + lawyer_practice_years_std + lawyer_practice_years_obs | firm_id + stack_year_fe + cause_side_fe + court + year_gender_fe", OUTCOME)),
    data = sub, cluster = ~ firm_id + stack_id
  )
  ip <- iplot(m, only.params = TRUE)
  ev <- as.data.table(ip$prms)
  ev[, .(
    event_time = as.numeric(estimate_names),
    estimate,
    ci_lo = fifelse(is_ref, 0, ci_low),
    ci_hi = fifelse(is_ref, 0, ci_high),
    is_ref
  )][order(event_time)]
}

draw_compare <- function(ev_a, ev_b, label_a, label_b, panel_label, file_path) {
  if (is.null(ev_a) || is.null(ev_b)) return(invisible(NULL))
  pdf(file_path, width = 7.0, height = 4.6, family = "serif")
  op <- par(bty = "l", las = 1, mar = c(4.5, 5.0, 2.5, 1.0))
  on.exit({par(op); dev.off()}, add = TRUE)
  x_lo <- min(c(ev_a$event_time, ev_b$event_time))
  x_hi <- max(c(ev_a$event_time, ev_b$event_time))
  y_lo <- min(c(ev_a$ci_lo, ev_b$ci_lo))
  y_hi <- max(c(ev_a$ci_hi, ev_b$ci_hi))
  plot(NA, xlim = c(x_lo - 0.4, x_hi + 0.4), ylim = c(y_lo, y_hi),
       xlab = "Years Since the Contract", ylab = Y_LABEL, main = panel_label,
       xaxt = "n")
  axis(1, at = seq(x_lo, x_hi, by = 1))
  abline(h = 0, col = "gray60", lwd = 1)
  abline(v = -0.5, col = "gray60", lty = 2)
  off <- 0.10
  segments(ev_a$event_time - off, ev_a$ci_lo,
           ev_a$event_time - off, ev_a$ci_hi, col = "black", lwd = 1.5)
  points(ev_a$event_time - off, ev_a$estimate, pch = 16, col = "black")
  segments(ev_b$event_time + off, ev_b$ci_lo,
           ev_b$event_time + off, ev_b$ci_hi, col = "gray45", lwd = 1.5)
  points(ev_b$event_time + off, ev_b$estimate, pch = 17, col = "gray45")
  legend("bottomleft",
         legend = c(label_a, label_b),
         pch = c(16, 17),
         col = c("black", "gray45"), bty = "n", cex = 0.9)
}

main <- function() {
  dt <- read_doc()
  splits <- list(
    list(file = "document_mechanism_event_study_by_party_membership.pdf",
         label = "Party Membership",
         a_mask = quote(lawyer_ccp == 1),
         b_mask = quote(lawyer_ccp == 0),
         a_label = "Party member",
         b_label = "Non-member"),
    list(file = "document_mechanism_event_study_by_gender.pdf",
         label = "Gender",
         a_mask = quote(lawyer_gender == 1),
         b_mask = quote(lawyer_gender == 0),
         a_label = "Female lawyer",
         b_label = "Male lawyer"),
    list(file = "document_mechanism_event_study_by_seniority.pdf",
         label = "Seniority",
         a_mask = quote(lawyer_seniority_high == 1L & lawyer_practice_years_obs == 1L),
         b_mask = quote(lawyer_seniority_high == 0L & lawyer_practice_years_obs == 1L),
         a_label = "Senior (>= 7 yrs)",
         b_label = "Junior (< 7 yrs)"),
    list(file = "document_mechanism_event_study_by_education.pdf",
         label = "Education",
         a_mask = quote(lawyer_high_edu == 1L),
         b_mask = quote(lawyer_high_edu == 0L),
         a_label = "Master or above",
         b_label = "Below master")
  )
  for (sp in splits) {
    ev_a <- event_study(dt, sp$a_mask)
    ev_b <- event_study(dt, sp$b_mask)
    out <- file.path(figure_dir, sp$file)
    draw_compare(ev_a, ev_b, sp$a_label, sp$b_label, sp$label, out)
    cat("Wrote", out, "\n")
  }
}

main()
