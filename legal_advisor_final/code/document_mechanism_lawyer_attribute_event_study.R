#!/usr/bin/env Rscript
# document_mechanism_lawyer_attribute_event_study.R
#
# Descriptive event trajectories by observed annual lawyer attributes.
# Separate strata show Party Membership, Female, High-experience, and Master+.
# Different significance levels do not establish a between-group difference.
# Formal interactions using fixed pre-award attributes are reported separately
# in exploratory_heterogeneity.R. These annual strata can change composition.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# --- locale guard for portable UTF-8 reads (RStudio + Rscript safe) ---
local({
  utf8_locales <- c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")
  for (loc in utf8_locales) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG = loc, LC_ALL = loc, LC_CTYPE = loc, LC_COLLATE = loc)
      break
    }
  }
})


get_root_dir <- function() {
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(script_arg)) return(normalizePath(getwd()))
  script_path <- normalizePath(sub("^--file=", "", script_arg[1]))
  normalizePath(file.path(dirname(script_path), ".."))
}

root_dir <- get_root_dir()
source(file.path(root_dir,"code","analysis_helpers.R"))
input_file <- file.path(root_dir, "data", "document_level_winner_vs_loser.csv")
figure_dir <- file.path(root_dir, "output", "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
setFixest_nthreads(4)

OUTCOME <- "legal_reasoning_share"
Y_LABEL <- "Legal Reasoning Share"

read_doc <- function() {
  dt <- fread(input_file, encoding = "UTF-8")
  dt[, stack_year_fe := sprintf("%s__%s", award_id, year)]
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
    data = sub, cluster = ~ firm_id + award_id
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

draw_compare <- function(ev_a,ev_b,label_a,label_b,panel_label,file_path) {
  if (is.null(ev_a)||is.null(ev_b)) return(invisible(NULL))
  z <- rbind(data.table::copy(ev_a)[,group:=label_a],data.table::copy(ev_b)[,group:=label_b])
  p <- ggplot2::ggplot(z,ggplot2::aes(event_time,estimate,shape=group,linetype=group,group=group)) +
    ggplot2::geom_hline(yintercept=0,colour="#707070",linewidth=.35) +
    ggplot2::geom_vline(xintercept=-.5,colour="#707070",linetype="dashed",linewidth=.35) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_lo,ymax=ci_hi),width=.13,linewidth=.45,position=ggplot2::position_dodge(.2),colour="#404040") +
    ggplot2::geom_line(linewidth=.55,colour="#202020") +
    ggplot2::geom_point(size=1.8,colour="#202020",position=ggplot2::position_dodge(.2)) +
    ggplot2::scale_x_continuous(breaks=sort(unique(z$event_time))) +
    ggplot2::labs(x="Years since the contract",y=Y_LABEL,shape=NULL,linetype=NULL) +
    sb_figure_theme() + ggplot2::theme(legend.position="top")
  ggplot2::ggsave(file_path,p,device=grDevices::pdf,width=7.2,height=3.6,family="Helvetica",useDingbats=FALSE)
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
