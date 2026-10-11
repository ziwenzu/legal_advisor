# analysis_helpers.R
#
# Shared helpers for the supplementary (2026-09-30) analysis scripts.
# Sourced by each new script via source(file.path(root_dir, "code", "analysis_helpers.R")).
# Mirrors the conventions of the original scripts: locale guard, root-dir
# resolution, star / number formatting, threeparttable writers, and the
# fixest event-study extraction and base-graphics plotting used in the
# original event-study figures.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

local({
  utf8_locales <- c("en_US.UTF-8", "en_US.utf8", "C.UTF-8")
  for (loc in utf8_locales) {
    if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") {
      Sys.setenv(LANG = loc, LC_ALL = loc, LC_CTYPE = loc, LC_COLLATE = loc)
      break
    }
  }
})

setFixest_nthreads(4)

sb_dirs <- function(root_dir) {
  dirs <- list(
    data = file.path(root_dir, "data"),
    tables = file.path(root_dir, "output", "tables"),
    figures = file.path(root_dir, "output", "figures")
  )
  dir.create(dirs$tables, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirs$figures, recursive = TRUE, showWarnings = FALSE)
  dirs
}

stars <- function(p) {
  if (length(p) == 0 || is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}
fmt_num <- function(x, digits = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}
fmt_int <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
fmt_p <- function(p) {
  if (length(p) == 0 || is.na(p)) return("NA")
  if (p < 0.001) return("$<0.001$")
  sprintf("%.3f", p)
}
fmt_pct <- function(x, digits = 1) sprintf(paste0("%.", digits, "f\\%%"), 100 * x)

coef_cell <- function(est, se, p, digits = 3) {
  c(paste0(fmt_num(est, digits), stars(p)), paste0("(", fmt_num(se, digits), ")"))
}

# Pull one coefficient (estimate, se, p) plus N and R2 from a fixest model.
pull_term <- function(model, term_name) {
  ct <- as.data.table(coeftable(model), keep.rownames = "term")
  row <- ct[term == term_name]
  if (nrow(row) == 0) {
    return(list(estimate = NA_real_, se = NA_real_, p_value = NA_real_,
                n_obs = nobs(model), r2 = fitstat(model, "r2")[[1]]))
  }
  p_column <- intersect(c("Pr(>|t|)", "Pr(>|z|)"), names(row))
  p_value <- if (length(p_column)) row[[p_column[1L]]] else NA_real_
  list(estimate = row[["Estimate"]], se = row[["Std. Error"]], p_value = p_value,
       n_obs = nobs(model), r2 = fitstat(model, "r2")[[1]])
}

# Test H0: beta[term_a] - beta[term_b] = 0 using the model's clustered vcov.
diff_test <- function(model, term_a, term_b) {
  b <- coef(model); V <- vcov(model)
  d <- unname(b[term_a] - b[term_b])
  v <- V[term_a, term_a] + V[term_b, term_b] - 2 * V[term_a, term_b]
  se <- sqrt(v)
  df <- tryCatch(fixest::degrees_freedom(model, type = "t"), error = function(e) Inf)
  p <- 2 * pt(-abs(d / se), df = df)
  list(diff = d, se = se, p_value = p)
}

# Generic threeparttable writer.
# header_lines: character vector for the column-header rows (already terminated with \\).
# body_lines:   character vector of body rows.
write_tpt_table <- function(path, caption, label, col_spec, header_lines, body_lines,
                            note_text, size = "\\footnotesize", placement = "!htbp") {
  lines <- c(
    sprintf("\\begin{table}[%s]", placement),
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\centering",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\begin{threeparttable}",
    size,
    sprintf("\\begin{tabular}{%s}", col_spec),
    "\\toprule",
    header_lines,
    "\\midrule",
    body_lines,
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]",
    "\\footnotesize",
    sprintf("\\item \\textit{Note:} %s $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$.", note_text),
    "\\end{tablenotes}",
    "\\end{threeparttable}",
    "\\end{table}"
  )
  writeLines(lines, path)
  cat("Wrote", path, "\n")
}

# Event-study helpers (fixest i(event_time, treat, ref = ...)).
extract_event_dt <- function(model) {
  ip <- iplot(model, only.params = TRUE)
  dt <- as.data.table(ip$prms)
  dt[, .(event_time = as.numeric(estimate_names), estimate,
         ci_lo = fifelse(is_ref, 0, ci_low), ci_hi = fifelse(is_ref, 0, ci_high), is_ref)][order(event_time)]
}

joint_pretest <- function(model, treat_var, time_var, pre_periods) {
  term_names <- sprintf("%s::%s:%s", time_var, pre_periods, treat_var)
  present <- intersect(names(coef(model)), term_names)
  if (length(present) == 0) return(list(stat = NA_real_, p_value = NA_real_))
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", present)
  keep_pattern <- paste0("^(", paste(escaped, collapse = "|"), ")$")
  w <- suppressMessages(wald(model, keep = keep_pattern, print = FALSE))
  list(stat = unname(w$stat), p_value = unname(w$p))
}

plot_event_study <- function(event_dt, y_title, x_title, main_effect, main_se, pre_p, file_path, ref_time=-1) {
  sb_plot_event(event_dt,y_title,x_title,file_path,ref_time,main_effect,main_se,pre_p)
}

# Shared monochrome rendering, following the supplied Grid figure conventions.
sb_figure_theme <- function() {
  ggplot2::theme_classic(base_size=10, base_family="sans") +
    ggplot2::theme(axis.line=ggplot2::element_line(colour="#202020",linewidth=.35),
      axis.ticks=ggplot2::element_line(colour="#202020",linewidth=.35),
      axis.text=ggplot2::element_text(colour="#202020",size=10.2),
      axis.title=ggplot2::element_text(size=10.5),
      axis.title.x=ggplot2::element_text(margin=ggplot2::margin(t=6)),
      axis.title.y=ggplot2::element_text(margin=ggplot2::margin(r=6)),
      panel.grid=ggplot2::element_blank(), panel.border=ggplot2::element_blank(),
      plot.title=ggplot2::element_blank(), plot.subtitle=ggplot2::element_blank(),
      legend.position="none", plot.margin=ggplot2::margin(5,7,4,5))
}
sb_plot_event <- function(event_dt,y_title,x_title,file_path,ref_time=-1,main_effect=NA_real_,main_se=NA_real_,pre_p=NA_real_) {
  z <- as.data.frame(event_dt)[order(event_dt$event_time),]
  if (!"is_ref" %in% names(z)) z$is_ref <- FALSE
  metadata_file <- Sys.getenv("FIGURE_DIAGNOSTICS_FILE", unset=file.path(dirname(dirname(file_path)),"tables","figure_diagnostics.csv"))
  record <- data.table::data.table(figure=basename(file_path),pre_joint_p=pre_p,
    average_estimate=main_effect,average_se=main_se,average_t=main_effect/main_se,
    pre_pointwise_rejections=sum((z$ci_lo>0 | z$ci_hi<0) & z$event_time<0 & !z$is_ref,na.rm=TRUE),
    minimum_pre_estimate=min(z$estimate[z$event_time<0 & !z$is_ref]),
    maximum_pre_estimate=max(z$estimate[z$event_time<0 & !z$is_ref]))
  old <- if(file.exists(metadata_file)) data.table::fread(metadata_file) else NULL
  if(!is.null(old)) old <- old[figure!=basename(file_path)]
  data.table::fwrite(data.table::rbindlist(list(old,record),fill=TRUE)[order(figure)],metadata_file)
  p <- ggplot2::ggplot(z,ggplot2::aes(event_time,estimate)) +
    ggplot2::geom_hline(yintercept=0,colour="#707070",linewidth=.35) +
    ggplot2::geom_vline(xintercept=-.5,colour="#707070",linewidth=.35,linetype="dashed") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_lo,ymax=ci_hi),width=.13,linewidth=.45,colour="#404040") +
    ggplot2::geom_line(linewidth=.55,colour="#202020") +
    ggplot2::geom_point(data=z[!z$is_ref,],size=1.8,colour="#202020") +
    ggplot2::geom_point(data=z[z$is_ref,],size=1.9,shape=21,fill="white",colour="#202020") +
    ggplot2::scale_x_continuous(breaks=seq.int(min(z$event_time),max(z$event_time)),expand=ggplot2::expansion(add=.3)) +
    ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=.06)) +
    ggplot2::labs(x=x_title,y=paste(strwrap(y_title,width=28),collapse="\n")) + sb_figure_theme()
  wide <- grepl("lawyer_formal_position|lawyer_political_position|gov_lawyer_share|firm_side_switching",basename(file_path))
  ggplot2::ggsave(file_path,p,device=grDevices::pdf,width=if(wide) 5.5 else 3.6,height=if(wide) 3.3 else 2.8,
    family="Helvetica",useDingbats=FALSE,bg="white")
}
