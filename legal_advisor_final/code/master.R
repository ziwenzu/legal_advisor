#!/usr/bin/env Rscript
# Run analysis scripts in separate R sessions using included data.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")) else normalizePath(getwd())
setwd(root)
options_args <- commandArgs(trailingOnly = TRUE)
inventory <- read.csv(file.path("code", "analysis_scripts.csv"), stringsAsFactors = FALSE)
scripts <- c(inventory$script, "build_manuscript_exhibits.R")
if ("--tables-only" %in% options_args) scripts <- "build_manuscript_exhibits.R"
only_pos <- match("--only", options_args)
if (!is.na(only_pos)) {
  selected <- options_args[seq.int(only_pos + 1L, length(options_args))]
  if (any(!selected %in% scripts)) stop("Unknown script requested: ", paste(setdiff(selected, scripts), collapse = ", "))
  scripts <- scripts[scripts %in% selected]
}
inputs <- list.files("data", pattern = "[.]csv$", full.names = TRUE)
if (!length(inputs)) stop("No input data found.")
if (any(!file.exists(file.path("code", scripts)))) stop("An analysis script is missing.")
dependencies <- c("data.table", "fixest", "did", "fwildclusterboot", "dqrng", "fect", "ggplot2", "TwoWayFEWeights", "bacondecomp")
missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install the required R packages: ", paste(missing, collapse = ", "))
if ("--check" %in% options_args) {
  cat("R dependencies, input files and script inventory are available.\n")
  quit(status = 0L)
}
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
before <- tools::md5sum(inputs)
Sys.setenv(OMP_NUM_THREADS = "4", OPENBLAS_NUM_THREADS = "4")
summary <- data.frame(script = character(), status = character(), exit_code = integer(), seconds = numeric(), log = character())
if (("--tables-only" %in% options_args || !is.na(only_pos)) && file.exists("output/run_summary.csv")) {
  summary <- read.csv("output/run_summary.csv", stringsAsFactors = FALSE)
  summary <- summary[!summary$script %in% scripts, , drop = FALSE]
}
for (i in seq_along(scripts)) {
  name <- scripts[i]
  start <- proc.time()[["elapsed"]]
  temporary <- file.path("output", "logs", paste0(name, ".running.log"))
  writeLines(character(), temporary)
  status_code <- system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(file.path("code", name))), stdout = temporary, stderr = temporary, timeout = 600L)
  lines <- readLines(temporary, encoding = "UTF-8", warn = FALSE)
  unlink(temporary)
  lines <- gsub(root, ".", lines, fixed = TRUE)
  log_path <- file.path("output", "logs", paste0(name, ".log"))
  writeLines(c(paste("Script:", name), paste("UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE)), lines), log_path, useBytes = TRUE)
  status <- if (status_code == 0L) "PASS" else "FAIL"
  if (status_code == 0L && any(grepl("NOT_APPLICABLE:", lines, fixed = TRUE))) status <- "NOT_APPLICABLE"
  summary <- rbind(summary, data.frame(script = name, status = status, exit_code = status_code,
    seconds = round(proc.time()[["elapsed"]] - start, 2L), log = log_path))
  write.csv(summary, "output/run_summary.csv", row.names = FALSE)
  cat(sprintf("%d/%d %s %s\n", i, length(scripts), status, name))
  flush.console()
  if (status_code != 0L) stop("Script failed; see ", log_path)
}
after <- tools::md5sum(inputs)
if (!identical(before, after)) stop("An input data file changed during analysis.")
write.csv(data.frame(file = basename(inputs), md5 = unname(after)), "output/input_fingerprints.csv", row.names = FALSE)
environment <- c(capture.output(sessionInfo()), "", "Required packages:",
  vapply(dependencies, function(name) paste(name, as.character(packageVersion(name))), character(1)))
writeLines(environment, "output/session_info.txt", useBytes = TRUE)
cat("All requested scripts completed; input data files are unchanged.\n")
