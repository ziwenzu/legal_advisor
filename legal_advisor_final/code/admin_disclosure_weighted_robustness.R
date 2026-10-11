#!/usr/bin/env Rscript
args <- grep("^--file=", commandArgs(trailingOnly=FALSE), value=TRUE)
root <- normalizePath(file.path(dirname(sub("^--file=", "", args[1])), ".."))
message <- "NOT_APPLICABLE: German-tank disclosure weights require original court filing sequences. Repaired study observation IDs and replacement samples cannot identify those sequences. This specification is omitted; no fabricated docket numbers or weights are used."
cat(message, "\n")
writeLines(message, file.path(root, "output", "tables", "disclosure_weighting_not_applicable.txt"))
