#!/usr/bin/env Rscript
# Run corridor processing test by executing notebook chunks first

cat("=== Running Corridor Processing Test ===\n\n")

# Load packages
cat("Loading packages...\n")
suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(data.table)
  library(lubridate)
  library(knitr)
})

# Set working directory to script location
setwd("/Users/mmclean/dev/r/illinois-people-over-parking-act")

# Extract chunks from Rmd
cat("Extracting code from notebook...\n")
temp_r_file <- tempfile(fileext = ".R")
knitr::purl("sb2111-people-over-parking.Rmd", output = temp_r_file, quiet = TRUE, documentation = 0)

# Read the extracted R code
all_code <- readLines(temp_r_file)

# Find chunk boundaries (purl adds ## ---- chunk_name ---- comments)
chunk_starts <- grep("^## ----", all_code)
chunk_names <- gsub("^## ---- (.+) ----.*$", "\\1", all_code[chunk_starts])

# Find chunks we need to run (up through process_hubs_UPDATED)
chunks_needed <- c("setup", "packages", "load_existing_data", "download_gtfs", "process_hubs_UPDATED")

cat(sprintf("Found %d chunks total\n", length(chunk_names)))
cat("Chunks needed:", paste(chunks_needed, collapse = ", "), "\n\n")

# Execute each needed chunk
for (chunk_name in chunks_needed) {
  chunk_idx <- which(chunk_names == chunk_name)

  if (length(chunk_idx) == 0) {
    cat(sprintf("⚠️  Chunk '%s' not found, skipping\n", chunk_name))
    next
  }

  cat(sprintf("Running chunk: %s...\n", chunk_name))

  # Get chunk code
  start_line <- chunk_starts[chunk_idx] + 1
  end_line <- ifelse(chunk_idx < length(chunk_starts),
                     chunk_starts[chunk_idx + 1] - 1,
                     length(all_code))

  chunk_code <- all_code[start_line:end_line]

  # Execute chunk
  tryCatch({
    eval(parse(text = chunk_code), envir = .GlobalEnv)
    cat(sprintf("  ✓ Chunk '%s' completed\n\n", chunk_name))
  }, error = function(e) {
    cat(sprintf("  ✗ Error in chunk '%s': %s\n\n", chunk_name, e$message))
    stop(e)
  })
}

# Clean up temp file
unlink(temp_r_file)

cat("=== Notebook chunks loaded successfully ===\n\n")

# Now source and run the test script
cat("Running corridor processing test...\n\n")
source("test_corridor_processing.R")
