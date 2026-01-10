#' Compare output files to expected output
#'
#' This script compares all files in data/output_files/ to data/expected_output/ by filename.
#' It reports which files match and which differ (using md5 hashes).
#'
#' Usage: source('inst/examples/compare_outputs.R')

output_dir <- "~/output_files"
expected_dir <- file.path('inst', 'examples', 'data', 'expected_output')

output_files <- list.files(output_dir, full.names = TRUE)
expected_files <- list.files(expected_dir, full.names = TRUE)

# Only compare files with the same name in both dirs
common_files <- intersect(basename(output_files), basename(expected_files))

if (length(common_files) == 0) {
  cat('No matching files to compare.\n')
} else {
  for (fname in common_files) {
    out_path <- file.path(output_dir, fname)
    exp_path <- file.path(expected_dir, fname)
    out_lines <- readLines(out_path, warn = FALSE)
    exp_lines <- readLines(exp_path, warn = FALSE)
    n_out <- length(out_lines)
    n_exp <- length(exp_lines)
    min_len <- min(n_out, n_exp)
    diff_found <- FALSE
    for (i in seq_len(min_len)) {
      if (!identical(out_lines[i], exp_lines[i])) {
        cat(sprintf('DIFFER: %s\n', fname))
        cat(sprintf('  Output lines: %d, Expected lines: %d\n', n_out, n_exp))
        cat(sprintf('  First difference at line %d:\n', i))
        cat(sprintf('    Output   : %s\n', out_lines[i]))
        cat(sprintf('    Expected : %s\n', exp_lines[i]))
        diff_found <- TRUE
        break
      }
    }
    if (!diff_found) {
      if (n_out == n_exp) {
        cat(sprintf('MATCH: %s (lines: %d)\n', fname, n_out))
      } else {
        cat(sprintf('DIFFER: %s\n', fname))
        cat(sprintf('  Output lines: %d, Expected lines: %d\n', n_out, n_exp))
        cat('  Files differ in length only.\n')
      }
    }
  }

  # Optionally, report files missing in either dir
  missing_in_output <- setdiff(basename(expected_files), basename(output_files))
  missing_in_expected <- setdiff(basename(output_files), basename(expected_files))
  if (length(missing_in_output) > 0) {
    cat('Missing in output_files:', paste(missing_in_output, collapse=', '), '\n')
  }
  if (length(missing_in_expected) > 0) {
    cat('Missing in expected_output:', paste(missing_in_expected, collapse=', '), '\n')
  }

}
