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

# Helper: try reading as data.frame, else NULL
try_read_table <- function(path) {
  tryCatch({
    read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) NULL)
}

# Compare two tabular files for identical, diff_order, diff_content, diff_shape
compare_tabular_files <- function(file1, file2) {
  df1 <- try_read_table(file1)
  df2 <- try_read_table(file2)
  if (is.null(df1) || is.null(df2)) return(NA)
  if (!all(dim(df1) == dim(df2))) return("diff_shape")
  if (identical(df1, df2)) return("identical")
  # Check for same rows and counts, but different order
  if (all(table(apply(df1, 1, paste, collapse = "\r")) == table(apply(df2, 1, paste, collapse = "\r")))) {
    return("diff_order")
  }
  return("diff_content")
}

if (length(common_files) == 0) {
  cat('No matching files to compare.\n')
} else {
  for (fname in common_files) {
    out_path <- file.path(output_dir, fname)
    exp_path <- file.path(expected_dir, fname)
    cmp <- compare_tabular_files(out_path, exp_path)
    if (!is.na(cmp)) {
      if (cmp == "identical") {
        cat(sprintf('MATCH: %s (tabular, identical)\n', fname))
      } else if (cmp == "diff_order") {
        cat(sprintf('DIFF_ORDER: %s (tabular, same rows, different order)\n', fname))
      } else if (cmp == "diff_shape") {
        cat(sprintf('DIFFER: %s\n  Reason: tabular files have different shape\n', fname))
      } else if (cmp == "diff_content") {
        cat(sprintf('DIFFER: %s\n  Reason: tabular files differ in content\n', fname))
      }
      next
    }
    # Fallback: original line-by-line logic for non-tabular files
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
