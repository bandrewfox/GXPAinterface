#' Copy example Rmd template(s) to a local folder
#'
#' Usage: `use_example_rmd(to = ".", overwrite = FALSE)`
#'
#' @param to Destination directory (default: current directory)
#' @param overwrite Whether to overwrite existing files
#' @export
use_example_rmd <- function(to = ".", overwrite = FALSE) {
  src <- system.file("examples", package = utils::packageName())
  if (!nzchar(src)) {
    stop("No inst/examples found in this package.", call. = FALSE)
  }

  dest <- file.path(to, "examples")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  ok <- file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE),
                  dest, recursive = TRUE, overwrite = overwrite)
  if (!all(ok)) warning("Some files were not copied.", call. = FALSE)

  message("Copied examples to: ", normalizePath(dest, winslash = "/"))
  invisible(dest)
}
