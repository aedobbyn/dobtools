#' Import Scripts
#'
#' @description Source in a directory of R scripts
#' @param path The directory path
#' @param pattern Extension pattern
#' @param recursive Grab files in subdirectories?
#' @param quiet Should script be sourced quietly?
#' @export
#'
#' @examples
#'
#' # Import data
#' import_scripts(path = "./data", pattern = ".rda")
#'
#' # Import scripts
#' import_scripts(path = "./data-raw", pattern = "get_droids.R", verbose = TRUE)
#' import_scripts(path = "foo")   # Should fail with a warning, not an error

import_scripts <- function(path, pattern = "*.R", recursive = FALSE, verbose = FALSE) {
  files <- list.files(path, pattern, recursive, ignore.case = TRUE)
  file_paths <- stringr::str_c(path, "/", files)

  for (file in file_paths) {
    if (verbose == TRUE) {
      message(glue::glue("Sourcing {file}"))
    }
    source(file)
  }
}

