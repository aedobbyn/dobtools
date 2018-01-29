#' Import Scripts
#'
#' @description Source in a directory of R scripts
#' @param path The directory path
#' @param pattern Extension pattern
#' @param quiet Should script be sourced quietly?
#' @import tidyverse
#' @import stringr
#' @export
#'
#' @examples
#'
#' # Import data
#' import_scripts(path = "./data", pattern = ".rda")
#'
#' # Import scripts
#' import_scripts(path = "./data-raw", pattern = "get_droids.R")
#' import_scripts(path = "foo")   # Should fail with a warning, not an error

import_scripts <- function(path, pattern = "*.R", quiet = TRUE) {
  files <- list.files(path, pattern, ignore.case = TRUE)
  file_paths <- str_c(path, "/", files)

  try_source <- possibly(source, otherwise = message("File not found :("),
    quiet = TRUE)

  for (file in file_paths) {
    try_source(file)
  }
}

