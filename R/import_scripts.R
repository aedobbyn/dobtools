#' Import Scripts
#'
#' @description Source in a directory of R scripts
#' @param path The directory path
#' @param pattern Extension pattern
#' @import tidyverse
#' @import stringr
#' @export
#'
#' @examples
#'
#' import_scripts(path = "./data", pattern = ".rda")
#' import_scripts(path = "./R", pattern = "get_droids.R")
#' droids   # droids defined in data/get_droids.R
#' import_scripts(path = "foo")   # Should fail with a warning, not an error

import_scripts <- function(path, pattern = "*.R") {
  files <- list.files(path, pattern, ignore.case = TRUE)
  file_paths <- str_c(path, "/", files)

  try_source <- possibly(source, otherwise = "bar",
    quiet = TRUE)

  for (file in file_paths) {
    try_source(file)
  }
}

