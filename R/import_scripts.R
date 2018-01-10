#' Import Scripts
#'
#' @description Source in a directory of R scripts
#' @param path The directory path
#' @param pattern Extension pattern
#' @import tidyverse
#' @import stringr
#' @return
#' @export
#'
#' @examples
#'
#' import_scripts(path = "./data")
#' droids   # droids defined in data/get_droids.R in this package
#' import_scripts(path = "foo")   # Should fail with a warning, not an error

import_scripts <- function(path, pattern = "*.R") {
  files <- list.files(path, pattern, ignore.case = TRUE)
  file_paths <- str_c(path, "/", files)
  try_source <- possibly(source, otherwise = message("Can't find this file or path."))

  for (file in file_paths) {
    try_source(file)
  }
}

