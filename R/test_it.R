#' Test It
#'
#' @description Return whether an individual test passed or not
#' @param f The file
#' @param verbose Should messages and warnings be suppressed
#'
#' @export
#'



test_it <- function(f, verbose = FALSE) {
  if (verbose == FALSE) {
    result <- suppressMessages(suppressWarnings(try(source(f), silent = TRUE)))
  } else {
    result <- try(source(f), silent = TRUE)
  }

  if (inherits(result, "try-error")) {
    message(paste0(" --- ", f, " FAILED --- "))
  } else {
    message(paste0(" --- ", f, " PASSED --- "))
  }
}
