#' Test It
#'
#' @description Return whether an individual test passed or not
#' @param f The file
#' @param beep Should we beep when done?
#' @param beep_num If so, what beep number should we use?
#' @param verbose Should messages and warnings be suppressed
#'
#' @export
#'

test_it <- function(f, beep = TRUE, beep_num = 1, verbose = FALSE) {
  if (verbose == FALSE) {
    result <- suppressMessages(suppressWarnings(try(source(f), silent = TRUE)))
  } else {
    result <- try(source(f), silent = TRUE)
  }

  if (inherits(result, "try-error")) {
    message(paste0(" --- ", f, " FAILED --- "))
    if (beep) beepr::beep(9)
    message(result)
  } else {
    message(paste0(" --- ", f, " PASSED --- "))
    if (beep) beepr::beep(beep_num)
  }
}
