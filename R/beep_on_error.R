#' Beep on error
#'
#' beepr::beep() only on error
#' @param inp Expression to be evaluated. Cannot be piped (yet)
#' @param error_beep Which beep? Defaults to "Wilhelm" (#9)
#' @keywords beep
#' @export
#' @examples
#'
#' \dontrun{
#' beep_on_error(log("foo"))   # Beeps on error
#' beep_on_error(log(3))      # No error, no beep
#' }

beep_on_error <- function(inp, error_beep = 9) {
  q_inp <- substitute(inp)

  msg <- paste0("An error occurred in ", deparse(q_inp))
  e <- simpleError(msg)

  tryCatch(inp, error = function(e) {
    message(paste0(msg, ": ", e$message))
    beepr::beep(error_beep)
  })
}
