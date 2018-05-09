#' Remove empty string elements
#'
#' Take a vector and remove empty string elements. Useful after string splitting and being left with lots of empty string vectors.
#' @param vec A vector that may contain empty strings
#' @keywords string
#'
#' @export
#'
#' @examples
#'
#' remove_empty_strings(c("a", "", "c"))

remove_empty_strings <- function(vec) {
  out <- vec[-which(vec == "")]
  return(out)
}
