#' Cut Off
#'
#' Throw out elements that exceed a certain absolute value.
#' @param vec A numeric vector.
#' @param cutoff A numeric cutoff value.
#' @param e An element of a vector.
#' @keywords cluster
#' @import tidyverse
#' @export
#' @examples
#'
#' mtcars$mpg %>% cut_off_vecwise(20)
#' mtcars$carb %>% map(cut_off_elementwise)

cut_off_vecwise <- function(vec, cutoff) {
  vec <- vec[abs(vec) < cutoff]
  return(vec)
}

cut_off_elementwise <- function(e, cutoff = 1.96) {
  if (abs(e) > cutoff) {
    e <- NULL
  }
  return(e)
}
