#' Cut Off Vecwise
#'
#' Throw out elements that exceed a certain absolute value.
#' @param vec A numeric vector.
#' @param cutoff A numeric cutoff value.
#' @keywords cutoff
#' @export
#' @examples
#'
#' mtcars$mpg %>% cut_off_vecwise(20)

cut_off_vecwise <- function(vec, cutoff) {
  vec <- vec[abs(vec) < cutoff]
  return(vec)
}


#' Cut Off Elementwise
#'
#' Throw out elements that exceed a certain absolute value.
#' @param cutoff A numeric cutoff value.
#' @param e An element of a vector.
#' @keywords cutoff
#' @export
#' @examples
#'
#' mtcars$carb[3:15] %>% purrr::map(cut_off_elementwise, cutoff = 3)

cut_off_elementwise <- function(e, cutoff = 1.96) {
  if (abs(e) > cutoff) {
    e <- NULL
  }
  e
}
