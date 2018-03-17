#' Replace x
#'
#' @description Replace missing or NA values with a replacement
#' @param x Element to replace
#' @param replacement Defaults to NA_character_
#'
#' @export
#'
#' @examples
#'
#' vec <- c("a", NA, "b")
#' vec %>% purrr::map(replace_x, replacement = "foo")
#'

replace_x <- function(x, replacement = NA_character_) {
  if(is.null(x) || is.na(x)) {
    replacement
  } else {
    x
  }
}


