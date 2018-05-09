#' Find last non-NA value
#'
#' Take vector and start at the end, to find the last non-NA element
#' to look for by |.
#' @param vec A datafame
#' @keywords NA
#' @export
#' @examples
#'
#' c(1, 2, NA_integer_, 4, 5, NA_integer_, NA_integer_) %>% find_last_non_na_value()
#' sample(c(letters, rep(NA_character_, 3))) %>% find_last_non_na_value()


find_last_non_na_value <- function(vec) {
  i <- length(vec)
  if (!is.na(vec[i])) {
    return(vec[i])
  } else {
    i <- i - 1
    find_last_non_na_value(vec[1:i])
  }
}
