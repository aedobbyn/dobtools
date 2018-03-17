#' NA to Zeros
#'
#' An alternative to na_to_zero_df(). Use purrr::map() instead.
#' @param val A value.
#' @keywords zero
#' @export
#' @examples
#'
#' sample_df <- list(a = c(1:2, NA),
#'                   b = letters[1:3],
#'                   c = c(NA, "foo", "bar")) %>%
#'   tibble::as_tibble()
#' sample_df %>% purrr::map_df(na_to_zero)
#'
#' cols_to_zero <- c("a", "b")
#' sample_df %>% purrr::map_at(na_to_zero, .at = cols_to_zero) %>% tibble::as_tibble()


na_to_zero <- function(val) {
  val[which(is.na(val))] <- 0
  return(val)
}


