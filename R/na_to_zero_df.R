

#' NA to Zeros
#'
#' Specify a dataframe and a vector of column names. If there are NAs in those columns,
#' convert them to 0s and return a dataframe. This is slower than using na_to_zero.R with purrr:: map().
#' @param df A dataframe.
#' @param cols A vector of quoted column names.
#' @keywords zero
#' @export
#' @examples
#' library(tidyverse)
#' sample_df <- list(a = 1:3, b = letters[1:3], c = c(NA, "foo", "bar")) %>% as_tibble()
#' cols_to_zero <- c("b", "c")
#' na_to_zero(sample_df, cols_to_zero)


na_to_zero <- function(df, cols, ...) {
  for (col in cols) {
    df[[col]][which(is.na(df[[col]]))] <- 0
  }
  df
}


