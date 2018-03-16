#' Cap Dataframe
#'
#' Capitalize the column names in a dataframe.
#' @param df A dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' mtcars_2 <- mtcars %>% cap_df()
#'

cap_df <- function(df) {
  names(df) <- names(df) %>% purrr::map(dobtools::cap_it) %>% purrr::as_vector()
  return(df)
}
