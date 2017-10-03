#' Cap Dataframe
#'
#' Capitalize the column names in a dataframe.
#' @param df A dataframe.
#' @keywords capitalize
#' @import Hmisc
#' @export
#' @examples
#' mtcars_2 <- mtcars %>% cap_df()


cap_df <- function(df) {
  names(df) <- names(df) %>% map(cap_it) %>% as_vector()
  return(df)
}
