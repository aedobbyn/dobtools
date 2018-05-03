#' Replace NA for a dataframe
#'
#' @description Replace all NA values in a dataframe
#' @param df Dataframe
#' @param replacement Defaults to NA_character_
#'
#' @export
#'
#' @examples
#'
#' starwars %>% replace_na_df()
#'

replace_na_df <- function(df, replacement = "") {
  out <- df %>%
    replace(is.na(.), "")

  return(out)
}
