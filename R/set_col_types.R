
#' Set Column Types
#'
#' Set column types in a dataframe using regexes. Separate full or partial column names
#' to look for by |.
#' @param df A datafame
#' @param fac_regex A regex of column names to be turned into factors.
#' @param num_regex A regex of column names to be turned numeric.
#' @param date_regex A regex of column names to be turned into dates.
#' @param datetm_regex A regex of column names to be turned into datetimes.
#' @param char_regex A regex of column names to be turned into character.
#' @param ... Other arguments
#' @keywords column types
#' @export
#' @examples
#' my_df <- as.data.frame(list(some = 1:5, column = Sys.Date() + 1:5,
#'                             types = letters[5:9],
#'                             to = c("good", "bad", "ugly", "other", "NA"),
#'                             change = rnorm(10:14)))
#'
#' factor_cols <- c("change")
#' char_cols <- c("some|to")
#'
#' my_new_df <- set_col_types(df = my_df, fac_regex = factor_cols,
#'                            char_regex = char_cols)
#' str(my_df)
#' str(my_new_df)


set_col_types <- function(df, fac_regex = "",
                          num_regex = "",
                          date_regex = "",
                          datetm_regex = "",
                          char_regex = "", ...) {

  for(col_name in names(df)) {
      # Factor
    if (fac_regex != "" & grepl(fac_regex, col_name) == TRUE) {
      df[[col_name]] <- factor(df[[col_name]])
      # Numeric -- take out commas first
    } else if (num_regex != "" & grepl(num_regex, col_name) == TRUE) {
      df[[col_name]] <- df[[col_name]] %>% as.character() %>% stringr::str_replace_all(., ",", "") %>% as.numeric()
      # Datetime
    } else if (datetm_regex != "" & grepl(datetm_regex, col_name) == TRUE) {
      df[[col_name]] <- lubridate::as_datetime(df[[col_name]])
      # Date
    } else if (date_regex != "" & grepl(date_regex, col_name) == TRUE) {
      df[[col_name]] <- lubridate::as_date(df[[col_name]])
    # Character
    } else if (char_regex != "" & grepl(char_regex, col_name) == TRUE) {
      df[[col_name]] <- as.character(df[[col_name]])
    }
    df <- tibble::as_tibble(df)
  }
  return(df)
}



