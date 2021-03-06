#' Contains a Number?
#'
#' @description If something can be coerced to numeric, is_num is TRUE; if it just contains digits, contains_num is TRUE.
#'
#' @param df A dataframe
#' @param col The unquoted name of the column of interest
#' @param add_contains_num Do you just want
#' @return Adds a new column for whether or not a value in col is or contains a number.
#' @export
#'
#' @examples
#'
#' mt_df <- mtcars %>% dplyr::mutate(car_name = rownames(.))
#' mt_df %>% find_nums(car_name)

find_nums <- function(df, col = word, add_contains_num = TRUE) {
  quo_col <- rlang::enquo(col)

  df <- df %>% dplyr::mutate(
    num = suppressWarnings(as.numeric(!!quo_col)),    # we could have as easily done this w a regex
    is_num = dplyr::case_when(
      !is.na(num) ~ TRUE,
      is.na(num) ~ FALSE
    )
  )

  if (add_contains_num == TRUE) {
    df <- df %>%
      dplyr::mutate(
        contains_num = dplyr::case_when(
          grepl("\\d+", !!quo_col) ~ TRUE,
          !(grepl("\\d+", !!quo_col)) ~ FALSE
        )
      )
  }

  df <- df %>% dplyr::select(-num)

  return(df)
}
