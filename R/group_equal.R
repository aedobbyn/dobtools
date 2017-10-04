#' Group Equal
#'
#' Split a dataframe column into equal-sized groups.
#' @param df A dataframe
#' @param col A numeric column. If the value here is outside the vector range, the `group` colum
#' will be set to NA for that row.
#' @param n_groups Number of groups to end.
#' @keywords group
#' @import tidyverse
#' lubridate
#' @export
#' @examples
#'
#' groups <- group_equal(mtcars, "mpg", n_groups = 5)
#' group_numeric(mtcars, "mpg", groups, add_first_last = TRUE)


group_equal <- function(df, col, n_groups = 5, add_first_last = TRUE) {
  df <- df %>% drop_na(!!col) %>%
    ungroup() %>%
    arrange_(.dots = col) %>%
    mutate(row_num = row_number())

  n_total <- nrow(df)
  rnge <- range(df[[col]])
  n_per_group <- floor(n_total/n_groups)

  vec <- NULL

  for (g in 1:n_groups) {
    this_upper_bound <- df[which(df[["row_num"]]
                                 == (n_per_group * g)), ][[col]]
    vec <- c(vec, this_upper_bound)
  }

  if (add_first_last == TRUE) {
    vec <- c(min(df[[col]]), vec, max(df[[col]]))
  }

  return(vec)
}

