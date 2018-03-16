#' Group Numeric
#'
#' Mutate a dataframe by assigning each row a group based on its value in a certain column.
#' @param df A dataframe
#' @param col A numeric or date column. If the value here is outside the vector range, the `group` colum
#' will be set to NA for that row.
#' @param vec A vector of cuts.
#' @param add_first_last Should the first and last element of the vector be used as the lower and upper bounds,
#' respectively, for the 1st and nth element? Or should those
#' In other words, should the 1st element of the vector be considered the lower (add_first_last = FALSE)
#'  or upper (add_first_last = TRUE) bound of the first group?
#' @keywords group
#' @export
#' @examples
#'
#' mpg_cuts <- c(15, 20, 25, 30)
#' group_numeric(mtcars, "mpg", mpg_cuts, add_first_last = TRUE)
#'
#' mtcars$some_dates <- (rnorm(n = 32) * 5) %>% lubridate::as_date()
#' date_cuts <- c("1969-11-01", "1969-12-29", "1970-01-01", "1970-01-04", "1970-02-01")
#' group_numeric(mtcars, "some_dates", date_cuts, add_first_last = FALSE)


group_numeric <- function(df, col, vec, add_first_last = FALSE) {
  assertthat::assert_that(is.numeric(df[[col]]) | lubridate::is.Date(df[[col]]),
                          msg = "Column must be of type numeric or date.")

  is_date <- lubridate::is.Date(df[[col]])

  if(is_date == TRUE) {
    vec <- vec %>% lubridate::as_date()
    if(add_first_last == TRUE) { # If we need to add a past and future date, do so
      vec <- c(lubridate::as_date("1970-01-01"), vec, lubridate::as_date(Sys.Date() + 1000))
    }
  } else if (is_date == FALSE & add_first_last == TRUE) {
    vec <- c(vec[1] - Inf, vec, vec[length(vec)] + Inf)
  }

  # Initialize sprint_number column
  df$group <- NA

  for (d in 1:nrow(df)) {
    for (i in 1:(length(vec) - 1)) {
      if(df[[col]][d] >= vec[i] & df[[col]][d] < vec[i + 1]) {
        df$group[d] <- i
      }
    }
  }
  return(df)
}



