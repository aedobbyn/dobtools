#' Trello Point Summary
#'
#' Summarise billed and unbilled points in a dataframe.
#' @param df A datafame
#' @param lowercase "billed" and "points" by default. If FALSE, will use `Billed?` and Points
#' @keywords summarise
#' @export
#' @examples
#'
#' library(tidyverse)
#' lowercase_df <- as.data.frame(list(points = seq(1, 10, by = 2),
#'                             billed = c("Yes", "No", "No", "Yes", "Yes")))
#'
#' uppercase_df <- lowercase_df %>%
#'   rename(
#'     Points = points,
#'     `Billed?` = billed
#'   )
#'
#' summarise_points(lowercase_df)
#' summarise_points(uppercase_df, lowercase = FALSE)


summarise_points <- function(df, lowercase = TRUE, ...) {

  if(lowercase == TRUE) {
    point_summary <- df %>%
      summarise(
        Billed = sum(points[billed == "Yes"], na.rm = TRUE),
        Unbilled = sum(points[billed == "No"], na.rm = TRUE),
        Total = sum(points, na.rm = TRUE)
      )
  } else {
    point_summary <- df %>%
      summarise(
        Billed = sum(Points[`Billed?` == "Yes"], na.rm = TRUE),
        Unbilled = sum(Points[`Billed?` == "No"], na.rm = TRUE),
        Total = sum(Points, na.rm = TRUE)
      )
  }

  return(point_summary)
}



