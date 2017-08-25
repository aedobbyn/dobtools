#' Trello Point Summary
#'
#' Summarise billed and unbilled points in a dataframe.
#' @param df A datafame
#' @param condition "billed" by default. For some dataframes this may
#' neet to be changed to `Billed?`
#' @keywords summarise
#' @export
#' @examples
#' summarise_points()

library(tidyverse)
lowercase_df <- as.data.frame(list(points = seq(1, 10, by = 2),
                            billed = c("Yes", "No", "No", "Yes", "Yes")))


uppercase_df <- lowercase_df %>%
  rename(
    Points = points,
    `Billed?` = billed
  )



summarise_points <- function(df, lowercase = TRUE, ...) {
  browser()

  if(lowercase == TRUE) {
    condition <- "billed"
    # pts <- "points"
  } else {
    condition <- "Billed?"
    # pts <- `Points`
  }

  point_summary <- df %>%
    summarise(
      Billed = sum(Points[condition == "Yes"], na.rm = TRUE),
      Unbilled = sum(Points[condition == "No"], na.rm = TRUE),
      Total = sum(Points, na.rm = TRUE)
    )

  return(point_summary)
}


summarise_points(lowercase_df)

summarise_points(uppercase_df, lowercase = FALSE)


