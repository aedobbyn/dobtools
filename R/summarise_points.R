#' Simple Capitalization
#'
#' Summarise billed and unbilled points in a dataframe.
#' @param df A datafame
#' @param condition "billed" by default. For some dataframes this may
#' neet to be changed to `Billed?`
#' @keywords summarise
#' @export
#' @examples
#' simple_cap()


summarise_points <- function(df, condition = "billed", ...) {
  point_summary <- this_proj_df %>%
    summarise(
      Billed = sum(points[condition == "Yes"], na.rm = TRUE),
      Unbilled = sum(points[condition == "No"], na.rm = TRUE),
      Total = sum(points, na.rm = TRUE)
    )

  return(point_summary)
}
