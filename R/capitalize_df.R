#' Capitalize Dataframe Columns
#'
#' Capitalize the column names in a dataframe.
#' Somewhat deprecated in favor of cap_it()
#' @param df Any dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' capitalize_df(iris)

capitalize_df <- function(df, ...) {

  names_out <- vector()

  for (i in names(df)) {
    if (grepl(pattern = "_", x = i) == TRUE) {
      i <- simple_cap(gsub(x = i, pattern = "_", replacement = " "))
    } else if (grepl(pattern = ".", x = i) == TRUE) {
      i <- simple_cap(gsub(x = i, pattern = "\\.", replacement = " "))
    } else {
      i <- Hmisc::capitalize(i)
    }
    names_out <- c(names_out, i)
  }

  names(df) <- names_out

  return(df)
}
