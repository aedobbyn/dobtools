#' Beer Caps
#'
#' Capitalize the column names in a beer dataframe.
#' @param df Any dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' capitalize_this("ibu")

capitalize_df <- function(df, ...) {

  names_out <- vector()

  for (i in df_names) {
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
