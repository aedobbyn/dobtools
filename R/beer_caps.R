#' Beer Caps
#'
#' Capitalize the column names in a beer dataframe.
#' @param df Any dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' capitalize_this("ibu")

capitalize_this <- function(df, ...) {
  out <- vector()
  for (i in names(df)) {
    if (i == "abv") {
      i <- "ABV"
    } else if (i == "ibu") {
      i <- "IBU"
    } else if (i == "srm") {
      i <- "SRM"
    } else if (grepl(pattern = "_", x = i) == TRUE) {
      i <- simple_cap(gsub(x = i, pattern = "_", replacement = " "))
    } else {
      i <- capitalize(i)
    }
    out <- c(out, i)
  }
  names(df) <- out
  df
}
