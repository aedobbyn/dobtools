
#' Simple Capitalization
#'
#' Capitalize a vector of words.
#' @param x Any vector
#' @keywords capitalize
#' @export
#' @examples
#' simple_cap()

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


#' Beer Caps
#'
#' Capitalize beer words.
#' @param df Any dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' capitalize_this()

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
