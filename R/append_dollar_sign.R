#' Append Dollar Sign
#'
#' Tack on a dollar sign to the end of values in certain columns.
#' @param vec A vector
#' @param regex A regex of elements in that vector to append the dollar sign to.
#' @keywords dollar
#' @import tidyverse
#' @export
#' @examples
#' append_dollar_sign(names(mtcars), "hp|dr|car")


append_dollar_sign <- function(vec, regex) {
  out <- NULL
  for (i in vec) {
    if (grepl(regex, i)) {
      i <- i %>% stringr::str_c(" ($)")
    }
    out <- c(out, i)
  }
  out
}
