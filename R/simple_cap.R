
#' Simple Capitalization
#'
#' Capitalize the first letter in every word given a vector of words.
#' @param e A character vector
#' @keywords capitalize
#' @export
#' @examples
#' simple_cap("goodbye lowercase")

simple_cap <- function(e) {
  s <- strsplit(e, " ")[[1]]

  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


