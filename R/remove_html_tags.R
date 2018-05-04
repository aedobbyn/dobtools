#' Remove HTML tags
#'
#' @description Take a string and remove HTML tags from hit. Does not remove \code{&amp}s or newlines
#'
#' @param h_str A string possibly containing HTML tags
#' @return Cleaned string
#' @export
#'
#' @examples
#'
#' to_clean <- "<li> some <b>bullet</b> </li>"
#' remove_html_tags(to_clean)

remove_html_tags <- function(h_str) {
  return(gsub("<.*?>", "", h_str))
}

