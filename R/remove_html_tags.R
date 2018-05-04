#' Remove HTML tags
#'
#' @description Take a string and remove HTML tags from hit.
#'
#' @param h_str A string possibly containing HTML tags
#' @return Cleaned string
#' @export
#'
#' @examples
#'
#' to_clean <- "<li> some \n <b>bullet</b> </li>"
#' remove_html_tags(to_clean)

remove_html_tags <- function(h_str) {
  out <- stringr::str_replace_all(h_str, "<.*?>", "") %>%
    stringr::str_replace_all("\\n", "") %>%
    stringr::str_replace_all("\\r", "") %>%
    stringr::str_replace_all("\\t", "") %>%
    stringr::str_replace_all("&amp", "&") %>%
    stringr::str_replace_all("&nbsp", " ")
  return(out)
}

