
#' Is this word plural?
#'
#' Return either the correct word to say for this string ("them" or "it") or a boolean value
#' @param word A string
#' @param return_bool Should you get the word to say back or a boolean?
#' @keywords plural
#' @import tidyverse
#' @export
#' @examples
#' is_plural("CATS")
#' is_plural("dog", return_bool = TRUE)

is_plural <- function(word, return_bool = FALSE) {

  if(substr(word, nchar(word), nchar(word)) %>% tolower() == "s") {
    is_plural_bool <- TRUE
    word_to_say <- "them"
  } else {
    is_plural_bool <- FALSE
    word_to_say <- "it"
  }

  if(return_bool == TRUE) {
    return(is_plural_bool)
  } else {
    return(word_to_say)
  }

}
