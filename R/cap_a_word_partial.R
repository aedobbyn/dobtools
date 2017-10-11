#' Cap a Word, Partial
#'
#' Find part of a word and make it full caps.
#' @param phrase A single element or phrase containing one or more of the elements in `to_cap`
#' @param to_cap A vector of words that, if contained in `phrase`, will be fully capitalized.
#' @keywords cap
#' @import tidyverse
#' @import assertthat
#' @import stringr
#' @export
#' @examples
#'
#' cap_a_word_partial("this_id")
#' cap_a_word_partial("this Id")
#' cap_a_word_partial("thisid")
#'
#' words_to_cap <- c("Petal", "Width")
#' names(iris) %>% map(cap_a_word_partial, words_to_cap) %>% as_vector()



cap_a_word_partial <- function(phrase, to_cap = c("id", "Id")) {

  assertthat::assert_that(is_character(phrase) && is_character(to_cap),
                          msg = "The phrase and words to find must be characters.")

  # If any of the words to cap appear in the vector
  if(any(str_detect(phrase, to_cap))) {
    # Find which of our keywords word we need to capitalize
    this_uncapped <- to_cap[which(stringr::str_detect(phrase, to_cap))]

    # Loop through the substrings that need to be capitalized in our phrase and capitalize them
    for (i in seq_along(this_uncapped)) {
      phrase <- gsub(this_uncapped[i], toupper(this_uncapped[i]), phrase)
    }
  }
  return(phrase)
}