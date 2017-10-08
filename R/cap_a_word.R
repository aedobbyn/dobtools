#' Cap a Word
#'
#' Find a word within a word and capitalize it.
#' @param phrase A single element or phrase containing one or more of the elements in `to_cap`
#' @param to_cap A vector of words that, if contained in `phrase`, will be fully capitalized.
#' @keywords cap
#' @import tidyverse
#' @import stringr
#' @export
#' @examples
#'
#' cap_a_word("this_id")
#' cap_a_word("this_id", collapse = "_")
#'
#' words_to_cap <- c("Petal", "Width")
#' names(iris) %>% map(cap_a_word, words_to_cap) %>% as_vector()

cap_a_word <- function(phrase, to_cap = c("id", "Id"),
                       collapse = " ") {

  assertr::assert_that(is_character(phrase) && is_character(to_cap))

  # If any of the words to cap appear in the phrasetor
  if(any(str_detect(phrase, to_cap))) {
    # Find which of our keywords word we need to capitalize
    this_uncapped <- to_cap[which(stringr::str_detect(phrase, to_cap))]

    # Split the phrasetor into individual words
    if (stringr::str_detect(phrase, " ")) {
      split_phrase <- str_split(phrase, " ") %>% as_vector()
    } else if (str_detect(phrase, "_")) {
      split_phrase <- str_split(phrase, "_") %>% as_vector()
    } else if (str_detect(phrase, " ") & str_detect(phrase, "_")) {
      split_phrase <- str_split(phrase, " ") %>% str_split(phrase, "_") %>% as_vector()
    } else {
      split_phrase <- str_split(phrase, "\\.") %>% as_vector()
    }

    # Find the index of the word in the phrasetor that needs to be capitalized
    to_cap_ind <- which(split_phrase == this_uncapped)

    # Capitalize just that index
    split_phrase[to_cap_ind] <- split_phrase[to_cap_ind] %>% toupper()

    # Put it all back together
    phrase <- str_c(split_phrase, collapse = collapse)
  } else {
    phrase <- phrase
  }
  return(phrase)
}
