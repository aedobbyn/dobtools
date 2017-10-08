#' Cap a Word
#'
#' Find a word within a word and capitalize it.
#' @param phrase A single element or phrase containing one or more of the elements in `to_cap`
#' @param to_cap A vector of words that, if contained in `phrase`, will be fully capitalized.
#' @param collapse What separator should the whole thing be collapsed down with?
#' @param split_on What separator should the word be split on? Mostly important for a separator of
#' "".
#' @keywords cap
#' @import tidyverse
#' @import stringr
#' @export
#' @examples
#'
#' cap_a_word("this_id")
#' cap_a_word("this Id", split_on = " ")
#' cap_a_word("this_id", collapse = "_")
#' cap_a_word("this_id", collapse = "")
#' cap_a_word("thisid", split_on = "")
#' cap_a_word("this.id", split_on = ".")
#'
#' words_to_cap <- c("Petal", "Width")
#' names(iris) %>% map(cap_a_word, words_to_cap) %>% as_vector()

cap_a_word <- function(phrase, to_cap = c("id", "Id"),
                       collapse = " ", split_on = "_") {

  assertthat::assert_that(is_character(phrase) && is_character(to_cap),
                          msg = "The phrase and words to find must be characters.")


  # If any of the words to cap appear in the phrasetor
  if(any(str_detect(phrase, to_cap))) {
    # Find which of our keywords word we need to capitalize
    this_uncapped <- to_cap[which(stringr::str_detect(phrase, to_cap))]

    browser()

    # Split the phrase into individual words

    if (split_on == " " & str_detect(phrase, " ")) {
      split_phrase <- str_split(phrase, " ") %>% as_vector()
    } else if (split_on == "_" & str_detect(phrase, "_")) {
      split_phrase <- str_split(phrase, "_") %>% as_vector()
    } else if (split_on == "both" & str_detect(phrase, " ") & str_detect(phrase, "_")) {
      split_phrase <- str_split(phrase, " ") %>% str_split(phrase, "_") %>% as_vector()
    } else if (split_on == "." & str_detect(phrase, "\\.")) {
      split_phrase <- str_split(phrase, "\\.") %>% as_vector()
    } else if (split_on == "") {
      split_phrase_first <- str_split(phrase, this_uncapped)[[1]][1]
      split_phrase_second <- str_extract(phrase, this_uncapped)
      split_phrase <- c(split_phrase_first, split_phrase_second)
    } else {
      split_phrase <- phrase
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
