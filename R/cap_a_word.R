#' Cap a Word
#'
#' Find a word within a word and capitalize it.
#' @param phrase A single element or phrase containing one or more of the elements in `to_cap`
#' @param to_cap A vector of words that, if contained in `phrase`, will be fully capitalized.
#' @param collapse What separator should the whole thing be collapsed down with?
#' @param no_sep The seperator is not a " " or a "_", we've got something like "thisId"
#' @keywords cap
#' @export
#' @examples
#'
#' cap_a_word("this_id")
#' cap_a_word("this Id")
#' cap_a_word("this_id", collapse = "_")
#' cap_a_word("this_id", collapse = "")
#' cap_a_word("thisid", no_sep = TRUE)
#' cap_a_word("this.id")
#'
#' words_to_cap <- c("Petal", "Width")

cap_a_word <- function(phrase, to_cap = c("id", "Id"),
                       collapse = " ", no_sep = FALSE) {

  assertthat::assert_that(rlang::is_character(phrase) && rlang::is_character(to_cap),
                          msg = "The phrase and words to find must be characters.")


  # If any of the words to cap appear in the phrasetor
  if(any(stringr::str_detect(phrase, to_cap))) {
    # Find which of our keywords word we need to capitalize
    this_uncapped <- to_cap[which(stringr::str_detect(phrase, to_cap))]

    # Split the phrase into individual words
    splitters <- "_| |\\."
    if (no_sep == FALSE & stringr::str_detect(phrase, splitters)) {
      split_phrase <- stringr::str_split(phrase, splitters) %>% purrr::as_vector()
    } else if (no_sep == TRUE) {
      split_phrase_first <- stringr::str_split(phrase, this_uncapped)[[1]][1]
      split_phrase_second <- stringr::str_extract(phrase, this_uncapped)
      split_phrase <- c(split_phrase_first, split_phrase_second)
    } else if (no_sep == FALSE & (stringr::str_detect(phrase, splitters) == FALSE)) {
      split_phrase <- phrase
    }

    # Find the index of the word in the phrasetor that needs to be capitalized
    to_cap_ind <- which(split_phrase == this_uncapped)

    # Capitalize just that index
    split_phrase[to_cap_ind] <- split_phrase[to_cap_ind] %>% toupper()

    # Put it all back together
    phrase <- stringr::str_c(split_phrase, collapse = collapse)

    # Eliminate any leading whitespace caused by split_phrase_first
    if(substr(phrase, 1, 1) == " ") {
      phrase <- substr(phrase, 2, nchar(phrase))
    }
  }
  return(phrase)
}
