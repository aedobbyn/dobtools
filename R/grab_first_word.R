#' Grab only the first word from a string.
#'
#' Specify the end of the word using the splitter variable.
#' @param d A character element to split.
#' @param splitter The character vector that delineates where the first word ends.
#' @keywords capitalize
#' @import tidyverse
#' @import stringr
#' @export
#' @examples
#' grab_first_word("some string but I just want the first word")
#'
#' starwars %>%
#'   select(name) %>%
#'   mutate(
#'     first_name = map_chr(name, grab_first_word),
#'     droid_first_name = map_chr(first_name, grab_first_word, splitter = "-")
#'   )


grab_first_word <- function(e, splitter = " ") {
  stopifnot(is.character(e))

  e <- e %>% stringr::str_split(pattern = splitter, simplify = TRUE) %>% first()
  return(e)
}

