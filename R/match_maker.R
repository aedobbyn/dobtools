#' Fuzzy Text Matching
#'
#' Find the best match (or no match at all) to string inputs.
#' @param e An character
#' @param matches A vector in which to look for matches with e.
#' @keywords match
#' @export
#' @examples
#'
#' iris %>% dplyr::mutate(
#'   foo = purrr::map_chr(as.character(Species), match_maker,
#'                 matches = c("Virginia", "California", "Sarasota"))
#'   )
#'
#' iris %>% dplyr::mutate(
#'   foo = purrr::map_chr(as.character(Species), match_maker,
#'                 matches = c("Virginia", "California", "Sarasota"), max_dist = 20)
#' )
#'
#' match_maker_full(as.character(iris$Species),
#'                  matches = c("Virginia", "California", "Sarasota"), max_dist = 20)
#'


match_maker <- function(e, matches, max_dist = 5) {

  matches[length(matches) + 1] <- "No matches found"  # Add a last element which we'll use in nomatch

  if (e %in% matches) {
    this_match <- matches[which(matches == e)]
  } else {
    this_match <- matches[stringdist::amatch(e, matches, maxDist = max_dist, nomatch = length(matches))]
  }
  return(this_match)
}


match_maker_full <- function(vec, matches, max_dist = 5) {
  out <- vector(length = length(vec))

  matches[length(matches) + 1] <- "No matches found"  # Add a last element which we'll use in nomatch

  out <- NULL
  for (i in vec) {
    if (i %in% matches) {
      this_match <- matches[which(matches == i)]
    } else {
      this_match <- matches[stringdist::amatch(i, matches, maxDist = max_dist, nomatch = length(matches))]
    }
    out <- c(out, this_match)
  }
  return(out)
}

