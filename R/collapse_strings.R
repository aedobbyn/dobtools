
#' Collapse Strings
#'
#' Collapse a vector of strings into one single string separated by commas. The last
#' element will be preceed with an "and ,".
#' @param vec A vector of strings.
#' @keywords collapse
#' @import stringr
#' @export
#' @examples
#'
#' letters %>% collapse_strings()

collapse_strings <- function(vec) {
  len_vec <- length(vec)

  if(len_vec == 1) {
    out <- vec
  } else if (len_vec == 2) {
    out <- str_c(vec[1:2], collapse = " and ")
  } else if (len_vec > 2) {
    out <- str_c(vec[1:(len_vec - 1)], collapse = ", ") %>% str_c(vec[len_vec], sep = ", and ")
  }

  return(out)
}
