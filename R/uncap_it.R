#' Uncap It
#'
#' Uncapitalize a vector or dataframe (usually the column names) and replace spaces
#' with underscores using one of the apply or purrr functions.
#' @param e A vector or dataframe.
#' @keywords uncapitalize
#' @import stringr
#' @export
#' @examples
#'
#' string_to_tidy <- "A String that needs Tidying"
#' uncap_it(string_to_tidy)


uncap_it <- function(e, ...) {
  e <- e %>%
    stringr::str_to_lower() %>%
    str_replace_all(., " ", "_")

  return(e)
}



