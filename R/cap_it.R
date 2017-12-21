#' Cap It
#'
#' Capitalize a vector or dataframe using one of the apply or purrr functions.
#' @param e A vector or dataframe.
#' @keywords capitalize
#' @import Hmisc
#' @import tidyverse
#' @export
#' @examples
#' library(tidyverse)
#' library(Hmisc)
#' pre_cap_vec <- c("an all", "lowercase_string", "we.want", "to capitalize")
#' capped_vec <- pre_cap_vec %>% map_chr(cap_it)
#'
#' pre_cap_df <- list("another all" = c("blah_blah", "blah"),
#'                    "lowercase.dataframe" = c(1, 4),
#'                    "to.cap" = c("BLAMO", "blam.o")) %>% data.frame()
#'
#'
#' first_col <- pre_cap_df[[1]] %>% map_chr(cap_it)
#' second_col <- pre_cap_df[[2]] %>% map_chr(cap_it) # Numeric column coerced from numeric to character
#'
#' # Capitalize all character elements of dataframe and column names
#' capped_df <- pre_cap_df %>% map(cap_it) %>% data.frame()  # Here, not coerced
#' names(capped_df) <- names(pre_cap_df) %>% map(cap_it) %>% as_vector()

cap_it <- function(e, ...) {

  dont_cap_me <- c("a", "the", "this", "and", "but")
  dont_cap_me_capped <- dont_cap_me %>% cap_it()
  dont_cap_me <- c(dont_cap_me, dont_cap_me_capped)

  if(is.character(e) & !(e %in% dont_cap_me)) {
    if (grepl(pattern = "_", x = e) == TRUE) {
      e <- simple_cap(gsub(x = e, pattern = "_", replacement = " "))
    } else if (grepl(pattern = "\\.", x = e) == TRUE) {
      e <- simple_cap(gsub(x = e, pattern = "\\.", replacement = " "))
    } else if (grepl(pattern = "-", x = e) == TRUE) {
      e <- simple_cap(gsub(x = e, pattern = "-", replacement = " "))  # Keep the dash for, e.g., Mercedes-Benz
      e <- gsub(x = e, pattern = " ", replacement = "-")
    } else {
      e <- simple_cap(e)
    }
  } else {
    e
  }
  return(e)
}

