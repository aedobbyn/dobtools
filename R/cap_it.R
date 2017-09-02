#' Cap It
#'
#' Capitalize a vector or dataframe using one of the apply or purrr functions.
#' @param e A vector or dataframe.
#' @keywords capitalize
#' @export
#' @examples
#' pre_cap_vec <- c("an all", "lowercase_string", "we.want", "to capitalize")
#' capped_vec <- pre_cap %>% map_chr(cap_it)
#'
#' pre_cap_df <- list("another all" = c("blah_blah", "blah"),
#'                    "lowercase.dataframe" = c(1, 4),
#'                    "to.cap" = c("BLAMO", "blam.o")) %>% as_tibble()
#'
#'
#' first_col <- pre_cap_df[[1]] %>% map_chr(cap_it)
#' second_col <- pre_cap_df[[2]] %>% map_chr(cap_it) # Numeric column coerced from numeric to character
#'
#' # Capitalize all character elements of dataframe and column names
#' capped_df <- pre_cap_df %>% map(cap_it) %>% as_tibble()  # Here, not coerced
#' names(capped_df) <- names(pre_cap_df) %>% map(cap_it) %>% as_vector()


cap_it <- function(e, ...) {

  if(is.character(e)) {
    if (grepl(pattern = "_", x = e) == TRUE) {
      e <- simple_cap(gsub(x = e, pattern = "_", replacement = " "))
    } else if (grepl(pattern = "\\.", x = e) == TRUE) {
      e <- simple_cap(gsub(x = e, pattern = "\\.", replacement = " "))
    } else {
      e <- Hmisc::capitalize(e)
    }
  } else {
    e
  }

}



