#' Replace Names
#'
#' Replace a vector of column names with another vector
#' @param vec Vector, some or all of which you want to replace
#' @param to_replace Subset of vec that you want to replace
#' @param new Replacements for to_replace
#' @keywords names
#' @export
#' @examples
#'
#' names_to_replace <- names(iris)[1:3]
#' replacement_names <- c("fee", "fi", "fo")
#' replace_names(c("foo", names(iris), "bar"), names_to_replace, replacement_names)

replace_names <- function(vec, to_replace, new) {

  ref_df <- cbind(to_replace = to_replace, new = new) %>% tibble::as_tibble()

  for (i in seq_along(vec)) {
    if (vec[i] %in% ref_df$to_replace) {
      j <- which(ref_df$to_replace == vec[i])
      vec[i] <- ref_df$new[j]
    } else {
      vec[i] <- vec[i]
    }
  }
  return(vec)
}


