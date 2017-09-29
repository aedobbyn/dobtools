#' Replace Names
#'
#' Replace a vector of column names with another vector
#' @param orig A vector of names to replace
#' @param new The column name of the ref_df vector serving as the new names
#' @keywords names
#' @import tidyverse
#' @export
#' @examples
#'
#' replacement_names <- c("fee", "fi", "fo", "fum", "foo")
#' names(iris) %>% replace_colnames(replacement_names)

replace_names <- function(orig, new) {

  ref_df <- cbind(orig = orig, new = new) %>% as_tibble()

  vec <- orig

  for (i in seq_along(vec)) {
    if (vec[i] %in% ref_df$orig) {
      j <- which(ref_df$orig==vec[i])
      vec[i] <- ref_df$new[j]
    } else {
      vec[i] <- vec[i]
    }
  }
  vec
}


