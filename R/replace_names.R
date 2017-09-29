#' Replace Column Names
#'
#' Replace a vector of column names with another vector
#' @param vec A vector of names to replace
#' @param ref_df A reference dataframe that ties the vector of names to replace to its replacements
#' @param orig_names The column name of the ref_df vector serving as the original names
#' @param new The column name of the ref_df vector serving as the new names
#' @keywords names
#' @import tidyverse
#' @export
#' @examples
#'
#' original_names <- names(iris)
#' replacement_names <- c("fee", "fi", "fo", "fum", "foo")
#' name_df <- cbind(original_names, replacement_names) %>% as_tibble()
#'
#' names(iris) %>% replace_colnames(name_df, "original_names", "replacement_names")

replace_colnames <- function(vec, ref_df, orig, new) {
  for (i in seq_along(vec)) {
    if (vec[i] %in% ref_df[[orig]]) {
      j <- which(ref_df[[orig]]==vec[i])
      vec[i] <- ref_df[[new]][j]
    } else {
      vec[i] <- vec[i]
    }
  }
  vec
}


