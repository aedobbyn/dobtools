

cap_it <- function(i, ...) {

  if(is.character(i)) {
    if (grepl(pattern = "_", x = i) == TRUE) {
      i <- simple_cap(gsub(x = i, pattern = "_", replacement = " "))
    } else if (grepl(pattern = "\\.", x = i) == TRUE) {
      i <- simple_cap(gsub(x = i, pattern = "\\.", replacement = " "))
    } else {
      i <- Hmisc::capitalize(i)
    }
  } else {
    right_class <- class(i)
    i <- as(i, !!right_class)
  }
}


pre_cap_vec <- c("an all", "lowercase_string", "we.want", "to capitalize")

capped_vec <- pre_cap %>% map_chr(cap_it)
capped_vec

pre_cap_df <- list("another all" = c("blah_blah", "blah"),
                   "lowercase.dataframe" = c(1, 4),
                   "to.cap" = c("BLAMO", "blam.o")) %>% as_tibble()


first_col <- pre_cap_df[[1]] %>% map_chr(cap_it)

second_col <- pre_cap_df[[2]] %>% map_chr(cap_it)

capped_df <- pre_cap_df %>% map_df(cap_it)

