#' Style Numeric
#'
#' Take a dataframe, round the digits to the thousandths, and optionally add commas (except
#' to year columns).
#' @param df A dataframe.
#' @param digits The number of digits to round decimals to.
#' @param add_commas If TRUE, add commas in between 1000s.
#' @param dont_add_commas A character vector of columns that should not have commas added,
#' if add_commas is TRUE.
#' @keywords numeric

#' @export
#' @examples
#' iris_2 <- iris %>% tibble::as_tibble()
#' iris_2$Sepal.Width <- iris$Sepal.Width*1e+12
#' iris_2 %>% style_numeric(add_commas = TRUE)


style_numeric <- function(df, digits = 3, add_commas = FALSE, dont_add_commas = NULL) {
  year_cols <- names(df)[c(which(names(df)=="year"), which(names(df)=="Year"))]

  if(!is.null(dont_add_commas)) {year_cols <- c(year_cols, dont_add_commas)}

  if (length(year_cols) > 0) {
    non_year_cols <- names(df)[-which(names(df)==year_cols)]
  } else {
    non_year_cols <- names(df)
  }

  df <- df %>% purrr::map_if(is.numeric, round, digits = digits) %>%
    tibble::as_tibble()

  if (add_commas == TRUE) {
    for (col in names(df)) {
      if(is.numeric(df[[col]]) & !(col %in% year_cols)) {
        df[[col]] <- df[[col]] %>% scales::comma_format()()
      }
    }
    message(paste0("Adding commas to columns: ", stringr::str_c(non_year_cols, collapse = ", ")))
  }
  return(df)
}

