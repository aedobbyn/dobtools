#' Style Numeric
#'
#' Take a dataframe, round the digits to the thousandths, and optionally add commas.
#' @param df A dataframe.
#' @param digits The number of digits to round decimals to.
#' @param add_commas If TRUE, add commas in between 1000s.
#' @keywords numeric
#' @import scales
#' @export
#' @examples
#' iris_2 <- iris %>% as_tibble()
#' iris_2$Sepal.Width <- iris$Sepal.Width*10000
#' iris_2 %>% style_numeric(add_commas = TRUE)


style_numeric <- function(df, digits = 3, add_commas = FALSE) {
  df <- df %>% map_if(is.numeric, round, digits = digits) %>%
    as_tibble()

  if (add_commas == TRUE) {
    for (col in names(df)) {
      if(is.numeric(df[[col]])) {
        df[[col]] <- df[[col]] %>% scales::comma_format()()
      }
    }
  }
  return(df)
}

