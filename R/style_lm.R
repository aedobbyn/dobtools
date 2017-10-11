#' Style a Regression Model Output
#'
#' Capitalize and round the result of a tidied regression.
#' Remove the Intercept term and Statistic column, capitalize all terms and names.
#' Round all numbers to the thousandths.
#' @param tidied_lm A tidied regression summary.
#' @keywords regression
#' @export
#' @examples
#' iris_lm <- lm(Sepal.Width ~ Sepal.Length, data = iris) %>% summary() %>% broom::tidy()
#' iris_lm[2, 3] <- 1234567
#' style_lm(iris_lm, add_commas = TRUE)


style_lm <- function(tidied_lm, add_commas = FALSE) {
  styled <- tidied_lm[-1, -4]  # Remove (Intercept) row and statistic column
  styled$term <- styled$term %>% map(dobtools::cap_it) %>% as_vector()
  names(styled) <- names(styled) %>% map(dobtools::cap_it) %>% as_vector()
  styled <- styled %>% cap_df() %>%
    rename(
      Variable = Term
    ) %>% style_numeric(add_commas = add_commas)

  row.names(styled) <- NULL

  return(styled)
}


