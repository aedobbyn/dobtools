#' Trello Point Summary
#'
#' Capitalize the result of a tidied regression.
#' Remove the Intercept term and Statistic column, capitalize all terms and names.
#' @param tidied_lm A tidied regression summary.
#' @keywords regression
#' @export
#' @examples
#' library(boom)
#' iris_lm <- lm(Sepal.Width ~ Sepal.Length, data = iris) %>% summary() %>% broom::tidy()
#' style_lm(iris_lm)

style_lm <- function(tidied_lm) {
  styled <- tidied_lm[-1, -4]  # Remove (Intercept) row and statistic column
  styled$term <- styled$term %>% map(dobtools::cap_it) %>% as_vector()
  names(styled) <- names(styled) %>% map(dobtools::cap_it) %>% as_vector()
  styled <- styled %>% capitalize_df()

  row.names(styled) <- NULL

  return(styled)
}


