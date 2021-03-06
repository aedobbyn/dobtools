#' Style glm or lm
#'
#' Style a model output depending on whether it's a linear model or generalized linear model.
#' @param m The output (not summarized) of a generalized linear model (glm) or linear model (lm).
#' @param add_commas Should commas be added to numbers >= 1000?
#' @keywords style
#' @export
#' @examples
#' mt_lm <- lm(mpg ~ hp, data = mtcars)
#' mt_lm %>% style_mod()
#'
#' iris_glm <- glm(Species ~ Sepal.Width, data = iris, family = binomial)
#' iris_glm %>% style_mod()

style_mod <- function(m, add_commas = FALSE) {
  if(grepl("glm", summary(m)$call[1])) {
    m %>% summary() %>% purrr::pluck(coefficients) %>%
      broom::fix_data_frame() %>%
      dplyr::rename(`p Value` = `Pr...z..`) %>%
      style_lm(add_commas = add_commas)  # Term renamed Variable in style_lm
  } else {
    m %>% summary() %>% purrr::pluck(coefficients) %>%
      broom::fix_data_frame() %>%
      dplyr::rename(`p Value` = `Pr...t..`) %>%
      style_lm(add_commas = add_commas)
  }
}

