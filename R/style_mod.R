#' Style glm or lm
#'
#' Style a model output depending on whether it's a linear model or generalized linear model.
#' @param d A dataframe.
#' @param var The quoted variable name you want to see a histogram of.
#' @keywords capitalize
#' @import tidyverse
#' @export
#' @examples
#' mt_lm <- lm(mpg ~ hp, data = mtcars)
#' mt_lm %>% style_mod()
#'
#' iris_glm <- glm(Species ~ Sepal.Width, data = iris, family = binomial)
#' iris_glm %>% style_mod()

style_mod <- function(m) {
  if(grepl("glm", summary(m)$call[1])) {
    summary(m)$coefficients %>% broom::tidy() %>%
      rename(term = `.rownames`,
             `p Value` = `Pr...z..`) %>% style_lm()  # Term renamed Variable in
  } else {
    summary(m) %>% broom::tidy() %>% style_lm()
  }
}

