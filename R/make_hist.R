#' Make a Histogram of a Variable
#'
#' Choose a variable from a dataframe and make a histogram out of it. Capitalizes variable names.
#' @param d A dataframe.
#' @param var The quoted variable name you want to see a histogram of.
#' @keywords capitalize
#' @import tidyverse
#' @export
#' @examples
#' make_hist(mtcars, "carb")


make_hist <- function(d, var) {
  tabl <- d %>%
    count_(var);
  ggplot(d) + geom_bar(aes_string(var), stat = "count") + theme_minimal() +
    ggtitle(paste0("Breakdown by ", cap_it(var))) +
    labs(x = cap_it(var), y = "Count")
}
