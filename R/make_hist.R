#' Make a Histogram of a Variable
#'
#' Choose a variable from a dataframe and make a histogram out of it. Capitalizes variable names.
#' @param d A dataframe.
#' @param var The quoted variable name you want to see a histogram of.
#' @param ... Other arguments
#' @keywords plot
#' @export
#' @examples
#' make_hist(mtcars, "carb")


make_hist <- function(d, var, ...) {
  tabl <- d %>%
    dplyr::count_(var);
  ggplot2::ggplot(d) + ggplot2::geom_bar(ggplot2::aes_string(var), stat = "count") + ggplot2::theme_minimal() +
    ggplot2::ggtitle(paste0("Breakdown by ", cap_it(var))) +
    ggplot2::labs(x = cap_it(var), y = "Count")
}
