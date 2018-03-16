
#' Analyze Random Forest
#'
#' Take a random forest model and extract accuracy (if classification) or MSE (if regression).
#' @param model A random forest model object.
#' @keywords analyze
#' @export
#' @examples
#'
#' iris_rf <- ranger::ranger(Species ~ . , data = iris)
#' analyze_rf(iris_rf)

analyze_rf <- function(model) {

  rf_analyzed <- NULL

  if (model$treetype == "Classification") {
    # Percent accuracy = 1 - OOB error * 100.
    accuracy <- (1 - model$prediction.error)*100
    root_MSE <- NULL
  } else if (model$treetype == "Regression") {
    root_MSE <- sqrt(model$prediction.error)
    accuracy <- NULL
  }

  rf_analyzed <- list(accuracy = accuracy, root_MSE = root_MSE)

  return(rf_analyzed)
}
