
#' Get Random Forest Variable Importance
#'
#' Given a random forest model, extract the importance coefficient and percent.
#' @param model A random forest model
#' @param keep_gini Keep the coefficient or just the percent importance?
#' @keywords importance
#' @export
#' @examples
#'
#' iris_rf <- ranger::ranger(Species ~ ., data = iris, importance = "impurity")
#' iris_rf %>% get_rf_importance()


get_rf_importance <- function(model, keep_gini = FALSE) {

  importance_sorted <- ranger::importance(model) %>% sort(., decreasing = TRUE) # Vector of imporances
  importance_names <- names(importance_sorted) %>%
    purrr::map(dobtools::cap_it) %>% purrr::as_vector()
  importance_vals <- importance_sorted %>% as.numeric() %>%
    purrr::map(round, digits = 1) %>% purrr::as_vector()

  importance_total <- sum(importance_vals)

  importance_df <- dplyr::bind_cols(`Variable Name` = importance_names,
                             "Importance" = importance_vals) %>%
    data.frame() %>%
    dplyr::mutate(
      `Importance Percent` = ((Importance/importance_total)) %>% round(digits=3) %>%
        scales::percent(),
      Importance = Importance %>% scales::comma_format()()
    )

  if(keep_gini == FALSE) {
    importance_df <- importance_df %>% dplyr::select(-Importance)
  }

  return(importance_df)
}
