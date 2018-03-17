#' Run a Random Forest model
#'
#' Prepare and run a random forest model.
#' @param data A dataframe
#' @param predictor_vars A vector of predictors
#' @param outcome_var A single outcome variable. Factor or numeric.
#' @param n_trees Number of trees to be grown in ranger::num.trees
#' @param prob Probability or response outcome? If TRUE, outcome variable must be a factor.
#' @keywords rf
#' @export
#' @examples
#'
#' preds <- c("disp", "hp", "qsec")
#' response <- "carb"
#' mt_rf <- run_rf(mtcars, preds, response, prob = FALSE)


run_rf <- function(data, predictor_vars, outcome_var, n_trees = 500, prob = FALSE) {

  data <- data %>%
    dplyr::mutate(
      row_num = 1:nrow(.)
    )

  both <- c(predictor_vars, outcome_var, "row_num")
  quo_both <- rlang::enquo(both)

  # Take out columns we don't need and remove rows with missing values from the ones we do
  dat_forest <- data %>% dplyr::ungroup() %>%
    dplyr::select_(.dots = both) %>%
    stats::na.omit()

  # Keep 80% for training
  forest_train <- dat_forest %>%
    dplyr::sample_frac(., 0.8)

  # The rest is for testing
  forest_test <- dat_forest %>%
    dplyr::filter(! (row_num %in% forest_train$row_num)) %>%
    dplyr::select(-row_num)

  # Save training df as global variable
  forest_train <- forest_train %>%
    dplyr::select(-row_num)

  # Separate rf_preds with +s and take out row_num
  rf_preds_sep <- paste0(predictor_vars, collapse = " + ")

  # Prepare the formula
  form <- stats::as.formula(paste0(outcome_var, " ~ ", rf_preds_sep))

  # Run the model
  if(prob == TRUE) {
    rf_mod <- ranger::ranger(form, data = forest_train,
                     probability = TRUE,
                     verbose = FALSE,
                     write.forest = TRUE,
                     seed = 11,
                     num.trees = n_trees,   # <------ default is 500
                     importance = "impurity")
  } else {
    rf_mod <- invisible(ranger::ranger(form, data = forest_train,
                               probability = FALSE,
                               verbose = FALSE,
                               write.forest = TRUE,
                               seed = 11,
                               num.trees = n_trees,   # <------ default is 500
                               importance = "impurity"))
  }

  return(rf_mod)
}


