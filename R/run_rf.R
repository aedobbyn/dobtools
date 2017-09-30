#' Run a Random Forest model
#'
#' Prepare and run a random forest model.
#' @param predictor_vars A vector of predictors
#' @param outcome_var A single outcome variable. Factor or numeric.
#' @param prob Probability or response outcome? If TRUE, outcome variable must be a factor.
#' @keywords rf
#' @import ranger
#' @import tidyverse
#' @export
#' @examples
#'
#' preds <- c("disp", "hp", "qsec")
#' response <- "carb"
#' mt_rf <- run_rf(mtcars, preds, response, prob = FALSE, filter_var = "vs", filter_to = 0)


run_rf <- function(data, predictor_vars, outcome_var, n_trees = 500, prob = FALSE) {

  data <- data %>%
    mutate(
      row_num = row_number()
    )

  both <- c(predictor_vars, outcome_var, "row_num")
  quo_both <- enquo(both)

  # Take out columns we don't need and remove rows with missing values from the ones we do
  dat_forest <- data %>% ungroup() %>%
    select_(.dots = both) %>%
    na.omit()

  # Keep 80% for training
  forest_train <- dat_forest %>%
    sample_frac(., 0.8)

  # The rest is for testing
  forest_test <- dat_forest %>%
    filter(! (row_num %in% forest_train$row_num)) %>%
    select(-row_num)

  # Save training df as global variable
  forest_train <- forest_train %>%
    select(-row_num)

  # Separate rf_preds with +s and take out row_num
  rf_preds_sep <- paste0(predictor_vars, collapse = " + ")

  # Prepare the formula
  form <- as.formula(paste0(outcome_var, " ~ ", rf_preds_sep))

  # Run the model
  if(prob == TRUE) {
    rf_mod <- ranger(form, data = forest_train,
                     probability = TRUE,
                     verbose = FALSE,
                     write.forest = TRUE,
                     seed = 11,
                     num.trees = n_trees,   # <------ default is 500
                     importance = "impurity")
  } else {
    rf_mod <- invisible(ranger(form, data = forest_train,
                               probability = FALSE,
                               verbose = FALSE,
                               write.forest = TRUE,
                               seed = 11,
                               num.trees = n_trees,   # <------ default is 500
                               importance = "impurity"))
  }



  return(rf_mod)

}


