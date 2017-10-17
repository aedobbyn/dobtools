#' Get Random Forest Variable Importance
#'
#' Given a variable impor, extract the importance coefficient and percent.
#' @param model A random forest model
#' @param keep_gini Keep the coefficient or just the percent importance?
#' @keywords importance
#' @import ranger
#' @import tidyverse
#' @import nnet
#' @import caret
#' @export
#' @examples
#'
#' iris_nn <- nnet::nnet(Species ~ ., data = iris, size = 3)
#' iris_nn %>% get_nn_importance()


get_nn_importance <- function(nn, keep_gini = FALSE) {

  importance_df <- nn %>% caret::varImp() %>%
    dplyr::arrange(desc(Overall)) %>%
    bind_cols(Variable = row.names(importance_vec)) %>%
    rename(Importance = Overall) %>%
    mutate(
      `Importance Percent` = ((Importance/sum(.$Importance))) %>% round(digits=3) %>%
        scales::percent()
    )

  if(keep_gini == FALSE) {
    importance_df <- importance_df %>% select(-Importance)
  }

  return(importance_df)
}
