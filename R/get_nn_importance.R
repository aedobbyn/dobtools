#' Get Neural Net Variable Importance
#'
#' Given a neural net, extract the importance coefficient and percent.
#' @param model A neural net or multinomial neural net
#' @param keep_gini Keep the coefficient or just the percent importance?
#' @keywords importance
#' @export
#' @examples
#'
#' iris_nn <- nnet::nnet(Species ~ ., data = iris, size = 3)
#' iris_multinom <- nnet::multinom(Species ~ ., data = iris)
#' iris_nn %>% get_nn_importance()
#' iris_multinom %>% get_nn_importance()


get_nn_importance <- function(nn, keep_gini = FALSE) {

  importance_df <- nn %>% caret::varImp()
  names <- importance_df %>% row.names()

  importance_df <- importance_df %>%
    tibble::as_tibble() %>% dplyr::select(Overall) %>%
    dplyr::arrange(desc(Overall)) %>%
    dplyr::bind_cols(Variable = names) %>%
    dplyr::rename(Importance = Overall) %>%
    dplyr::mutate(
      `Importance Percent` = ((Importance/sum(.$Importance))) %>% round(digits=3) %>%
        scales::percent()
    )

  if(keep_gini == FALSE) {
    importance_df <- importance_df %>% dplyr::select(-Importance)
  }

  return(importance_df)
}
