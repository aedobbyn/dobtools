#' Prep Clusters
#'
#' Prepare a dataframe for clustering. Specify a vector of predictors, scale some or all of them,
#' and specify a response variable.
#' Return a list with the original dataframe (df_for_clustering), the predictors (preds), and the
#' outcome variables (outcome).
#' @param df A dataframe containing data to cluster on.
#' @param preds A vector of predictor variables.
#' @param to_scale A subset of those predictors that should be scaled.
#' @param resp The response variable.
#' @keywords cluster
#' @export
#' @examples
#'
#'
#'
#' cluster_on <- c("mpg", "cyl", "disp", "hp", "drat")
#' to_scale <- c("mpg", "cyl", "disp")
#' response_vars <- c("gear", "carb")
#'
#' prep_car_clusters <- prep_clusters(df = mtcars,
#'                               preds = cluster_on,
#'                               to_scale = to_scale,
#'                               resp = response_vars)


prep_clusters <- function(df, preds, to_scale, resp) {
  df_for_clustering <- df %>%
    dplyr::select_(.dots = c(resp, preds)) %>%
    stats::na.omit()

  df_all_preds <- df_for_clustering %>%
    dplyr::select_(.dots = preds)

  df_preds_scale <- df_all_preds %>%
    dplyr::select_(.dots = to_scale) %>%
    scale() %>%
    tibble::as_tibble()

  df_preds <- dplyr::bind_cols(df_preds_scale, df_all_preds[, (!names(df_all_preds) %in% to_scale)])

  df_outcome <- df_for_clustering %>%
    dplyr::select_(.dots = resp) %>%
    stats::na.omit()

  cluster_prep_out <- list(df_for_clustering = df_for_clustering, preds = df_preds, outcome = df_outcome)

  return(cluster_prep_out)
}


