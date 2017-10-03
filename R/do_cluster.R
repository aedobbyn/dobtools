#' K-Means Cluster
#'
#' From beginning to end
#' @param df A dataframe
#' @param vars Variables from that dataframe to keep, some of which will be scaled and used for clustering.
#' @param to_scale A subset of vars, all numeric, to be scaled and used for clustering.
#' @param n_centers The number of clusters you want.
#' @keywords cluster
#' @import tidyverse
#' stringr
#' @export
#' @examples
#'
#' v <- names(iris)
#' t_s <- v[-which(v == "Species" | v == "Petal.Length")]
#'
#' clustered_iris <- do_cluster(iris, vars = v, to_scale = t_s, n_centers = 3)
#'
#' ggplot(data=clustered_iris %>% trim_outliers()) +
#'   geom_text(aes(Sepal.Width_scaled, Sepal.Length_scaled,
#'                 colour = cluster_assignment,
#'                 label = Species)) +
#'   theme_bw()


do_cluster <- function (df, vars, to_scale, n_centers = 5) {
  df_for_clustering <- df %>% select(!!vars) %>% na.omit()

  # Scale the ones to be scaled and append _scaled to their names
  df_vars_scale <- df_for_clustering %>% select(!!to_scale) %>%
    scale() %>% as_tibble()
  names(df_vars_scale) <- names(df_vars_scale) %>% stringr::str_c("_scaled")

  # Do the clustering on the scaled data
  set.seed(9)
  clusters_out <- kmeans(x = df_vars_scale, centers = n_centers, trace = FALSE)

  # Combine cluster assignment, scaled data, and unscaled rest of data
  clustered_df <- bind_cols(
    cluster_assignment = factor(clusters_out$cluster),   # Cluster assignment
    df_vars_scale,
    df_for_clustering
  )

  return(clustered_df)
}
