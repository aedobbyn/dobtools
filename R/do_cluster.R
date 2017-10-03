#' K-Means Cluster
#'
#' From beginning to end
#' @param df A dataframe
#' @param vars Variables from that dataframe to keep, some of which will be scaled and used for clustering.
#' @param to_scale A subset of vars, all numeric, to be scaled and used for clustering.
#' @param n_centers The number of clusters you want.
#' @keywords cluster
#' @import tidyverse
#' @export
#' @examples




do_cluster <- function (df, vars, to_scale, n_centers = 5) {
  df_for_clustering <- df %>% select(!!vars) %>% na.omit()

  # Scale the ones to be scaled and append _scaled to their names
  df_vars_scale <- df_for_clustering %>% select(!!to_scale) %>%
    scale() %>% as_tibble()
  names(df_vars_scale) <- names(df_vars_scale) %>% str_c("_scaled")

  # # Put the scaled variables together with the rest of them
  # cluster_prep_out <- bind_cols(df_vars_scale,
  #                               df_for_clustering)

  # Find the indices of
  # scaled_var_inds <- which(grepl("_scaled", names(cluster_prep_out)))

  set.seed(9)
  clusters_out <- kmeans(x = df_vars_scale, centers = n_centers, trace = FALSE)

  clustered_df <- as_tibble(data.frame(
    cluster_assignment = factor(clusters_out$cluster),   # Cluster assignment
    # clustered_df_out,
    df_vars_scale,
    df_for_clustering
  ))

  return(clustered_df)
}
