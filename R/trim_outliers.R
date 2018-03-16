#' Trim Outliers
#'
#' Remove outliers by either choosing a vector of column names to include for consideration or choosing a vector
#' for exclusion. Either include or exclude must be non-NULL (not neither and not both).
#' @param df A dataframe.
#' @param cutoff A numeric standard deviation cutoff value.
#' @param include Vector of variables to subject to cutting off.
#' @param exclude Vector of variables not to subject to cutting off.
#' @param keep_scaled Should the newly scaled numeric variables be kept in the output?
#' @keywords trim
#' @export
#' @examples
#'
#' trim_outliers(iris, cutoff = 1, exclude = c("Species", "Sepal.Length"), keep_scaled = FALSE)
#' trim_outliers(iris, cutoff = 2, include = c("Petal.Width", "Sepal.Length"), keep_scaled = TRUE)
#'
#' # Error -- exclude and include can't both be non-NULL
#' trim_outliers(iris, cutoff = 2, exclude = "Species", include = c("Petal.Width", "Sepal.Length"), keep_scaled = TRUE)
#'
#' # Error -- exclude and include can't both be NULL
#' trim_outliers(iris, cutoff = 2, keep_scaled = TRUE)


trim_outliers <- function(df, cutoff = 1.96, exclude = NULL, include = NULL, keep_scaled = TRUE){

  add_scale_names <- function(n) {
    if(n %in% to_scale) {
      n <- n %>% stringr::str_c("_scaled")
    } else {
      n <- n
    }
  }

  do_scale_names <- function(df) {
    names(df) <- names(df) %>% purrr::map(add_scale_names) # %>% purrr::as_vector()
    return(df)
  }

  assertthat::assert_that(!(is.null(exclude) & is.null(include)),
                          msg = "Either include or exclude must not be NULL.")

  assertthat::assert_that(!(!is.null(exclude) & !(is.null(include))),
                          msg = "Only one of include and exclude can be non-NULL.")

  if (!is.null(exclude)) {
    to_scale <- names(df)[!names(df) %in% exclude]
  }

  if (!is.null(include)) {
    to_scale <- names(df)[names(df) %in% include]
  }

  assertthat::assert_that(!(FALSE %in%
        sapply(df[, which(names(df) %in% to_scale)], is.numeric)),
        msg = "All variables to scale must be numeric.")

  scale_and_vectorize <- function(x) {x <- as.vector(scale(x))}

  df_scaled <- df %>%
    # select(!!to_scale) %>%
    dplyr::mutate_at(to_scale, scale_and_vectorize)

  df_scaled <- df_scaled %>% do_scale_names() %>% purrr::map_dfc(., purrr::as_vector)   # Add the scaled bit

  df_out <- dplyr::bind_cols(df_scaled, df)

  df_out_trimmed <- df_out %>%
    dplyr::filter_at(
      .vars = dplyr::vars(dplyr::contains("_scaled")),
      .vars_predicate = dplyr::all_vars(. < abs(cutoff))
    )

  if(keep_scaled == FALSE) {
    df_out_trimmed <- df_out_trimmed %>% dplyr::select(!!names(df))
  }

  return(df_out_trimmed)
}

