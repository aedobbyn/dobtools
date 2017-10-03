#' Trim Outliers
#'
#' Remove outliers
#' @param df A dataframe.
#' @param cutoff A numeric cutoff value.
#' @param exclude Vector of variables not to subject to cutting off.
#' @param keep_scaled Should the newly scaled numeric variables be kept in the output?
#' @keywords trim
#' @import tidyverse
#' @export
#' @examples
#'
#' trim_outliers(iris, cutoff = 1, exclude = "Sepal.Length", keep_scaled = FALSE)


trim_outliers <- function(df, cutoff = 1.96, exclude = NULL, keep_scaled = TRUE){

  to_scale <- names(df)[!names(df) %in% exclude]

  df_scaled <- df %>%
    select(!!to_scale) %>%
    transmute_if(
      is.numeric, scale
    )
  names(df_scaled) <- names(df_scaled) %>% stringr::str_c("_scaled")

  df_out <- bind_cols(df_scaled, df)

  df_out_trimmed <- df_out %>%
    filter_at(
      .vars = vars(contains("_scaled")),
      .vars_predicate = all_vars(. < abs(cutoff))
    )

  if(keep_scaled == FALSE) {
    df_out_trimmed <- df_out_trimmed %>% select(!!names(df))
  }

  return(df_out_trimmed)
}

