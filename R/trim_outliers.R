#' Trim Outliers
#'
#' Remove outliers
#' @param df A dataframe.
#' @param cutoff A numeric cutoff value.
#' @param keep_scaled Should the newly scaled numeric variables be kept in the output?
#' @keywords trim
#' @import tidyverse
#' @export
#' @examples
#'
#' mtcars$mpg %>% cut_off_vecwise(20)
#' mtcars$carb %>% map(cut_off_elementwise)

trim_outliers <- function(df, cutoff = 1.96, keep_scaled = TRUE){
  df_scaled <- df %>%
    transmute_if(
      is.numeric, scale
    )
  names(df_scaled) <- names(df_scaled) %>% stringr::str_c("_scaled")

  df_out <- bind_cols(df_scaled, df)

  browser()
  df_out_trimmed <- df_out %>%
    filter_at(
      .vars = vars(contains("_scaled")),
      .vars_predicate = all_vars(. < abs(cutoff))
    )

  if(keep_scaled == FALSE) {
    df_out_trimmed <- select(names(df))
  }

  return(df_out_trimmed)
}

trim_outliers(iris, cutoff = 1.96, keep_scaled = TRUE)
