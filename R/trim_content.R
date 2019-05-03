#' Trim content
#'
#' For easy kable-izing, take a sample of a dataframe and trim a single column (usually a long string)
#' @param df Input dataframe
#' @param col Unquoted column name of character column
#' @param last_char Starting at character 1, how what's the last character that should be included.
#' @param sample_some Number of rows to sample. If NULL, all rows are retained.
#' @keywords trim
#' @export
#' @examples
#'
#' tbl <- janeaustenr::northangerabbey[100:109] %>%
#'   tibble::enframe() %>%
#'   dplyr::rename(
#'     content = value
#'   ) %>%
#'   dplyr::mutate(
#'     num = runif(10)
#'   )
#'
#' tbl %>% trim_content(last_char = 5, sample_some = 7)


trim_content <- function(df, col = content,
                         last_char = 50, sample_some = 3) {

  assertthat::assert_that(deparse(substitute(col)) %in% names(df),
                          msg = "col supplied must be present in df")
  assertthat::assert_that(is.null(sample_some) || is.numeric(sample_some),
                          msg = "sample_some must be NULL or numeric")

  if (!is.null(sample_some)) {
    df <- df %>%
      dplyr::sample_n(sample_some)
  }

  q_col <- rlang::enquo(col)

  out <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      !!rlang::quo_name(q_col) := substr(!!q_col, 1, last_char))

  return(out)
}
