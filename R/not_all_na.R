#' Not all NA
#'
#' @description Logical for whether a vector contains all NAs
#' @param x Vector
#'
#' @export
#'
#' @examples
#'
#' c(1:3) %>% not_all_na()
#' rep(NA, 3) %>% not_all_na()
#' c(1, 2, NA) %>% not_all_na()
#
# starwars %>%
#   dplyr::select_if(not_all_na)

not_all_na <- function(x) !all(is.na(x))


