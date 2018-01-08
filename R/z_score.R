#' Z-Score a vector
#'
#' Scale a vector and get a plain vanilla numeric vector back.
#' @param vec A numeric vector
#' @keywords scale
#' @import tidyverse
#' @export
#' @examples
#'
#' z_score(mtcars$mpg)

z_score <- function(vec) {
  vec_mean <- mean(vec)
  vec_sd <- sd(vec)

  get_score <- function(e) {
    z <- (e - vec_mean) / vec_sd
  }

  vec_z <- vec %>% map_dbl(get_score)
  return(vec_z)
}

