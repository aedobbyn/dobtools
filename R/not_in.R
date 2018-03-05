#' Not In
#'
#' @description
#' @export
#'
#' @examples
#'
#' mtcars$gear[mtcars$gear %notin% 3]

`%notin%` <- Negate(`%in%`)
