#' Not In
#'
#' @description Shortcut for !x %in% y
#' @export
#'
#' @examples
#' mtcars$gear[mtcars$gear %notin% 3]
#'

`%notin%` <- Negate(`%in%`)
