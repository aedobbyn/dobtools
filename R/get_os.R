
#' Get current operating system
#'
#' @return
#' @export
#'
#' @examples
#'
#' get_os()

get_os <- function() {
  tolower(Sys.info()[["sysname"]])
}
