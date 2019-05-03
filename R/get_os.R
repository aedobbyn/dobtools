
#' Get current operating system
#'
#' @return Name of current operating system.
#' @export
#'
#' @examples
#'
#' get_os()

get_os <- function() {
  tolower(Sys.info()[["sysname"]])
}
