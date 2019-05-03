#' Disconnect All
#'
#' Disconnect all open database connections.
#' @param drv A db driver. Defaults to \code{RMySQL::MySQL()}.
#' @keywords database
#' @export
#' @return List of all closed connections.
#' @examples
#' \dontrun{
#'  disconnect_all()
#' }

disconnect_all <- function(drv = RMySQL::MySQL()) {
  lapply(DBI::dbListConnections(drv), DBI::dbDisconnect)
}
