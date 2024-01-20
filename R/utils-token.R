#' Retrieve ARCGIS Token
#'
#' Utility function for sanitizing token
#'
#' @details
#' Looks for an environment variable called "ARCGIS_TOKEN." If this does not
#' exist, it returns an empty token (i.e., NULL value).
#'
#' @return
#' If token exists, returns scalar character string; otherwise, NULL.
#' @export
#'
#' @examples
#' Sys.setenv(ARCGIS_TOKEN = "testit")
#' arc_token()
#' Sys.unsetenv("ARCGIS_TOKEN")
#' arc_token()
arc_token <- function() {
  token <- Sys.getenv("ARCGIS_TOKEN")
  if (token == "") {
    token <- NULL
  }
  token
}
