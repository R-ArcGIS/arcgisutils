#' Configuration Options
#'
#' @name opts
#'
#' @description
#'
#' These options are used to customize behavior of the R packages within the R-ArcGIS Bridge project.
#'
#' ## Custom Headers: `arcgis.req_headers`
#'
#' A named list that is injected into all requests in `arc_base_req()` via [httr2::req_headers].
#'
#' Use this to inject, for example, a `Referer` header.
#'
#' ## Debug Requests: `arcgis.req_debug`
#'
#' When configured by a package, will print the cURL request created by the used package via [httr2::httr2_translate].
#' @examples
#' options(
#'   "arcgis.req_headers" = list(
#'     referer = "https://myportal.com",
#'     `X-Custom-Header` = "sneaky-value"
#'   )
#' )
#' # see the headers
#' arc_base_req("https://arcgis.com")
NULL
