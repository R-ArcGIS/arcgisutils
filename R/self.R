#' Access the Self Resource
#'
#' The function returns the [`/self`](https://developers.arcgis.com/rest/users-groups-and-items/portal-self.htm) resource from the ArcGIS REST API. The `/self` endpoint
#' returns the view of the portal as seen by the current user, whether anonymous
#' or signed in.
#'
#' @details
#'
#' See the [endpoint documentation](https://developers.arcgis.com/rest/users-groups-and-items/portal-self.htm) for more details.
#'
#' The Portal Self response can vary based on whether it's called by a user, an app, or both.
#'
#' The response includes user and appinfo properties, and the variations in responses are primarily related to these two properties. As the names indicate, the user property includes information about the user making the call, and the appinfo property includes information pertaining to the app that made the call.
#'
#' @returns
#'
#' A named list.
#'
#' @inheritParams arc_base_req
#' @export
#' @examples
#' \dontrun{
#' set_arc_token(auth_code())
#' self <- arc_self_meta()
#' names(self)
#' }
arc_self_meta <- function(token = arc_token(), error_call = rlang::current_call()) {

  obj_check_token(token)

  burl <- file.path(
    # use the host from a token if set, otherwise default
    token[["arcgis_host"]] %||% arc_host(),
    "sharing", "rest", "portals", "self",
    fsep = "/"
  )

  b_req <- arc_base_req(burl, token)
  req <- httr2::req_body_form(b_req, f = "json")
  resp <- httr2::req_perform(req, error_call = error_call)

  res <- RcppSimdJson::fparse(
    httr2::resp_body_string(resp)
  )

  res

}
