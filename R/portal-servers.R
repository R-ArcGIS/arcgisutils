#' List ArcGIS Enterprise Servers
#'
#' The servers resource lists the ArcGIS Server sites that have been federated with the portal.
#'
#' @inheritParams arc_portal_users
#' @export
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#' arc_portal_servers()
#' }
#' @returns a data.frame of servers
arc_portal_servers <- function(
  id = arc_portal_self(token)[["id"]],
  token = arc_token()
) {
  check_string(id)
  obj_check_token(token)
  resp <- arc_base_req(
    host,
    path = c("sharing", "rest", "portals", id, "servers"),
    query = c(f = "json"),
    token = token
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yyjsonr::read_json_str() |>
    detect_errors()

  if (!rlang::is_empty(resp[["servers"]])) {
    data_frame(resp[["servers"]])
  } else {
    resp
  }
}
