#' Portal File Resources
#'
#' The resources endpoint lists all file resources for the organization.
#'
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/resources-portal/)
#' @returns a data.frame of resources available to your portal.
#' @family portal
#' @export
#' @inheritParams arc_paginate_req
#' @inheritParams arc_base_req
#' @inheritParams arc_portal_users
#' @examples
#' \dontrun{
#'   set_arc_token(auth_user())
#'   arc_portal_resources()
#' }
#'
arc_portal_resources <- function(
  id = arc_portal_self(token)[["id"]],
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  token = arc_token()
) {
  check_string(id)
  obj_check_token(token)
  resps <- arc_base_req(
    host,
    path = c("sharing", "rest", "portals", id, "resources"),
    query = c(f = "json"),
    token = token
  ) |>
    arc_paginate_req(
      max_pages = max_pages,
      page_size = page_size,
      .progress = .progress
    )

  results <- lapply(resps, function(.resp) {
    resp <- httr2::resp_body_string(.resp) |>
      yyjsonr::read_json_str() |>
      detect_errors()
    resp[["resources"]]
  })

  res <- data_frame(rbind_results(results))
  res[["created"]] <- from_esri_date(as.numeric(res[["created"]]))
  res
}
