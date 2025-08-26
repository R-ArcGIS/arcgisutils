#' Portal Content Items
#'
#' For a given user or group, returns a data.frame of all content items owned by them.
#'
#' @param group a scalar character of the group ID or a `PortalGroup` object created using [`arc_group()`]
#' @param user a scalar character of the username or a `PortalUser` object created using [`arc_user()`]
#' @inheritParams arc_item
#' @keywords content portal
#' @export
#' @examples
#' \dontrun{
#' library(arcgis)
#'
#' # authenticate
#' set_arc_token(auth_user())
#'
#' # get your own content items
#' self <- arc_user_self()
#' arc_user_content(self$username)
#'
#' # get a specific group's items
#' arc_group_content("2f0ec8cb03574128bd673cefab106f39")
#' }
#' @references
#' - [Group Content API Reference](https://developers.arcgis.com/rest/users-groups-and-items/group-content/)
#' - [User Content API Reference](https://developers.arcgis.com/rest/users-groups-and-items/user-content/)
#' @name content
#' @returns a `data.frame` of content item metadata
arc_group_content <- function(
  group,
  host = arc_host(),
  token = arc_token()
) {
  if (inherits(group, "PortalGroup")) {
    group <- group$id
  }

  if (!rlang::is_string(group)) {
    cli::cli_abort(
      "{.arg group} must be a string or {.cls PortalGroup} created with {.fn arc_group}"
    )
  }

  req <- arc_base_req(
    host,
    path = c("sharing", "rest", "content", "groups", group),
    query = c("f" = "json"),
    token = token
  )

  all_resps <- arc_paginate_req(req)

  results <- lapply(all_resps, function(.resp) {
    RcppSimdJson::fparse(httr2::resp_body_string(.resp))[["items"]]
  })

  res <- data_frame(rbind_results(results))

  res[["created"]] <- from_esri_date(res[["created"]])
  res[["modified"]] <- from_esri_date(res[["modified"]])
  res[["lastViewed"]] <- from_esri_date(res[["lastViewed"]])
  res
}


#' @name content
#' @export
arc_user_content <- function(
  user,
  host = arc_host(),
  token = arc_token()
) {
  if (inherits(user, "PortalUser")) {
    user <- user$username
  }

  if (!rlang::is_string(user)) {
    cli::cli_abort(
      "{.arg user} must be a string or {.cls PortalUser} created with {.fn arc_user}"
    )
  }

  req <- arc_base_req(
    host,
    path = c("sharing", "rest", "content", "users", user),
    query = c("f" = "json"),
    token = token
  )

  all_resps <- arc_paginate_req(req)

  results <- lapply(all_resps, function(.resp) {
    RcppSimdJson::fparse(httr2::resp_body_string(.resp))[["items"]]
  })

  res <- data_frame(rbind_results(results))

  res[["created"]] <- from_esri_date(res[["created"]])
  res[["modified"]] <- from_esri_date(res[["modified"]])
  res[["lastViewed"]] <- from_esri_date(res[["lastViewed"]])
  res
}
