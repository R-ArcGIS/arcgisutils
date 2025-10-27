#' Portal Users
#'
#' This function lists all users in a portal.
#'
#' @param id the portal ID. By default it fetches the `id` from [`arc_portal_self()`].
#' @param sort_field optional field to sort by. It must be one of "username", "fullname", "created", "lastlogin", "mfaenabled", "level", "role".
#' @param provider optional filter users based on their identity provider. Must be one of "arcgis", "enterprise", "facebook", "google", "apple", or "github".
#' @param sort_order optional order to sort by. It must be one of `"asc"` or `"desc"`.
#' @param role optional role to filter down to. It must be one of "org_admin", "org_publisher", "org_user".
#' @param fullname optional string of the user's fullanme to search for.
#' @param username optional string of the user's user name to search for.
#' @param firstname optional string of the user's first name to search for.
#' @param lastname optional string of the user's last name to search for.
#' @param filter_intersection optional boolean value. If `TRUE` mutliple filters are treated as an `"and"` condition. If `FALSE`, treated as an `"or"`.
#' @inheritParams arc_base_req
#' @inheritParams arc_paginate_req
#' @inheritParams arc_user
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/users/)
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#' arc_portal_users()
#' }
#' @family portal
#' @returns a data.frame of users.
#' @export
arc_portal_users <- function(
  id = arc_portal_self(token)[["id"]],
  sort_field = NULL,
  provider = NULL,
  sort_order = NULL,
  role = NULL,
  fullname = NULL,
  username = NULL,
  firstname = NULL,
  lastname = NULL,
  filter_intersection = NULL,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  obj_check_token(token)
  check_string(id)
  check_bool(filter_intersection, allow_null = TRUE)
  check_string(fullname, allow_null = TRUE, allow_empty = FALSE)
  check_string(firstname, allow_null = TRUE, allow_empty = FALSE)
  check_string(lastname, allow_null = TRUE, allow_empty = FALSE)
  check_string(username, allow_null = TRUE, allow_empty = FALSE)

  if (!is.null(sort_field)) {
    rlang::arg_match(
      sort_field,
      c(
        "username",
        "fullname",
        "created",
        "lastlogin",
        "mfaenabled",
        "level",
        "role"
      )
    )
  }

  if (!is.null(provider)) {
    rlang::arg_match(
      provider,
      c("arcgis", "enterprise", "facebook", "google", "apple", "github")
    )
  }

  if (!is.null(provider)) {
    rlang::arg_match(provider, c("org_admin", "org_publisher", "org_user"))
  }

  if (!is.null(sort_order)) {
    rlang::arg_match(sort_order, c("asc", "desc"))
  }

  params <- compact(
    list(
      sortField = sort_field,
      sortOrder = sort_order,
      provider = provider,
      role = role,
      fullname = fullname,
      username = username,
      lastname - lastname,
      firstname = firstname,
      applyFilterIntersection = filter_intersection
    )
  )
  resps <- arc_base_req(
    host,
    path = c("sharing", "rest", "portals", id, "users"),
    token = token
  ) |>
    httr2::req_body_form(!!!params, f = "json") |>
    arc_paginate_req(
      max_pages = max_pages,
      page_size = page_size,
      .progress = .progress
    )

  results <- lapply(resps, function(.resp) {
    resp <- httr2::resp_body_string(.resp) |>
      yyjsonr::read_json_str() |>
      detect_errors()

    # extract the users from this
    users <- resp[["users"]]

    # unlist these fields in the event that there is a `-1`
    # which causes the dates to become a list
    users[["lastLogin"]] <- unlist(users$lastLogin, recursive = FALSE)
    users[["modified"]] <- unlist(users$modified, recursive = FALSE)
    users
  })

  res <- data_frame(rbind_results(results))
  res[["created"]] <- from_esri_date(as.numeric(res[["created"]]))
  res[["modified"]] <- from_esri_date(as.numeric(res[["modified"]]))
  res[["emailStatusDate"]] <- from_esri_date(as.numeric(res[[
    "emailStatusDate"
  ]]))
  res
}
