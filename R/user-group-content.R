#' Portal Content Items
#'
#' For a given user or group, returns a data.frame of all content items owned by them.
#'
#' @param group a scalar character of the group ID or a `PortalGroup` object created using [`arc_group()`]
#' @param user a scalar character of the username or a `PortalUser` object created using [`arc_user()`]
#' @inheritParams arc_item
#' @inheritParams arc_paginate_req
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
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
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

  fetch_content(
    c("sharing", "rest", "content", "groups", group),
    page_size,
    max_pages,
    .progress,
    host,
    token
  )
}


#' @name content
#' @export
arc_user_content <- function(
  user = arc_user_self(token = token),
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
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

  fetch_content(
    c("sharing", "rest", "content", "users", user),
    page_size,
    max_pages,
    .progress,
    host,
    token
  )
}

fetch_content <- function(
  path,
  page_size,
  max_pages,
  .progress,
  host,
  token
) {
  resps <- arc_base_req(
    host,
    path = path,
    query = c("f" = "json"),
    token = token
  ) |>
    arc_paginate_req(
      page_size = page_size,
      max_pages = max_pages,
      .progress = .progress
    )

  items <- lapply(resps, function(.resp) {
    RcppSimdJson::fparse(httr2::resp_body_string(.resp))[["items"]]
  })

  res <- data_frame(rbind_results(items))

  for (col in c("created", "modified", "lastViewed")) {
    res[[col]] <- from_esri_date(res[[col]])
  }

  res
}
