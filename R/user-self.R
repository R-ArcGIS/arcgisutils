#' Discover Authenticated User Metadata
#'
#' Given an authentication token, return a list of user-specfic information such as the user ID, username, available credits, email, groups, last login date and more.
#'
#' @inheritParams arc_item
#' @inheritParams arc_base_req
#' @keywords portal self
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/self/)
#' @returns a list of the authenticated user's metadata
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   arc_user_self(token = auth_user())
#' }
#' }
arc_user_self <- function(
  host = arc_host(),
  token = arc_token(),
  error_call = rlang::caller_call()
) {
  req <- arc_base_req(
    host,
    token,
    path = "sharing/rest/community/self",
    query = c("f" = "json")
  )

  resp <- httr2::req_perform(
    req,
    error_call = error_call
  )

  res <- httr2::resp_body_string(resp) |>
    RcppSimdJson::fparse() |>
    detect_errors()

  as_portal_user(res)
}

as_portal_user <- function(res) {
  for (field in c("created", "modified", "emailStatusDate", "lastLogin")) {
    if (!is.null(res[[field]])) {
      res[[field]] <- from_esri_date(res[[field]])
    }
  }

  if (!is.null(res[["groups"]])) {
    groups <- data_frame(res[["groups"]])

    for (field in c("created", "modified")) {
      if (!is.null(groups[[field]])) {
        groups[[field]] <- from_esri_date(groups[[field]])
      }
    }

    res[["groups"]] <- groups
  }

  structure(res, class = c("PortalUser", "list"))
}
