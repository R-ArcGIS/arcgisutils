#' Discover Authenticated User Metadata
#'
#' Given an authentication token, return a list of user-specfic information such as the user ID, username, available credits, email, groups, last login date and more.
#'
#' @inheritParams arc_item
#' @keywords portal self
#' @export
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/self/)
#' @returns a list of the authenticated user's metadata
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

  date_fields <- c("created", "modified", "emailStatusDate", "lastLogin")

  for (field in date_fields) {
    res[[field]] <- from_esri_date(res[[field]])
  }

  # make into a tbl
  res[["groups"]] <- data_frame(res[["groups"]])
  # process dates

  for (field in c("modified", "created")) {
    col <- res[["groups"]][[field]]
    res[["groups"]][[field]] <- from_esri_date(col)
  }

  res
}
