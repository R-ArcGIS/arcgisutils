#' List users in a group
#'
#' List all users in a given group.
#'
#' @param group a group ID as a scalar string or `PortalGroup` created via [`arc_group()`]
#' @param name a scalar string of a user to search for.
#' @param member_type default `NULL`. If provided must be one of `"admin"` or `"member"`.
#' @param joined_before default `NULL`. A scalar date to search for users who joined before this date.
#' @param joined_after default `NULL`. A scalar date to search for users who joined after this date.
#' @param sort_field default `"username"`. The field to sort by. Must be one of `"username"`, `"membertype"`, or `"joined"`.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/group-users-list/)
#' @inheritParams search_items
#' @export
#' @returns a data.frame
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#' groups <- arc_user("r-bridge-docs")$groups
#' arc_group_users(groups$id[1])
#' }
arc_group_users <- function(
  group,
  name = NULL,
  member_type = NULL,
  joined_before = NULL,
  joined_after = NULL,
  sort_field = c("username", "membertype", "joined"),
  sort_order = c("asc", "desc"),
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  # validate user groups
  if (inherits(group, "PortalGroup")) {
    group <- group$id
    if (!rlang::is_string(group)) {
      cli::cli_abort(
        "{.cls PortalGroup} has an invalid group ID. Expected a string"
      )
    }
  } else if (rlang::is_string(group)) {
    group <- group
  } else {
    cli::cli_abort(
      "Expected a {.cls PortalGroup} or a bare string of the group ID. Found {obj_type_friendly(group)}"
    )
  }

  check_string(name, allow_null = TRUE)

  # if not-missing, validate
  if (!is.null(member_type)) {
    member_type <- rlang::arg_match(
      member_type,
      c("admin", "member")
    )
  }

  sort_field <- rlang::arg_match(
    sort_field
  )
  sort_order <- rlang::arg_match(sort_order)

  if (!is.null(joined_after)) {
    joined_after <- date_to_ms(joined_after)
  }

  if (!is.null(joined_before)) {
    joined_before <- date_to_ms(joined_before)
  }

  joined <- if (!is.null(joined_before) || !is.null(joined_after)) {
    sprintf("%s,%s", joined_after %||% "", joined_before %||% "")
  } else {
    NULL
  }

  params <- compact(list(
    sortField = sort_field,
    sortOrder = sort_order,
    joined = joined,
    name = name,
    joined = joined
  ))

  resps <- arc_base_req(
    host,
    path = c("sharing", "rest", "community", "groups", group, "userList"),
    query = c("f" = "json"),
    token = token
  ) |>
    httr2::req_url_query(!!!params) |>
    arc_paginate_req(
      max_pages = max_pages,
      page_size = page_size,
      .progress = .progress
    )

  results <- lapply(resps, function(.resp) {
    resp <- httr2::resp_body_string(.resp) |>
      yyjsonr::read_json_str() |>
      detect_errors()
    res <- resp[["users"]]
  })

  res <- data_frame(rbind_results(results))
  res[["joined"]] <- from_esri_date(as.numeric(res$joined))
  res
}
