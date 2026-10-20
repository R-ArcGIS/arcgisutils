#' Search for Groups, Users, and Group Content
#'
#' Searches a portal for groups, for users, or for the items shared with a
#' single group.
#'
#' @param group a `PortalGroup` from [`arc_group()`] or a scalar character
#'   group ID.
#' @param query optional. A scalar character for free text search or a query
#'   string as defined by the REST API.
#' @param filter optional. A scalar character of exact-match terms in the form
#'   `field:"term"`, for example `owner:"jdoe"`.
#' @param sort_field optional. A scalar character naming the field to sort by.
#'   `search_groups()` accepts `"title"`, `"owner"`, `"created"`, and
#'   `"modified"`. `search_users()` accepts `"username"`, `"fullname"`,
#'   `"created"`, and `"modified"`. `search_group_content()` accepts the
#'   values listed in [`search_items()`] and `"added"`.
#' @param sort_order optional. One of `"asc"` or `"desc"`. The portal sorts
#'   in ascending order by default.
#' @param member_only default `FALSE`. When `TRUE`, `search_groups()` searches
#'   only the groups that the signed in user owns or belongs to.
#' @param categories optional. A character vector of up to 8 full category
#'   paths, such as `"/Categories/Water"`. `search_group_content()` returns
#'   items that match any of them.
#' @inheritParams search_items
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Supply `query`, `filter`, or both. When you supply both, the portal
#' returns results that match the query and the filter.
#' @references
#' - [Group Search API Reference](https://developers.arcgis.com/rest/users-groups-and-items/group-search/)
#' - [User Search API Reference](https://developers.arcgis.com/rest/users-groups-and-items/user-search/)
#' - [Group Content Search API Reference](https://developers.arcgis.com/rest/users-groups-and-items/group-content-search/)
#' @export
#' @family portal search
#' @returns a `data.frame` with one row per result. Zero rows when nothing
#'   matches.
#' @examplesIf curl::has_internet()
#' search_groups("Living Atlas", max_pages = 1)
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' search_groups(filter = 'owner:"jdoe"', member_only = TRUE)
#' search_users("parry", sort_field = "username")
#' search_group_content("2f0ec8cb03574128bd673cefab106f39", "parcels")
#' }
search_groups <- function(
  query = NULL,
  filter = NULL,
  sort_field = NULL,
  sort_order = NULL,
  member_only = FALSE,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  check_bool(member_only)

  params <- search_params(
    query,
    filter,
    sort_field,
    sort_order,
    c("title", "owner", "created", "modified")
  )

  if (member_only) {
    params[["searchUserAccess"]] <- "groupMember"
  }

  portal_search(
    c("sharing", "rest", "community", "groups"),
    params,
    page_size,
    max_pages,
    .progress,
    host,
    token
  )
}

#' @export
#' @rdname search_groups
search_users <- function(
  query = NULL,
  filter = NULL,
  sort_field = NULL,
  sort_order = NULL,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  params <- search_params(
    query,
    filter,
    sort_field,
    sort_order,
    c("username", "fullname", "created", "modified")
  )

  portal_search(
    c("sharing", "rest", "community", "users"),
    params,
    page_size,
    max_pages,
    .progress,
    host,
    token
  )
}

#' @export
#' @rdname search_groups
search_group_content <- function(
  group,
  query = NULL,
  filter = NULL,
  categories = NULL,
  sort_field = NULL,
  sort_order = NULL,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token()
) {
  id <- group_id(group)

  params <- search_params(
    query,
    filter,
    sort_field,
    sort_order,
    c(
      "title",
      "created",
      "type",
      "owner",
      "modified",
      "added",
      "avgrating",
      "numratings",
      "numcomments",
      "numviews"
    )
  )

  check_character(categories, allow_null = TRUE)

  if (!is.null(categories)) {
    if (length(categories) > 8) {
      cli::cli_abort("{.arg categories} must have 8 or fewer values.")
    }
    params[["categories"]] <- paste0(categories, collapse = ",")
  }

  portal_search(
    c("sharing", "rest", "content", "groups", id, "search"),
    params,
    page_size,
    max_pages,
    .progress,
    host,
    token
  )
}

search_params <- function(
  query,
  filter,
  sort_field,
  sort_order,
  sort_fields,
  call = rlang::caller_env()
) {
  check_string(query, allow_null = TRUE, allow_empty = FALSE, call = call)
  check_string(filter, allow_null = TRUE, allow_empty = FALSE, call = call)
  check_string(sort_field, allow_null = TRUE, call = call)

  if (is.null(query) && is.null(filter)) {
    cli::cli_abort("Supply {.arg query}, {.arg filter}, or both.", call = call)
  }

  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
      sort_fields,
      error_arg = "sort_field",
      error_call = call
    )
  }

  if (!is.null(sort_order)) {
    sort_order <- enum_arg(SortOrder, sort_order, call = call)
  }

  compact(list(
    q = query,
    filter = filter,
    sortField = sort_field,
    sortOrder = sort_order
  ))
}

portal_search <- function(
  path,
  params,
  page_size,
  max_pages,
  .progress,
  host,
  token
) {
  resps <- arc_base_req(host, token, path = path, query = c(f = "json")) |>
    httr2::req_url_query(!!!params) |>
    arc_paginate_req(
      page_size = page_size,
      max_pages = max_pages,
      .progress = .progress
    )

  # the user search reference names the array both `results` and `users`
  results <- lapply(resps, function(.resp) {
    res <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors()

    res[["results"]] %||% res[["users"]]
  })

  if (all(vapply(results, rlang::is_empty, logical(1)))) {
    return(data_frame(data.frame()))
  }

  parse_portal_dates(data_frame(rbind_results(results)))
}

parse_portal_dates <- function(x) {
  for (col in c("created", "modified", "lastViewed", "lastLogin")) {
    if (!is.null(x[[col]])) {
      x[[col]] <- from_esri_date(x[[col]])
    }
  }
  x
}

group_id <- function(group, call = rlang::caller_env()) {
  id <- if (inherits(group, "PortalGroup")) group[["id"]] else group

  if (!rlang::is_string(id) || !nzchar(id)) {
    cli::cli_abort(
      "{.arg group} must be a group ID or a {.cls PortalGroup}.",
      call = call
    )
  }

  id
}
