#' Search for Groups, Users, and Group Content
#'
#' `search_groups()` searches a portal for groups, `search_users()` searches
#' for users, and `search_group_content()` searches the items shared with a
#' single group.
#'
#' @param filter optional. A scalar character of exact-match terms in the form
#'   `field:"term"`, for example `owner:"jdoe"`.
#' @param sort_field optional. A scalar character naming the field to sort by.
#'   `search_groups()` accepts `"title"`, `"owner"`, `"created"`, and
#'   `"modified"`. `search_users()` accepts `"username"`, `"fullname"`,
#'   `"created"`, and `"modified"`. `search_group_content()` accepts the
#'   values listed in [`search_items()`] and `"added"`.
#' @param member_only default `FALSE`. When `TRUE`, `search_groups()` searches
#'   only the groups that the signed in user owns or belongs to.
#' @inheritParams search_items
#' @inheritParams content
#' @inheritParams upload_file
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
#' @returns a `data.frame` with one row per result.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_string(query, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(filter, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_bool(member_only, call = error_call)

  if (is.null(query) && is.null(filter)) {
    cli::cli_abort(
      "Supply {.arg query}, {.arg filter}, or both.",
      call = error_call
    )
  }

  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
      c("title", "owner", "created", "modified"),
      error_call = error_call
    )
  }

  if (!is.null(sort_order)) {
    sort_order <- SortOrder(sort_order)@value
  }

  req <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "community", "groups"),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_url_query(
      !!!compact(list(
        q = query,
        filter = filter,
        sortField = sort_field,
        sortOrder = sort_order,
        searchUserAccess = if (member_only) {
          "groupMember"
        }
      ))
    )

  all_resps <- arc_paginate_req(
    req,
    page_size = page_size,
    max_pages = max_pages,
    .progress = .progress
  )

  results <- lapply(all_resps, function(.resp) {
    res <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors(error_call = error_call)

    res[["results"]]
  })

  res <- data_frame(rbind_results(results))
  for (col in c("created", "modified")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_string(query, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(filter, allow_null = TRUE, allow_empty = FALSE, call = error_call)

  if (is.null(query) && is.null(filter)) {
    cli::cli_abort(
      "Supply {.arg query}, {.arg filter}, or both.",
      call = error_call
    )
  }

  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
      c("username", "fullname", "created", "modified"),
      error_call = error_call
    )
  }

  if (!is.null(sort_order)) {
    sort_order <- SortOrder(sort_order)@value
  }

  req <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "community", "users"),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_url_query(
      !!!compact(list(
        q = query,
        filter = filter,
        sortField = sort_field,
        sortOrder = sort_order
      ))
    )

  all_resps <- arc_paginate_req(
    req,
    page_size = page_size,
    max_pages = max_pages,
    .progress = .progress
  )

  results <- lapply(all_resps, function(.resp) {
    res <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors(error_call = error_call)

    res[["results"]]
  })

  res <- data_frame(rbind_results(results))
  for (col in c("created", "modified", "lastLogin")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  if (inherits(group, "PortalGroup")) {
    group <- group$id
  }

  check_string(group, allow_empty = FALSE, call = error_call)
  check_string(query, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(filter, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_character(categories, allow_null = TRUE, call = error_call)

  if (is.null(query) && is.null(filter)) {
    cli::cli_abort(
      "Supply {.arg query}, {.arg filter}, or both.",
      call = error_call
    )
  }

  if (length(categories) > 8) {
    cli::cli_abort(
      "{.arg categories} must have 8 or fewer values.",
      call = error_call
    )
  }

  if (!is.null(sort_field)) {
    sort_field <- rlang::arg_match(
      sort_field,
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
      ),
      error_call = error_call
    )
  }

  if (!is.null(sort_order)) {
    sort_order <- SortOrder(sort_order)@value
  }

  req <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "groups", group, "search"),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_url_query(
      !!!compact(list(
        q = query,
        filter = filter,
        categories = if (!is.null(categories)) {
          paste0(categories, collapse = ",")
        },
        sortField = sort_field,
        sortOrder = sort_order
      ))
    )

  all_resps <- arc_paginate_req(
    req,
    page_size = page_size,
    max_pages = max_pages,
    .progress = .progress
  )

  results <- lapply(all_resps, function(.resp) {
    res <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors(error_call = error_call)

    res[["results"]]
  })

  res <- data_frame(rbind_results(results))
  for (col in c("created", "modified", "lastViewed")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
}
