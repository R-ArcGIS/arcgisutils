#' Portal Item Relationships
#'
#' Fetches items related to a portal item, such as the source data behind a
#' hosted feature service or the maps that consume it.
#'
#' @param item a `PortalItem` from [`arc_item()`] or a scalar character item ID.
#' @param relationship_type default `NULL`, returning every relationship type.
#'   Otherwise one of [`RelationshipType`].
#' @param direction one of [`RelationshipDirection`]. `"forward"` returns
#'   destinations of the relationship, `"reverse"` returns origins.
#' @inheritParams arc_item
#' @details
#'
#' The REST documentation lists the type filter as a required parameter named
#' `relationshipTypes`. It is actually named `relationshipType` and is optional.
#'
#' See [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/related-items/) for more.
#'
#' `r lifecycle::badge("experimental")`
#' @export
#' @family portal item
#' @examplesIf curl::has_internet()
#' arc_related_items("9df5e769bfe8412b8de36a2e618c7672")
#' @returns a `data.frame` of related item metadata. Zero rows if there are none.
arc_related_items <- function(
  item,
  relationship_type = NULL,
  direction = "forward",
  ...,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  id <- item_id(item)

  query <- list(
    f = "json",
    direction = enum_arg(RelationshipDirection, direction)
  )

  if (!is.null(relationship_type)) {
    query[["relationshipType"]] <- enum_arg(
      RelationshipType,
      relationship_type
    )
  }

  resp <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "items", id, "relatedItems"),
    query = query
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  related <- resp[["relatedItems"]]

  if (rlang::is_empty(related)) {
    return(data_frame(data.frame()))
  }

  res <- data_frame(related)

  for (field in c("created", "modified", "lastViewed")) {
    if (!is.null(res[[field]])) {
      res[[field]] <- from_esri_date(res[[field]])
    }
  }

  res
}

item_id <- function(item, call = rlang::caller_env()) {
  id <- if (inherits(item, "PortalItem")) item[["id"]] else item

  if (!rlang::is_string(id) || !nzchar(id)) {
    cli::cli_abort(
      "{.arg item} must be an item ID or a {.cls PortalItem}.",
      call = call
    )
  }

  id
}
