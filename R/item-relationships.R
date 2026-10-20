#' Add or Delete Item Relationships
#'
#' `add_relationship()` creates a relationship from an origin item to a
#' destination item. `delete_relationship()` removes one.
#'
#' @param origin a `PortalItem` from [`arc_item()`] or a scalar character item
#'   ID. The origin of the relationship.
#' @param destination a `PortalItem` from [`arc_item()`] or a scalar character
#'   item ID. The destination of the relationship.
#' @param relationship_type one of [`RelationshipType`], such as
#'   `"Map2Service"`.
#' @param user the username that owns `origin`. Taken from `origin` when it is
#'   a `PortalItem`.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Only the owner of `origin` or an administrator can add or delete its
#' relationships. You can delete only the relationships you created.
#' [`arc_related_items()`] lists the relationships an item already has.
#' @references
#' - [Add Relationship API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-relationship/)
#' - [Delete Relationship API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-relationship/)
#' @export
#' @family portal item
#' @returns `TRUE`, invisibly.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' map <- arc_item("a40fddbcd3fc4fadbbb50485a50c3ed0")
#' add_relationship(map, "b512083cd1b64e2da1d3f66dbb135956", "Map2Service")
#' delete_relationship(map, "b512083cd1b64e2da1d3f66dbb135956", "Map2Service")
#' }
add_relationship <- function(
  origin,
  destination,
  relationship_type,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  relationship_req(
    "addRelationship",
    origin,
    destination,
    relationship_type,
    user,
    host,
    token
  )
}

#' @export
#' @rdname add_relationship
delete_relationship <- function(
  origin,
  destination,
  relationship_type,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  relationship_req(
    "deleteRelationship",
    origin,
    destination,
    relationship_type,
    user,
    host,
    token
  )
}

# the REST reference names the type parameter `relationshipTypes`, but
# the related items endpoint and the ArcGIS Python API both use the singular
relationship_req <- function(
  operation,
  origin,
  destination,
  relationship_type,
  user,
  host,
  token,
  call = rlang::caller_env()
) {
  parts <- item_owner(origin, user, call = call)
  destination <- item_id(destination, call = call)
  relationship_type <- enum_arg(
    RelationshipType,
    relationship_type,
    arg = "relationship_type",
    call = call
  )

  arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "users", parts$owner, operation),
    error_call = call
  ) |>
    httr2::req_body_form(
      f = "json",
      originItemId = parts$id,
      destinationItemId = destination,
      relationshipType = relationship_type
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = call)

  invisible(TRUE)
}

#' Set an Item's Content Status
#'
#' Marks an item as authoritative or deprecated, or clears its status.
#'
#' @param item a `PortalItem` from [`arc_item()`] or a scalar character item ID.
#' @param status one of [`ContentStatus`]: `"org_authoritative"`,
#'   `"public_authoritative"`, `"deprecated"`, or `"none"`. `"none"` clears
#'   the status.
#' @param ... these dots are for future extensions and must be empty.
#' @inheritParams arc_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Setting `"public_authoritative"` requires an organization that is public
#' and verified by Esri. Only administrators can mark an item as
#' authoritative.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/set-content-status/)
#' @export
#' @family portal item
#' @returns The item ID, invisibly.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' set_content_status("c20206e7efd94c65a643cfa3c004734e", "org_authoritative")
#' set_content_status("c20206e7efd94c65a643cfa3c004734e", "none")
#' }
set_content_status <- function(
  item,
  status,
  ...,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  id <- item_id(item)
  status <- enum_arg(ContentStatus, status)

  body <- if (status == "none") {
    list(status = "", clearEmptyFields = "true")
  } else {
    list(status = status)
  }

  resp <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "items", id, "setContentStatus")
  ) |>
    httr2::req_body_form(f = "json", !!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  invisible(resp[["itemId"]] %||% id)
}

#' Assign Content Categories to Items
#'
#' Replaces the organization content categories of up to 100 items.
#'
#' @param items a character vector of item IDs, a `PortalItem`, or a list of
#'   them. At most 100.
#' @param categories a character vector of full category paths, such as
#'   `"/Categories/Water"`. `character(0)` removes every category from
#'   `items`.
#' @inheritParams set_content_status
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Every item in `items` receives the same `categories`, replacing any
#' categories it had. The function warns about each item it could not update.
#' ArcGIS Enterprise allows at most 20 categories per item.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/update-items-with-content-categories/)
#' @export
#' @family portal item
#' @returns A `data.frame` with columns `itemId` and `success`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' set_item_categories(
#'   c("c20206e7efd94c65a643cfa3c004734e", "a40fddbcd3fc4fadbbb50485a50c3ed0"),
#'   c("/Categories/Geology", "/Categories/Elevation")
#' )
#' }
set_item_categories <- function(
  items,
  categories,
  ...,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  ids <- item_ids(items)
  check_character(categories)

  if (length(ids) > 100L) {
    cli::cli_abort("{.arg items} must be 100 or fewer, not {length(ids)}.")
  }

  payload <- lapply(ids, function(id) {
    rlang::set_names(list(list(categories = I(categories))), id)
  })

  resp <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "updateItems")
  ) |>
    httr2::req_body_form(
      f = "json",
      items = yyjsonr::write_json_str(payload, auto_unbox = TRUE)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  res <- data_frame(resp[["results"]])
  warn_failed_items(res, "update")
  res
}

warn_failed_items <- function(res, action) {
  failed <- res[!res[["success"]], "itemId", drop = TRUE]

  if (length(failed) > 0L) {
    cli::cli_warn("Could not {action} {.val {failed}}.")
  }

  invisible(res)
}
