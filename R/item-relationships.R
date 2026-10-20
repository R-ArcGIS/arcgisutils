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
#' @inheritParams share_item
#' @inheritParams upload_file
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
#' @returns A list with the element `success`.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(origin, user, call = error_call)
  destination <- item_id(destination, call = error_call)
  relationship_type <- RelationshipType(relationship_type)@value

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "addRelationship"
    ),
    error_call = error_call
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
    detect_errors(error_call = error_call)
}

#' @export
#' @rdname add_relationship
delete_relationship <- function(
  origin,
  destination,
  relationship_type,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(origin, user, call = error_call)
  destination <- item_id(destination, call = error_call)
  relationship_type <- RelationshipType(relationship_type)@value

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "deleteRelationship"
    ),
    error_call = error_call
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
    detect_errors(error_call = error_call)
}

#' Set an Item's Content Status
#'
#' `set_content_status()` marks an item as authoritative or deprecated, or
#' clears its status.
#'
#' @param status one of [`ContentStatus`]: `"org_authoritative"`,
#'   `"public_authoritative"`, `"deprecated"`, or `"none"`. `"none"` clears
#'   the status.
#' @inheritParams share_item
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Setting `"public_authoritative"` requires an organization that is public
#' and verified by Esri. Only administrators can mark an item as
#' authoritative.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/set-content-status/)
#' @export
#' @family portal item
#' @returns A list with the elements `success` and `itemId`.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  id <- item_id(item, call = error_call)
  status <- ContentStatus(status)@value

  body <- if (status == "none") {
    list(status = "", clearEmptyFields = "true")
  } else {
    list(status = status)
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c("sharing", "rest", "content", "items", id, "setContentStatus"),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json", !!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Assign Content Categories to Items
#'
#' `set_item_categories()` replaces the organization content categories of up
#' to 100 items.
#'
#' @param categories a character vector of full category paths, such as
#'   `"/Categories/Water"`. `character(0)` removes every category from
#'   `items`.
#' @inheritParams delete_items
#' @inheritParams share_item
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `set_item_categories()` gives every item in `items` the same `categories`
#' and replaces the categories it had. It warns about each item it could not
#' update.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  ids <- item_ids(items, call = error_call)
  check_character(categories, call = error_call)

  if (length(ids) > 100L) {
    cli::cli_abort(
      "{.arg items} must be 100 or fewer, not {length(ids)}.",
      call = error_call
    )
  }

  payload <- lapply(ids, function(id) {
    rlang::set_names(list(list(categories = I(categories))), id)
  })

  resp <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c("sharing", "rest", "content", "updateItems"),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      f = "json",
      items = yyjsonr::write_json_str(payload, auto_unbox = TRUE)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  res <- data_frame(resp[["results"]])
  failed <- res[!res[["success"]], "itemId", drop = TRUE]

  if (length(failed) > 0L) {
    cli::cli_warn("Could not update {.val {failed}}.")
  }

  res
}
