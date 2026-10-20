#' Update a Portal Item
#'
#' `update_item()` changes an item's metadata, replaces its file, or both.
#'
#' @param path optional. A scalar character path to a local file that
#'   replaces the item's file.
#' @param title optional. A scalar character. The new title of the item.
#' @param type_keywords optional. A character vector of type keywords that
#'   replaces the item's type keywords.
#' @inheritParams upload_file
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `update_item()` changes only the fields you supply. You must supply at
#' least one field.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/update-item/)
#' @export
#' @family portal item
#' @returns The updated `PortalItem`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- upload_file("penguins.csv", "Penguins")
#' item <- update_item(item, title = "Palmer penguins", tags = "penguins")
#'
#' # replace the file
#' update_item(item, path = "penguins-2026.csv")
#' }
update_item <- function(
  item,
  path = NULL,
  title = NULL,
  tags = NULL,
  description = NULL,
  snippet = NULL,
  type_keywords = NULL,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)

  check_string(path, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(title, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_character(tags, allow_null = TRUE, call = error_call)
  check_string(description, allow_null = TRUE, call = error_call)
  check_string(snippet, allow_null = TRUE, call = error_call)
  check_character(type_keywords, allow_null = TRUE, call = error_call)

  if (!is.null(path) && !file.exists(path)) {
    cli::cli_abort(
      "{.arg path} does not exist: {.file {path}}",
      call = error_call
    )
  }

  body <- compact(list(
    file = if (!is.null(path)) {
      curl::form_file(path)
    },
    title = title,
    tags = if (!is.null(tags)) {
      paste0(tags, collapse = ",")
    },
    description = description,
    snippet = snippet,
    typeKeywords = if (!is.null(type_keywords)) {
      paste0(type_keywords, collapse = ",")
    }
  ))

  if (rlang::is_empty(body)) {
    cli::cli_abort("Supply at least one field to update.", call = error_call)
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "update"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_multipart(f = "json", !!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  arc_item(parts$id, host = token[["arcgis_host"]], token = token)
}

#' Delete a Portal Item
#'
#' `delete_item()` deletes a single item. [`delete_items()`] deletes up to 100
#' of your own items in one request.
#'
#' @inheritParams share_item
#' @inheritParams delete_items
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Unlike [`delete_items()`], `delete_item()` sends the request as the item's
#' owner, so an administrator can delete another member's item. The portal
#' refuses to delete a protected item. Call [`unprotect_item()`] first.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-item/)
#' @export
#' @family portal item
#' @returns A list with the elements `success` and `itemId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- upload_file("penguins.csv", "Penguins")
#' delete_item(item)
#' }
delete_item <- function(
  item,
  permanent = FALSE,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  check_bool(permanent, call = error_call)
  parts <- item_owner(item, user, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "delete"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json", permanentDelete = tolower(permanent)) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Protect a Portal Item
#'
#' `protect_item()` prevents anyone from deleting an item. `unprotect_item()`
#' removes the protection.
#'
#' @inheritParams share_item
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#' @references
#' - [Protect API Reference](https://developers.arcgis.com/rest/users-groups-and-items/protect/)
#' - [Unprotect API Reference](https://developers.arcgis.com/rest/users-groups-and-items/unprotect/)
#' @export
#' @family portal item
#' @returns A list with the element `success`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- arc_item("b512083cd1b64e2da1d3f66dbb135956")
#' protect_item(item)
#' unprotect_item(item)
#' }
protect_item <- function(
  item,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "protect"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' @export
#' @rdname protect_item
unprotect_item <- function(
  item,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "unprotect"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Move Portal Items Between Folders
#'
#' `move_items()` moves up to 100 of the signed in user's items into a folder.
#'
#' @param folder a scalar character folder ID, or `"/"` for the root folder.
#' @inheritParams delete_items
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `move_items()` warns about each item it could not move.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/move-items/)
#' @export
#' @family portal item
#' @returns A `data.frame` with columns `itemId` and `success`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- upload_file("penguins.csv", "Penguins")
#' move_items(item, "d1e7a2f0c9b84f3e8a1b2c3d4e5f6a7b")
#'
#' # back to the root folder
#' move_items(item, "/")
#' }
move_items <- function(
  items,
  folder,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(folder, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)
  ids <- item_ids(items, call = error_call)

  if (length(ids) > 100L) {
    cli::cli_abort(
      "{.arg items} must be 100 or fewer, not {length(ids)}.",
      call = error_call
    )
  }

  resp <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      token[["username"]],
      "moveItems"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      f = "json",
      items = paste0(ids, collapse = ","),
      folder = folder
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  res <- data_frame(resp[["results"]])
  failed <- res[!res[["success"]], "itemId", drop = TRUE]

  if (length(failed) > 0L) {
    cli::cli_warn("Could not move {.val {failed}}.")
  }

  res
}

#' Copy a Portal Item
#'
#' `copy_item()` copies an item into the signed in user's content.
#'
#' @param include_resources default `FALSE`. When `TRUE`, `copy_item()` also
#'   copies the item's public file resources.
#' @param folder optional. A scalar character folder ID for the copy. The
#'   portal uses the original item's folder by default.
#' @inheritParams upload_file
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/copy-item/)
#' @export
#' @family portal item
#' @returns The new `PortalItem`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- arc_item("b512083cd1b64e2da1d3f66dbb135956")
#' copy_item(item, title = "Copy of my map")
#' }
copy_item <- function(
  item,
  title = NULL,
  tags = NULL,
  folder = NULL,
  include_resources = FALSE,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)
  check_string(title, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_character(tags, allow_null = TRUE, call = error_call)
  check_string(folder, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_bool(include_resources, call = error_call)

  body <- compact(list(
    title = title,
    tags = if (!is.null(tags)) {
      paste0(tags, collapse = ",")
    },
    folder = folder,
    includeResources = tolower(include_resources)
  ))

  resp <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "copy"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json", !!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  arc_item(
    resp[["itemId"]] %||% resp[["id"]],
    host = token[["arcgis_host"]],
    token = token
  )
}

#' Reassign a Portal Item
#'
#' `reassign_item()` transfers ownership of an item to another user. You must
#' be an administrator.
#'
#' @param target_user a scalar character. The username of the new owner.
#' @param target_folder default `"/"`, the new owner's root folder. A scalar
#'   character folder name. The portal creates the folder when it does not
#'   exist.
#' @inheritParams share_item
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/reassign-item/)
#' @export
#' @family portal item
#' @returns A list with the elements `success` and `itemId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- arc_item("b512083cd1b64e2da1d3f66dbb135956")
#' reassign_item(item, "asmith", "Handover")
#' }
reassign_item <- function(
  item,
  target_user,
  target_folder = "/",
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)
  check_string(target_user, allow_empty = FALSE, call = error_call)
  check_string(target_folder, allow_empty = FALSE, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "reassign"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      f = "json",
      targetUsername = target_user,
      targetFolderName = target_folder
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Update a Portal Item's Thumbnail
#'
#' `update_thumbnail()` replaces an item's thumbnail with a local image.
#'
#' @inheritParams upload_file
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/update-thumbnail/)
#' @export
#' @family portal item
#' @returns A list with the element `success`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' item <- arc_item("b512083cd1b64e2da1d3f66dbb135956")
#' update_thumbnail(item, "thumbnail.png")
#' }
update_thumbnail <- function(
  item,
  path,
  ...,
  user = NULL,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  parts <- item_owner(item, user, call = error_call)
  check_string(path, allow_empty = FALSE, call = error_call)

  if (!file.exists(path)) {
    cli::cli_abort(
      "{.arg path} does not exist: {.file {path}}",
      call = error_call
    )
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "updateThumbnail"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_multipart(f = "json", file = curl::form_file(path)) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}
