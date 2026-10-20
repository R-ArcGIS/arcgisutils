#' Update a Portal Item
#'
#' Changes an item's metadata, replaces its file, or both.
#'
#' @param path optional. A scalar character path to a local file that
#'   replaces the item's file.
#' @param title optional. A scalar character. The new title of the item.
#' @param tags optional. A character vector of tags. Replaces the item's tags.
#' @param description optional. A scalar character. The new description.
#' @param snippet optional. A scalar character. The new summary, up to 2048
#'   characters.
#' @param type_keywords optional. A character vector of type keywords.
#'   Replaces the item's type keywords.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `update_item()` changes only the fields you supply and leaves the rest of
#' the item as it is. Supply at least one field.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)

  check_string(path, allow_null = TRUE, allow_empty = FALSE)
  check_string(title, allow_null = TRUE, allow_empty = FALSE)
  check_character(tags, allow_null = TRUE)
  check_string(description, allow_null = TRUE)
  check_string(snippet, allow_null = TRUE)
  check_character(type_keywords, allow_null = TRUE)

  if (!is.null(path)) {
    check_file_exists(path)
  }

  body <- compact(list(
    file = if (!is.null(path)) curl::form_file(path),
    title = title,
    tags = if (!is.null(tags)) paste0(tags, collapse = ","),
    description = description,
    snippet = snippet,
    typeKeywords = if (!is.null(type_keywords)) {
      paste0(type_keywords, collapse = ",")
    }
  ))

  if (rlang::is_empty(body)) {
    cli::cli_abort("Supply at least one field to update.")
  }

  item_owner_post(parts, "update", body, host, token, multipart = TRUE)
  arc_item(parts$id, host = host, token = token)
}

#' Delete a Portal Item
#'
#' Deletes a single item. Use [`delete_items()`] to delete up to 100 of your
#' own items in one request.
#'
#' @inheritParams share_item
#' @inheritParams delete_items
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Unlike [`delete_items()`], `delete_item()` acts on the item's owner, so an
#' administrator can delete another member's item. The portal refuses to
#' delete a protected item. Call [`unprotect_item()`] first.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-item/)
#' @export
#' @family portal item
#' @returns The item ID, invisibly.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  check_bool(permanent)
  parts <- item_owner(item, user)

  resp <- item_owner_post(
    parts,
    "delete",
    list(permanentDelete = tolower(permanent)),
    host,
    token
  )

  invisible(resp[["itemId"]] %||% parts$id)
}

#' Protect a Portal Item
#'
#' `protect_item()` prevents anyone from deleting an item until
#' `unprotect_item()` removes the protection.
#'
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#' @references
#' - [Protect API Reference](https://developers.arcgis.com/rest/users-groups-and-items/protect/)
#' - [Unprotect API Reference](https://developers.arcgis.com/rest/users-groups-and-items/unprotect/)
#' @export
#' @family portal item
#' @returns The item ID, invisibly.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)
  item_owner_post(parts, "protect", list(), host, token)
  invisible(parts$id)
}

#' @export
#' @rdname protect_item
unprotect_item <- function(
  item,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)
  item_owner_post(parts, "unprotect", list(), host, token)
  invisible(parts$id)
}

#' Move Portal Items Between Folders
#'
#' Moves up to 100 items owned by the signed in user into a folder.
#'
#' @param folder a scalar character folder ID, or `"/"` for the root folder.
#' @inheritParams delete_items
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The function warns about each item it could not move.
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
  warn_failed_items(res, "move")
  res
}

#' Copy a Portal Item
#'
#' Creates a copy of an item in the signed in user's content.
#'
#' @param title optional. A scalar character. The title of the copy. Defaults
#'   to the original item's title.
#' @param tags optional. A character vector of tags for the copy.
#' @param folder optional. A scalar character folder ID for the copy. Defaults
#'   to the original item's folder.
#' @param include_resources default `FALSE`. When `TRUE`, also copies the
#'   item's public file resources.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)
  check_string(title, allow_null = TRUE, allow_empty = FALSE)
  check_character(tags, allow_null = TRUE)
  check_string(folder, allow_null = TRUE, allow_empty = FALSE)
  check_bool(include_resources)

  body <- compact(list(
    title = title,
    tags = if (!is.null(tags)) paste0(tags, collapse = ","),
    folder = folder,
    includeResources = tolower(include_resources)
  ))

  resp <- item_owner_post(parts, "copy", body, host, token)

  # the reference documents `itemId` but its example returns `id`
  arc_item(resp[["itemId"]] %||% resp[["id"]], host = host, token = token)
}

#' Reassign a Portal Item
#'
#' Transfers ownership of an item to another user. Requires administrator
#' privileges.
#'
#' @param target_user a scalar character. The username of the new owner.
#' @param target_folder default `"/"`, the new owner's root folder. A scalar
#'   character folder name. The portal creates the folder when it does not
#'   exist.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/reassign-item/)
#' @export
#' @family portal item
#' @returns The item ID, invisibly.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)
  check_string(target_user, allow_empty = FALSE)
  check_string(target_folder, allow_empty = FALSE)

  resp <- item_owner_post(
    parts,
    "reassign",
    list(targetUsername = target_user, targetFolderName = target_folder),
    host,
    token
  )

  invisible(resp[["itemId"]] %||% parts$id)
}

#' Update a Portal Item's Thumbnail
#'
#' Replaces an item's thumbnail with a local image.
#'
#' @param path a scalar character path to a PNG, JPEG, or GIF image. Esri
#'   recommends 600 by 400 pixels.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/update-thumbnail/)
#' @export
#' @family portal item
#' @returns The item ID, invisibly.
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
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)
  check_string(path, allow_empty = FALSE)
  check_file_exists(path)

  item_owner_post(
    parts,
    "updateThumbnail",
    list(file = curl::form_file(path)),
    host,
    token,
    multipart = TRUE
  )

  invisible(parts$id)
}

item_owner_post <- function(
  parts,
  operation,
  body,
  host,
  token,
  multipart = FALSE,
  call = rlang::caller_env()
) {
  req <- item_owner_req(parts, operation, host, token)

  req <- if (multipart) {
    httr2::req_body_multipart(req, f = "json", !!!body)
  } else {
    httr2::req_body_form(req, f = "json", !!!body)
  }

  req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = call)
}

check_file_exists <- function(
  path,
  arg = rlang::caller_arg(path),
  call = rlang::caller_env()
) {
  if (!file.exists(path)) {
    cli::cli_abort("{.arg {arg}} does not exist: {.file {path}}", call = call)
  }
  invisible(path)
}
