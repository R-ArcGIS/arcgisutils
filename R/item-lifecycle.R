#' Update a Portal Item
#'
#' `update_item()` changes an item's metadata, replaces its file, or both.
#'
#' @param path optional. A scalar character path to a local file that
#'   replaces the item's file.
#' @param title optional. A scalar character. The new title of the item.
#' @param type_keywords optional. A character vector of type keywords that
#'   replaces the item's type keywords.
#' @param thumbnail optional. A scalar character path to a local PNG, GIF, or
#'   JPEG file, or the URL of one. The portal limits a thumbnail file to 10 MB
#'   and a thumbnail URL to 1 MB.
#' @param metadata optional. A scalar character path to a local metadata file,
#'   such as `metadata.xml`.
#' @param metadata_editable optional. A scalar logical. When `TRUE`, members of
#'   the organization can edit the item's metadata on its item page.
#' @param metadata_format optional. One of [`MetadataFormat`]: `"fgdc"`,
#'   `"inspire"`, `"iso19139"`, `"iso19139-3.2"`, `"iso19115"`, or `"dcplus"`.
#' @param extent optional. The item's extent. Anything [`as_bbox()`] accepts.
#'   `update_item()` reprojects it to WGS84.
#' @param spatial_reference optional. The item's coordinate system. Anything
#'   [`validate_crs()`] accepts that has a well-known ID.
#' @param access_information optional. A scalar character crediting the
#'   item's source.
#' @param license_info optional. A scalar character describing the item's
#'   license or restrictions.
#' @param culture optional. A scalar character locale code, such as `"en-us"`
#'   or `"en"`.
#' @param classification optional. A named list. The item's classification
#'   marking. It must include `banner` and match the organization's
#'   classification schema. ArcGIS Enterprise 11.4 and later support it.
#' @param properties optional. A named list of item properties, such as a
#'   notebook's runtime.
#' @param text optional. A named list. The portal stores it as the item's JSON
#'   data.
#' @param url optional. A scalar character. The item's URL.
#' @param data_url optional. A scalar character URL of an unsecured file. The
#'   portal downloads the file in the background and stores it as the item's
#'   file.
#' @param categories optional. A character vector of full category paths, such
#'   as `"/Categories/Water"`, that the portal adds to the item.
#' @param app_categories,industries,languages optional. Character vectors of
#'   the categories, industries, and languages of an application item.
#' @param large_thumbnail,banner optional. A scalar character path to a local
#'   image file, or the URL of one, for an application item.
#' @param screenshots optional. A character vector of paths to local image
#'   files, or URLs of them, for an application item.
#' @param is_viewer_copiable optional. A scalar logical. When `TRUE`, anyone
#'   outside the organization can copy the item when it is shared with the
#'   organization or the public.
#' @param clear_empty_fields optional. A scalar logical. When `TRUE`, the
#'   portal clears each field you supply as an empty string.
#' @param async optional. A scalar logical. When `TRUE`, the portal replaces
#'   the item's file in the background. Check its progress with
#'   [`item_status()`].
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
  thumbnail = NULL,
  metadata = NULL,
  metadata_editable = NULL,
  metadata_format = NULL,
  extent = NULL,
  spatial_reference = NULL,
  access_information = NULL,
  license_info = NULL,
  culture = NULL,
  classification = NULL,
  properties = NULL,
  text = NULL,
  url = NULL,
  data_url = NULL,
  categories = NULL,
  app_categories = NULL,
  industries = NULL,
  languages = NULL,
  large_thumbnail = NULL,
  banner = NULL,
  screenshots = NULL,
  is_viewer_copiable = NULL,
  clear_empty_fields = NULL,
  async = NULL,
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
  check_string(
    thumbnail,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_string(
    metadata,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_bool(metadata_editable, allow_null = TRUE, call = error_call)
  check_string(access_information, allow_null = TRUE, call = error_call)
  check_string(license_info, allow_null = TRUE, call = error_call)
  check_string(culture, allow_null = TRUE, call = error_call)
  check_string(url, allow_null = TRUE, call = error_call)
  check_string(
    data_url,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_character(categories, allow_null = TRUE, call = error_call)
  check_character(app_categories, allow_null = TRUE, call = error_call)
  check_character(industries, allow_null = TRUE, call = error_call)
  check_character(languages, allow_null = TRUE, call = error_call)
  check_string(
    large_thumbnail,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_string(
    banner,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_character(screenshots, allow_null = TRUE, call = error_call)
  check_bool(is_viewer_copiable, allow_null = TRUE, call = error_call)
  check_bool(clear_empty_fields, allow_null = TRUE, call = error_call)
  check_bool(async, allow_null = TRUE, call = error_call)

  if (
    !is.null(classification) &&
      !(rlang::is_bare_list(classification) && rlang::is_named(classification))
  ) {
    cli::cli_abort(
      "{.arg classification} must be a named list.",
      call = error_call
    )
  }

  if (
    !is.null(properties) &&
      !(rlang::is_bare_list(properties) && rlang::is_named(properties))
  ) {
    cli::cli_abort("{.arg properties} must be a named list.", call = error_call)
  }

  if (!is.null(text) && !(rlang::is_bare_list(text) && rlang::is_named(text))) {
    cli::cli_abort("{.arg text} must be a named list.", call = error_call)
  }

  wkid <- NULL

  if (!is.null(spatial_reference)) {
    wkid <- validate_crs(spatial_reference, call = error_call)[[
      "spatialReference"
    ]][["wkid"]]

    if (is.null(wkid)) {
      cli::cli_abort(
        "{.arg spatial_reference} must have a well-known ID.",
        call = error_call
      )
    }
  }

  local_files <- c(
    path,
    metadata,
    Filter(
      function(x) !is_url(x),
      c(thumbnail, large_thumbnail, banner, screenshots)
    )
  )
  missing_files <- local_files[!file.exists(local_files)]

  if (length(missing_files) > 0L) {
    cli::cli_abort(
      "{.file {missing_files}} {?does/do} not exist.",
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
    },
    thumbnail = if (!is.null(thumbnail) && !is_url(thumbnail)) {
      curl::form_file(thumbnail)
    },
    thumbnailurl = if (!is.null(thumbnail) && is_url(thumbnail)) {
      thumbnail
    },
    metadata = if (!is.null(metadata)) {
      curl::form_file(metadata)
    },
    metadataEditable = tolower(metadata_editable),
    metadataFormats = if (!is.null(metadata_format)) {
      MetadataFormat(metadata_format)@value
    },
    extent = if (!is.null(extent)) {
      paste0(
        unlist(as_bbox(extent, crs = 4326, error_call = error_call)),
        collapse = ","
      )
    },
    spatialReference = as.character(wkid),
    accessInformation = access_information,
    licenseInfo = license_info,
    culture = culture,
    classification = if (!is.null(classification)) {
      yyjsonr::write_json_str(classification, auto_unbox = TRUE)
    },
    properties = if (!is.null(properties)) {
      yyjsonr::write_json_str(properties, auto_unbox = TRUE)
    },
    text = if (!is.null(text)) {
      yyjsonr::write_json_str(text, auto_unbox = TRUE)
    },
    url = url,
    dataUrl = data_url,
    categories = if (!is.null(categories)) {
      yyjsonr::write_json_str(categories)
    },
    appCategories = if (!is.null(app_categories)) {
      yyjsonr::write_json_str(app_categories)
    },
    industries = if (!is.null(industries)) {
      yyjsonr::write_json_str(industries)
    },
    languages = if (!is.null(languages)) {
      yyjsonr::write_json_str(languages)
    },
    largeThumbnail = if (!is.null(large_thumbnail)) {
      if (is_url(large_thumbnail)) {
        large_thumbnail
      } else {
        curl::form_file(large_thumbnail)
      }
    },
    banner = if (!is.null(banner)) {
      if (is_url(banner)) {
        banner
      } else {
        curl::form_file(banner)
      }
    },
    isViewerCopiable = tolower(is_viewer_copiable),
    clearEmptyFields = tolower(clear_empty_fields),
    async = tolower(async)
  ))

  body <- c(
    body,
    rlang::set_names(
      lapply(screenshots, function(x) {
        if (is_url(x)) {
          x
        } else {
          curl::form_file(x)
        }
      }),
      rep("screenshot", length(screenshots))
    )
  )

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
  check_string(
    folder,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
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
