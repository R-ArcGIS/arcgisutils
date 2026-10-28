#' List a Portal Item's Resources
#'
#' `arc_item_resources()` lists the files stored in an item's resources
#' folder, such as the Parquet files behind a `"Parquet Feature Layer"`.
#'
#' @inheritParams arc_related_items
#' @inheritParams arc_paginate_req
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/item-resources/)
#' @export
#' @family portal item
#' @returns A `data.frame` with one row per resource. `arc_item_resources()`
#'   converts the `created` and `expires` columns to date-times. Zero rows if
#'   there are none.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' arc_item_resources("fd6e506c77de46d5ad401528f74b5d5e")
#' }
arc_item_resources <- function(
  item,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  host = arc_host(),
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  id <- item_id(item, call = error_call)
  check_string(host, allow_empty = FALSE, call = error_call)

  req <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "items", id, "resources"),
    query = c(f = "json"),
    error_call = error_call
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

    res[["resources"]]
  })

  res <- data_frame(rbind_results(results))

  if (!is.null(res[["created"]])) {
    res[["created"]] <- from_esri_date(res[["created"]])
  }

  if (!is.null(res[["expires"]])) {
    res[["expires"]] <- from_esri_date(res[["expires"]])
  }

  res
}

#' Add a Resource to a Portal Item
#'
#' `arc_add_item_resource()` uploads a local file, or a string of text, to an
#' item's resources folder.
#'
#' @param file optional. A scalar character path to a local file. Supply
#'   `file` or `text`, not both.
#' @param text optional. A scalar character that the portal stores as a
#'   resource file. Requires `file_name`.
#' @param file_name optional. A scalar character name for the resource,
#'   including its extension, such as `"part-00000.parquet"`. By default the
#'   portal uses the name of `file`.
#' @param prefix optional. A scalar character folder path that the portal
#'   places the resource in, such as `"20260922T000626Z_6b2a"`.
#' @param archive default `FALSE`. When `TRUE`, the portal extracts an
#'   uploaded zip file into its folders and ignores `access`.
#' @param access default `"inherit"`. One of `"inherit"` or `"private"`.
#'   `"inherit"` gives the resource the same access as the item. `"private"`
#'   limits the resource to the item owner and administrators.
#' @inheritParams arc_related_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal adds the resource under the item owner's content, so the signed
#' in user does not need to own the item. `arc_add_item_resource()` fetches
#' the item with [`arc_item()`] to find its owner when `item` is an ID.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-resources/)
#' @export
#' @family portal item
#' @returns A list with the elements `success`, `itemId`, `owner`, and
#'   `folder`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' tf <- tempfile(fileext = ".parquet")
#' arrow::write_parquet(head(iris), tf)
#'
#' arc_add_item_resource(
#'   "fd6e506c77de46d5ad401528f74b5d5e",
#'   file = tf,
#'   file_name = "part-00001.parquet",
#'   prefix = "20260922T000626Z_6b2a"
#' )
#' }
arc_add_item_resource <- function(
  item,
  file = NULL,
  text = NULL,
  file_name = NULL,
  prefix = NULL,
  archive = FALSE,
  access = "inherit",
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_exclusive(file, text, .call = error_call)
  check_string(file, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(text, allow_null = TRUE, call = error_call)
  check_string(
    file_name,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_string(
    prefix,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_bool(archive, call = error_call)
  access <- rlang::arg_match0(
    access,
    c("inherit", "private"),
    error_call = error_call
  )
  obj_check_token(token, call = error_call)

  if (!is.null(file) && !file.exists(file)) {
    cli::cli_abort(
      "{.arg file} does not exist: {.file {file}}",
      call = error_call
    )
  }

  if (!is.null(text) && is.null(file_name)) {
    cli::cli_abort(
      "{.arg file_name} is required when {.arg text} is supplied.",
      call = error_call
    )
  }

  if (!inherits(item, "PortalItem")) {
    item <- arc_item(
      item_id(item, call = error_call),
      host = token[["arcgis_host"]],
      token = token
    )
  }

  body <- compact(list(
    file = if (!is.null(file)) {
      curl::form_file(file)
    },
    text = text,
    fileName = file_name,
    resourcesPrefix = prefix,
    archive = tolower(archive),
    access = access
  ))

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      item[["owner"]],
      "items",
      item[["id"]],
      "addResources"
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_body_multipart(!!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}
