#' Upload a Large File to a Portal
#'
#' Uploads a local file in parts and adds it to a portal as a new content
#' item. Use it for files too large for a single [`upload_file()`] request,
#' such as large parquet files, file geodatabases, and tile packages.
#'
#' @param part_size default `50`. The size of each part in megabytes. Must be
#'   at least 5, and the file must fit in 10,000 parts.
#' @param timeout default `Inf`. The maximum number of seconds to wait for
#'   the portal to assemble the parts.
#' @param .progress default `TRUE`. Whether to show a progress bar while
#'   uploading parts.
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `upload_large_file()` creates an empty item, uploads the file in parts,
#' and commits them. The portal then assembles the parts in the background,
#' and `upload_large_file()` waits for it with [`await_item()`] before it sets
#' the title, tags, description, and snippet.
#'
#' If any step fails, `upload_large_file()` deletes the partial item and
#' raises the error.
#' @references
#' - [Add Item API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-item/)
#' - [Add Part API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-item-part/)
#' - [Commit API Reference](https://developers.arcgis.com/rest/users-groups-and-items/commit/)
#' @export
#' @family portal item
#' @returns An object of class `PortalItem`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' upload_large_file("parcels.gdb.zip", "Parcels", type = "File Geodatabase")
#' }
upload_large_file <- function(
  path,
  title,
  type = NULL,
  tags = NULL,
  description = NULL,
  snippet = NULL,
  folder = NULL,
  part_size = 50,
  timeout = Inf,
  .progress = TRUE,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_string(path, allow_empty = FALSE, call = error_call)
  check_string(title, allow_empty = FALSE, call = error_call)
  check_character(tags, allow_null = TRUE, call = error_call)
  check_string(description, allow_null = TRUE, call = error_call)
  check_string(snippet, allow_null = TRUE, call = error_call)
  check_string(folder, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_number_decimal(part_size, min = 5, call = error_call)
  check_bool(.progress, call = error_call)
  check_token_has_user(token, call = error_call)
  check_file_exists(path, call = error_call)

  type <- item_type(type %||% infer_item_type(path, error_call = error_call))@value

  part_bytes <- floor(part_size * 1024^2)
  n_parts <- max(1, ceiling(file.size(path) / part_bytes))

  if (n_parts > 10000) {
    cli::cli_abort(
      c(
        "{.file {basename(path)}} needs {n_parts} parts, but the portal accepts at most 10,000.",
        "i" = "Increase {.arg part_size}."
      ),
      call = error_call
    )
  }

  user <- token[["username"]]
  host <- token[["arcgis_host"]]

  properties <- compact(list(
    title = title,
    type = type,
    tags = if (!is.null(tags)) paste0(tags, collapse = ","),
    description = description,
    snippet = snippet
  ))

  added <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "users", user, folder, "addItem"),
    error_call = error_call
  ) |>
    httr2::req_body_multipart(
      f = "json",
      multipart = "true",
      filename = basename(path),
      !!!properties
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  parts <- list(id = added[["id"]], owner = user)

  rlang::try_fetch(
    {
      upload_parts(path, parts, part_bytes, n_parts, host, token, .progress)
      item_owner_post(parts, "commit", properties, host, token, call = error_call)
      await_item(parts$id, timeout = timeout, user = user, host = host, token = token)
    },
    error = function(cnd) {
      try(delete_item(parts$id, user = user, host = host, token = token), silent = TRUE)
      cli::cli_abort(
        "Failed to upload {.file {basename(path)}}. Deleted the partial item.",
        parent = cnd,
        call = error_call
      )
    }
  )

  # the portal ignores item properties sent with a multipart addItem
  update_item(
    parts$id,
    title = title,
    tags = tags,
    description = description,
    snippet = snippet,
    user = user,
    host = host,
    token = token
  )
}

upload_parts <- function(path, parts, part_bytes, n_parts, host, token, .progress) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  chunk <- tempfile()
  on.exit(unlink(chunk), add = TRUE)

  if (.progress) {
    cli::cli_progress_bar("Uploading parts", total = n_parts)
  }

  for (part_num in seq_len(n_parts)) {
    writeBin(readBin(con, raw(), part_bytes), chunk)

    item_owner_post(
      parts,
      "addPart",
      list(file = curl::form_file(chunk), partNum = as.character(part_num)),
      host,
      token,
      multipart = TRUE
    )

    if (.progress) {
      cli::cli_progress_update()
    }
  }

  invisible(n_parts)
}
