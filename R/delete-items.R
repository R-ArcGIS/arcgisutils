#' Delete Portal Items
#'
#' Deletes up to 100 items owned by the signed in user.
#'
#' @param items A character vector of item IDs, a `PortalItem`, or a list of them.
#' @param permanent Boolean. Skip the recycle bin. ArcGIS Online only.
#' @param ... these dots are for future extensions and must be empty.
#' @inheritParams arc_base_req
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-items/)
#' @export
#' @family portal item
#' @returns A data.frame with columns `itemId` and `success`.
#' @examples
#' \dontrun{
#' item <- upload_file("penguins.parquet", "penguins")
#' delete_items(item)
#' }
delete_items <- function(
  items,
  permanent = FALSE,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_bool(permanent, call = error_call)
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
    path = paste0(
      "sharing/rest/content/users/",
      token[["username"]],
      "/deleteItems"
    ),
    query = c("f" = "json"),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      items = paste0(ids, collapse = ","),
      permanentDelete = tolower(permanent)
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()

  detect_errors(resp)

  res <- data_frame(resp[["results"]])
  warn_not_deleted(res)
  res
}

item_ids <- function(items, call = rlang::caller_env()) {
  if (inherits(items, "PortalItem")) {
    items <- list(items)
  }

  if (is.list(items)) {
    items <- vapply(
      items,
      function(x) x[["id"]] %||% NA_character_,
      character(1)
    )
  }

  check_character(items, call = call)

  if (length(items) == 0L || anyNA(items) || !all(nzchar(items))) {
    cli::cli_abort("{.arg items} must be item IDs.", call = call)
  }

  items
}

warn_not_deleted <- function(res) {
  failed <- res[!res[["success"]], "itemId"]

  if (length(failed) > 0L) {
    cli::cli_warn("Could not delete {.val {failed}}.")
  }

  invisible(res)
}
