#' Item Metadata
#'
#' Return the item's portal metadata.
#'
#' @param item_id the ID of the item to fetch. A scalar character.
#' @inherit source components
#' @details
#'
#' See [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/item/) for more information.
#'
#' @export
#' @family portal item
arc_item <- function(item_id, host = arc_host(), token = arc_token()) {
  check_string(item_id, allow_empty = FALSE)
  resp <- arc_base_req(
    host,
    path = paste0("sharing/rest/content/items/", item_id),
    token,
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  structure(resp, class = c("PortalItem", "list"))
}

#' @export
print.PortalItem <- function(x, ...) {
  cat(sprintf("<PortalItem<%s>>\n", x$type))
  for (field in c("id", "title", "owner")) {
    cat(field, ": ", x[[field]], "\n", sep = "")
  }
  invisible(x)
}

#' Organization's URLs
#'
#' Returns the URLs of an organizations services.
#'
#' See [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/urls/) for more information.
#' @export
#' @family portal
arc_portal_urls <- function(host = arc_host(), token = arc_token()) {
  arc_base_req(
    host,
    token,
    "sharing/rest/portals/self/urls",
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse()
}


#' Download an Item's Data
#'
#' Download the data backing a portal item. This function always returns
#' a raw vector as the type of the data that is downloaded cannot always be known.
#'
#' @param item the item ID or the result of `arc_item()`.
#' @export
#' @family portal item
arc_item_data <- function(item, host = arc_host(), token = arc_token()) {
  e_msg <- "Expected a content ID or {.cls PortalItem<_>} created with {.fn arc_item}"

  if (rlang::is_string(item)) {
    item <- rlang::try_fetch(
      arc_item(item, host, token),
      error = function(cnd) cli::cli_abort(e_msg)
    )
  }

  if (!inherits(item, "PortalItem")) {
    cli::cli_abort(e_msg)
  }

  arc_base_req(
    host,
    path = c("sharing/rest/content/items/", item_id, "data"),
    token
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_raw()
}


#' Fetch Group Information
#' @export
#' @family portal organization
arc_group <- function(group_id, host = arc_host(), token = arc_token()) {
  arc_base_req(
    host,
    token,
    c("sharing/rest/community/groups", group_id),
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()
}

#' Fetch User Information
#' @export
#' @family portal organization
arc_user <- function(username, host = arc_host(), token = arc_token()) {
  arc_base_req(
    host,
    token,
    c("sharing/rest/community/users", username),
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()
}
