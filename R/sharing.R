#' Portal Item Metadata
#'
#' Given the unique ID of a content item, fetches the item's metadata from a portal.
#'
#' @param item_id the ID of the item to fetch. A scalar character.
#' @inheritParams auth_user
#' @details
#'
#' See [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/item/) for more information.
#'
#' `r lifecycle::badge("experimental")`
#' @export
#' @family portal item
#' @examplesIf curl::has_internet()
#' arc_item("9df5e769bfe8412b8de36a2e618c7672")
#' @returns an object of class `PortalItem` which is a list with the item's metadata.
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

  for (field in c("created", "modified", "lastViewed")) {
    resp[[field]] <- from_esri_date(resp[[field]])
  }

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
#' `r lifecycle::badge("experimental")`
#' @export
#' @family portal
#' @inheritParams auth_user
#' @examplesIf curl::has_internet()
#' arc_portal_urls()
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
#' `r lifecycle::badge("experimental")`
#' @param item the item ID or the result of `arc_item()`.
#' @export
#' @inheritParams auth_user
#' @family portal item
#' @examplesIf curl::has_internet()
#' arc_item_data("9df5e769bfe8412b8de36a2e618c7672")
#' @returns a raw vector containing the bytes of the data associated with the item. If the response is `application/json` then the json string is returned without parsing.
arc_item_data <- function(
  item,
  host = arc_host(),
  token = arc_token()
) {
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

  resp <- arc_base_req(
    host,
    path = c("sharing/rest/content/items/", item[["id"]], "data"),
    token
  ) |>
    httr2::req_perform()

  resp_type <- httr2::resp_content_type(resp)

  if (resp_type == "application/json") {
    resp_str <- httr2::resp_body_string(resp)
    catch_error(resp_str)
    resp_str
  } else {
    httr2::resp_body_raw(resp)
  }
}


#' Fetch Group Information
#'
#' Fetches metadata about a group based on a provided `group_id`.
#'
#' `r lifecycle::badge("experimental")`
#' @param group_id the unique group identifier. A scalar character.
#' @inheritParams arc_item
#' @export
#' @family portal organization
#' @examplesIf curl::has_internet()
#' arc_group("2f0ec8cb03574128bd673cefab106f39")
#' @returns a list with group metadata
arc_group <- function(
  group_id,
  host = arc_host(),
  token = arc_token()
) {
  check_string(group_id)
  group <- arc_base_req(
    host,
    token,
    c("sharing/rest/community/groups", group_id),
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  for (field in c("created", "modified")) {
    group[[field]] <- from_esri_date(group[[field]])
  }

  structure(group, class = c("PortalGroup", "list"))
}

#' @export
print.PortalGroup <- function(x, ...) {
  fields <- c("id", "owner", "created")
  cat(sprintf("<PortalGroup<%s>>\n", x[["title"]]))
  for (field in fields) {
    v <- x[[field]]

    if (is.null(v)) {
      next
    }

    if (!nzchar(v)) {
      next
    }

    cat(field, ": ", format(x[[field]]), "\n", sep = "")
  }
  invisible(x)
}

#' User Information
#'
#' Fetch a user's metadata based on username.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param username the username to fetch. A scalar character.
#' @inheritParams arc_item
#' @export
#' @family portal organization
#' @returns a list of class `PortalUser`
#' @examplesIf curl::has_internet()
#' arc_user("esri_en")
arc_user <- function(username, host = arc_host(), token = arc_token()) {
  user <- arc_base_req(
    host,
    token,
    c("sharing/rest/community/users", username),
    query = c("f" = "json")
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  for (field in c("created", "modified")) {
    user[[field]] <- from_esri_date(user[[field]])
  }

  structure(user, class = c("PortalUser", "list"))
}

#' @export
print.PortalUser <- function(x, ...) {
  cat(sprintf("<PortalUser<%s>>\n", x$username))
  for (field in c("id", "fullName", "created")) {
    v <- x[[field]]

    if (is.null(v)) {
      next
    }

    if (!nzchar(v)) {
      next
    }

    cat(field, ": ", format(x[[field]]), "\n", sep = "")
  }
  invisible(x)
}
