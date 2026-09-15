#' Share a Portal Item
#'
#' Sets the access level of an item and optionally shares it with groups.
#'
#' @param item a `PortalItem` from [`arc_item()`] or a scalar character item ID.
#' @param access one of `"private"`, `"org"`, or `"public"`.
#' @param groups optional. A character vector of group IDs or a list of
#'   `PortalGroup` objects from [`arc_group()`].
#' @param confirm_item_control default `FALSE`. Set to `TRUE` to share with
#'   groups that have item update capability.
#' @param user the username that owns `item`. Taken from `item` when it is a
#'   `PortalItem`.
#' @param ... these dots are for future extensions and must be empty.
#' @inheritParams arc_item
#' @details
#'
#' The REST API expresses access as independent `everyone` and `org` flags.
#' `access` maps onto them so that invalid combinations cannot be expressed.
#'
#' Groups the item could not be shared with are reported as a warning.
#'
#' See [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/share-item-as-item-owner/) for more.
#'
#' `r lifecycle::badge("experimental")`
#' @export
#' @family portal item
#' @examples
#' \dontrun{
#' item <- arc_item("9df5e769bfe8412b8de36a2e618c7672")
#'
#' share_item(item, "org")
#' share_item(item, "private", groups = "4774c1c2b79046f285b2e86e5a20319e")
#' unshare_item(item)
#' }
#' @returns The item ID, invisibly.
share_item <- function(
  item,
  access = "private",
  groups = NULL,
  confirm_item_control = FALSE,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)

  body <- share_body(parts$id, access, groups, confirm_item_control)

  resp <- item_owner_req(parts, "share", host, token) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  warn_not_shared(resp)

  invisible(resp[["itemId"]] %||% parts$id)
}

#' @export
#' @rdname share_item
unshare_item <- function(
  item,
  groups = NULL,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, user)

  body <- list(f = "json")

  if (!is.null(groups)) {
    body[["groups"]] <- paste0(group_ids(groups), collapse = ",")
  }

  resp <- item_owner_req(parts, "unshare", host, token) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()

  invisible(resp[["itemId"]] %||% parts$id)
}

share_access <- function(access, call = rlang::caller_env()) {
  access <- enum_arg(ItemAccess, access, arg = "access", call = call)

  list(
    everyone = tolower(as.character(access == "public")),
    org = tolower(as.character(access == "org"))
  )
}

share_body <- function(
  id,
  access,
  groups,
  confirm_item_control,
  call = rlang::caller_env()
) {
  check_bool(confirm_item_control, call = call)

  body <- c(
    list(f = "json"),
    share_access(access, call = call),
    list(confirmItemControl = tolower(as.character(confirm_item_control)))
  )

  if (!is.null(groups)) {
    body[["groups"]] <- paste0(group_ids(groups, call = call), collapse = ",")
  }

  body
}

group_ids <- function(groups, call = rlang::caller_env()) {
  ids <- vapply(
    groups,
    function(g) if (inherits(g, "PortalGroup")) g[["id"]] else g,
    character(1)
  )

  check_character(ids, call = call)
  ids
}

warn_not_shared <- function(resp) {
  not_shared <- resp[["notSharedWith"]]

  if (rlang::is_empty(not_shared)) {
    return(invisible(NULL))
  }

  cli::cli_warn(
    "Item was not shared with {length(not_shared)} group{?s}: {.val {not_shared}}"
  )
}

item_owner <- function(item, user, call = rlang::caller_env()) {
  if (inherits(item, "PortalItem")) {
    id <- item[["id"]]
    owner <- user %||% item[["owner"]]
  } else {
    id <- item
    owner <- user
  }

  if (!rlang::is_string(id) || !nzchar(id)) {
    cli::cli_abort("{.arg item} must have an {.field id}.", call = call)
  }

  if (!rlang::is_string(owner) || !nzchar(owner)) {
    cli::cli_abort(
      c(
        "Can't determine the item {.field owner}.",
        "i" = "Supply {.arg user}, or pass a {.cls PortalItem} from {.fn arc_item}."
      ),
      call = call
    )
  }

  list(id = id, owner = owner)
}

item_owner_req <- function(parts, operation, host, token) {
  arc_base_req(
    host,
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      operation
    )
  )
}
