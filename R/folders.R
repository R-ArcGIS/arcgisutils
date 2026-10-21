#' Create a Portal Folder
#'
#' `arc_create_folder()` creates a folder in the signed in user's content.
#'
#' @param title a scalar character. The name of the new folder. The portal
#'   rejects a title that another of the user's folders already uses, and
#'   `arc_create_folder()` errors when the title exceeds 250 characters.
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Pass `folder$id` from the result to the `folder` argument of
#' [upload_file()].
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/create-folder/)
#' @export
#' @family portal folder
#' @returns A list with the elements `success` and `folder`, a list with the
#'   elements `username`, `id`, and `title`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' folder <- arc_create_folder("Penguins")
#' upload_file("penguins.csv", "Penguins", folder = folder$folder$id)
#' }
arc_create_folder <- function(
  title,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(title, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  if (nchar(title) > 250L) {
    cli::cli_abort(
      "{.arg title} must be 250 characters or fewer, not {nchar(title)}.",
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
      token[["username"]],
      "createFolder"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json", title = title) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Delete a Portal Folder
#'
#' `arc_delete_folder()` deletes a folder from the signed in user's content.
#'
#' @param folder a scalar character folder ID. Pass the `id` of a folder from
#'   [arc_create_folder()].
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `arc_delete_folder()` also deletes every item in the folder. The portal refuses
#' to delete the root folder.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-folder/)
#' @export
#' @family portal folder
#' @returns A list with the elements `success` and `folder`, a list with the
#'   elements `username`, `id`, and `title`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' folder <- arc_create_folder("Scratch")
#' arc_delete_folder(folder$folder$id)
#' }
arc_delete_folder <- function(
  folder,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(folder, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      token[["username"]],
      folder,
      "delete"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Rename a Portal Folder
#'
#' `arc_rename_folder()` renames a non-root folder in the signed in user's content.
#'
#' @param title a scalar character. The new name for the folder.
#' @inheritParams arc_delete_folder
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal compares titles without regard to case and refuses a title that
#' another of the user's folders already uses.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/update-folder/)
#' @export
#' @family portal folder
#' @returns A list with the element `success`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' folder <- arc_create_folder("Scratch")
#' arc_rename_folder(folder$folder$id, "Archive")
#' }
arc_rename_folder <- function(
  folder,
  title,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(folder, allow_empty = FALSE, call = error_call)
  check_string(title, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      token[["username"]],
      folder,
      "updateFolder"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json", newTitle = title) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' List the Items in a Portal Folder
#'
#' `arc_folder()` lists the items in a folder in the signed in user's
#' content.
#'
#' @inheritParams arc_delete_folder
#' @inheritParams arc_paginate_req
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal lists a folder's items only for its owner or an organization
#' administrator. `arc_user_content()` lists the items in the root folder.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/folder/)
#' @export
#' @family portal folder
#' @returns A `data.frame` with one row per item in the folder.
#'   `arc_folder()` converts the `created`, `modified`, and
#'   `lastViewed` columns to date-times.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' folder <- arc_create_folder("Scratch")
#' arc_folder(folder$folder$id)
#' }
arc_folder <- function(
  folder,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_string(folder, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  fetch_content(
    c("sharing", "rest", "content", "users", token[["username"]], folder),
    page_size,
    max_pages,
    .progress,
    token[["arcgis_host"]],
    token
  )
}
