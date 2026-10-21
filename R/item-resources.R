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
#'
#' The portal returns the `access` column only to the item owner and
#' organization administrators.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/item-resources/)
#' @export
#' @family portal item
#' @returns A `data.frame` with one row per resource. `arc_item_resources()`
#'   converts the `created` column to date-times. Zero rows if there are none.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  id <- item_id(item, call = error_call)
  obj_check_token(token, call = error_call)

  req <- arc_base_req(
    token[["arcgis_host"]],
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

  res
}
