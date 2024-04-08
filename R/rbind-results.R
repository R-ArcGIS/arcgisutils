#' Combine multiple data.frames
#'
#' A general function that takes a list of `data.frame`s and returns a single
#' and combines them into a single object. It will use the fastest method
#' available. In order this is [`collapse::rowbind()`], [`data.table::rbindlist()`],
#' [`vctrs::list_unchop()`], then `do.call(rbind.data.frame, x)`.
#'
#' If all items in the list are `data.frame`s, then the result will be a `data.frame`.
#' If all elements are an `sf` object, then the result will be an `sf` object.
#' If the items are mixed, the result will be a `data.frame`.
#'
#' If any items are `NULL`, then an attribute `null_elements` will be attached
#' to the result. The attribute is an integer vector of the indices that
#' were `NULL`.
#'
#' @param x a list where each element is a `data.frame` or `NULL`.
#' @param .ptype currently unused. Reserved for a future release.
#' @export
#' @returns see details.
#' @inheritParams parse_esri_json
#' @examples
#'
#' x <- head(iris)
#' res <- rbind_results(list(x, NULL, x))
#' attr(res, "null_elements")
rbind_results <- function(
    x,
    call = rlang::current_env(),
    .ptype = data.frame()) {
  # use for loop for side effects
  # check that each element is a data.frame
  for (item in x) {
    check_data_frame(
      item,
      allow_null = TRUE,
      call = call,
      arg = rlang::caller_arg(x)
    )
  }

  # check if all results are sf, if so, we must return sf
  return_sf <- all(vapply(x, inherits_or_null, logical(1), class = "sf"))

  # identify which results are missing
  missing_elements <- vapply(x, is.null, logical(1))
  are_missing <- which(missing_elements)

  if (all(missing_elements)) {
    # return empty data.frame() if all missing
    # FIXME should we take a ptype here? to
    return(structure(data.frame(), null_elements = are_missing))
  }

  if (rlang::is_installed("collapse", version = "2.0.0")) {
    x <- collapse::rowbind(x)
  } else if (rlang::is_installed("data.table")) {
    x <- data.table::rbindlist(x)
    data.table::setDF(x)
  } else if (rlang::is_installed("vctrs")) {
    # vctrs::vec_rbind() doesn't handle NULL
    x <- vctrs::list_unchop(x, error_call = call)
  } else {
    x <- do.call(rbind.data.frame, x)
  }

  # cast to sf if not already (the case with collapse)
  if (return_sf && !rlang::inherits_any(x, "sf")) {
    x <- sf::st_as_sf(x)
  }

  if (length(are_missing) > 0) {
    attr(x, "null_elements") <- are_missing
  }

  x
}

#' Check if an object is NULL or inherits a class
#'
#' Uses [`rlang::inherits_any()`] for the class check.
#' @keywords internal
#' @noRd
inherits_or_null <- function(x, class) {
  if (is.null(x)) {
    return(TRUE)
  } else {
    rlang::inherits_any(x, class)
  }
}
