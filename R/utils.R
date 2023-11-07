#' General utility functions
#'
#' @details
#'
#' - `compact()` removes any `NULL` list elements
#' - `%||%` is a special pipe operator that returns `b` if `a` is `NULL`
#'
#' @examples
#'
#' # remove null elements
#' compact(list(a = NULL, b = 1))
#'
#' # if NULL return rhs
#' NULL %||% 123
#'
#' # if not NULL return lhs
#' 123 %||% NULL
#'
#' @param .x a list
#' @export
#' @rdname utilities
#' @returns
#' - `compact()` a list
#' - `%||%` the first non-null item or `NULL` if both are `NULL`
compact <- function(.x) Filter(length, .x)

#' @export
#' @rdname utilities
#' @param a an R object
#' @param b an R object
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @keywords internal
transpose <- function(.l, .names = NULL) {
  transpose_impl(.l, .names)
}

#' Convert an object to an extent
#'
#' Given an sf or sfc object create a list that represents the extent of the
#' object. The result of this function can be parsed directly into json using
#' `jsonify::to_json(x, unbox = TRUE)` or included into a list as the extent
#' component that will be eventually converted into json using the above function.
#'
#' @param x an sf or sfc object
#' @param crs the CRS of the object. Must be parsible by `sf::st_crs()`
#' @export
#' @examples
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' as_extent(nc)
#' @returns
#' An extent json object. Use `jsonify::to_json(x, unbox = TRUE)` to convert
#' to json.
as_extent <- function(x, crs = sf::st_crs(x)) {

  # if a Table (no spatial dimensions) return NULL
  if (inherits(x, "data.frame") && !inherits(x, "sf")) {
    return(NULL)
  }

  if (is.na(crs)) {
    crs <- NULL
  } else {
    crs <- validate_crs(crs)
  }

  bbox <- as.list(sf::st_bbox(x))
  compact(c(bbox, crs))

}
