#' @export
#' @name gp_params
#' @inheritParams arc_base_req
#' @examples
#' x <- list(
#'   xmin = -122.4195,
#'   ymin = 37.330219000000056,
#'   xmax = -122.030757,
#'   ymax = 37.77650360000007,
#'   spatialReference = list(wkid = 4326L, latestWkid = 4326L)
#' )
#'
#' from_envelope(x)
from_envelope <- function(x, error_call = rlang::caller_call()) {
  e_msg <- "Envelope must be a bare list with fields {.code xmin}, {.code ymin}, {.code xmax}, {.code ymax}, and {.code spatialReference}"

  if (!rlang::is_bare_list(x)) {
    cli::cli_abort(
      e_msg,
      call = error_call
    )
  }

  if (
    !all(names(x) %in% c("xmin", "ymin", "xmax", "ymax", "spatialReference"))
  ) {
    cli::cli_abort(
      e_msg,
      call = error_call
    )
  }

  crs <- from_spatial_reference(x$spatialReference, error_call)

  sf::st_bbox(
    unlist(x[c("xmin", "ymin", "xmax", "ymax")]),
    crs = crs
  )
}

#' Coerce to a bounding box
#'
#' Converts an extent to a [wk::rct()], optionally in a different CRS.
#'
#' @details
#'
#' Accepts a `wk_rct`, an `sf` `bbox`, a length four numeric
#' `c(xmin, ymin, xmax, ymax)`, or anything [wk::wk_bbox()] understands, such as
#' an `sf` or `sfc` object. `wk` carries a CRS but does not reproject, so when
#' `crs` differs from the extent's own the transform goes through `sf`.
#'
#' @param x An extent. See details.
#' @param crs Coordinate reference system to return. Anything [wk::wk_crs()]
#'   accepts. Used as the CRS when `x` carries none.
#' @inheritParams arc_base_req
#' @returns A `wk_rct`.
#' @export
#' @examples
#' as_bbox(c(-104, 35.6, -94.32, 41), crs = 4326)
as_bbox <- function(x, crs = NULL, error_call = rlang::caller_env()) {
  bbox <- if (inherits(x, "wk_rct")) {
    x
  } else if (inherits(x, "bbox")) {
    wk::as_rct(x)
  } else if (is.numeric(x)) {
    if (length(x) != 4L) {
      cli::cli_abort(
        "{.arg x} must be a {.cls wk_rct} or a length four numeric.",
        call = error_call
      )
    }

    wk::rct(x[[1L]], x[[2L]], x[[3L]], x[[4L]], crs = crs)
  } else {
    rlang::try_fetch(
      wk::wk_bbox(x),
      error = function(cnd) {
        cli::cli_abort(
          "{.arg x} must be a {.cls wk_rct} or a length four numeric.",
          call = error_call
        )
      }
    )
  }

  if (is.null(crs)) {
    return(bbox)
  }

  from <- wk::wk_crs(bbox)

  if (is.null(from)) {
    return(wk::wk_set_crs(bbox, crs))
  }

  if (wk::wk_crs_equal(from, crs)) {
    return(bbox)
  }

  wk::as_rct(sf::st_bbox(
    sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox)), sf::st_crs(crs))
  ))
}
