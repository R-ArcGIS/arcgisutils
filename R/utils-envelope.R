#' @export
#' @name gp_params
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
