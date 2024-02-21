#' Validate CRS object
#'
#'
#' Takes a representation of a CRS and ensures that it is a valid one. The CRS
#' is validated using `sf::st_crs()` if it cannot be validated, a null CRS is returned.
#'
#' See [`sf::st_crs()`] for more details on valid representations.
#'
#' @param crs a representation of a coordinate reference system.
#' @inheritParams cli::cli_abort
#' @inheritParams rlang::caller_arg
#'
#' @export
#' @examples
#'
#' # using epsg code integer or string representation
#' validate_crs(3857)
#' validate_crs("EPSG:4326")
#'
#' # using a custom proj4 string
#' proj4string <- "+proj=longlat +datum=WGS84 +no_defs"
#'
#' crs <- validate_crs(proj4string)
#'
#' # using wkt2 (from above result)
#' crs <- validate_crs(crs$spatialReference$wkt)
#'
#' @returns
#'
#' Returns a list of length 1 with an element named `spatialReference` which is itself
#' a named list.
#'
#' If the provided CRS returns a valid well-known ID (WKID) `spatialReference` contains
#' a named element called `wkid` which is the integer value of the WKID. If the WKID
#' is not known but the CRS returned is a valid well-known text representation the `wkid` field
#' is `NA` and another field `wkt` contains the valid wkt.
#'
#'
validate_crs <- function(crs, arg = rlang::caller_arg(crs), call = rlang::caller_env()) {

  if (!((inherits(crs, "character") || inherits(crs, "crs") || inherits(crs, "numeric")))) {
    cli::cli_abort(c(
      "Invalid CRS supplied: {.arg {arg}}",
      "i" = "must be compatible with {.fn sf::st_crs}",
      arg = arg,
      call = call
    ))
  }

  crs <- sf::st_crs(crs)

  # extract SRID
  srid <- crs$srid

  if (!is.null(srid) && !is.na(srid)) {
    wkid <- as.integer(strsplit(srid, ":")[[1]][2])
    wkt <- NULL
  } else {
    wkid <- NULL
    wkt <- crs$wkt
  }


  sr_components <- compact(list(wkid = wkid, wkt = wkt))
  list(spatialReference = sr_components)
}


