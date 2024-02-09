#' Determine Esri Geometry type
#'
#' Takes an `sf` or `sfc` object and returns the appropriate Esri geometry type.
#'
#' ### Geometry type mapping
#'
#' - `POINT`:  `esriGeometryPoint`
#' - `MULTIPOINT`:  `esriGeometryMultipoint`
#' - `LINESTRING`:  `esriGeometryPolyline`
#' - `MULTILINESTRING`:  `esriGeometryPolyline`
#' - `POLYGON`:  `esriGeometryPolygon`
#' - `MULTIPOLYGON`:  `esriGeometryPolygon`
#'
#' @param x an object of class `data.frame`, `sf`, `sfc`, or `sfg`.
#' @returns returns a character scalar of the corresponding Esri geometry type
#' @export
#' @examples
#' determine_esri_geo_type(sf::st_point(c(0, 0)))
determine_esri_geo_type <- function(x) {

  # if `geom` is a data.frame return NULL
  if (inherits(x, "data.frame") && !inherits(x, "sf")) return(NULL)

  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))
  switch(
    geom_type,
    "POINT" = "esriGeometryPoint",
    "MULTIPOINT" = "esriGeometryMultipoint",
    "LINESTRING" = "esriGeometryPolyline",
    "MULTILINESTRING" = "esriGeometryPolyline",
    "POLYGON" = "esriGeometryPolygon",
    "MULTIPOLYGON" = "esriGeometryPolygon",
    stop("`", geom_type, "` is not a supported type")
  )
}

#' Convert an object to an extent
#'
#' Given an sf or sfc object create a list that represents the extent of the
#' object. The result of this function can be parsed directly into json using
#' `jsonify::to_json(x, unbox = TRUE)` or included into a list as the extent
#' component that will be eventually converted into json using the above function.
#'
#' @param x an sf or sfc object
#' @param crs the CRS of the object. Must be parsable by `sf::st_crs()`
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

