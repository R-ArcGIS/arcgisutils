#' Determine Esri Geometry type
#'
#' Takes an `sf` or `sfc` object and returns the appropriate Esri geometry type.
#'
#' Alternatively, if
#' #' ### Geometry type mapping
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
#'
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

as_extent <- function(x, crs = sf::st_crs(x)) {
  if (is.na(crs)) {
    crs <- NULL
  } else {
    crs <- validate_crs(crs)
  }

  bbox <- as.list(sf::st_bbox(x))
  compact(c(bbox, crs))

}
