# **DEVELOPER NOTE**:
# FeatureSets have optionally an objectIdField and and fields properties.
# The current implementation does not have this. They have not yet been necessary.
# objectIdField _has been necessary_ in as_feature_collection(), however. If OID
# is needed in featureSets look to that function for logic.


#' Create Esri geometry objects
#'
#' These functions convert R objects to Esri json representations. There are three
#' types of representations. These are, from smallest to largest, a [geometry object](https://developers.arcgis.com/documentation/common-data-types/geometry-objects.htm), a [feature](https://developers.arcgis.com/documentation/common-data-types/feature-object.htm) and a [feature set](https://developers.arcgis.com/documentation/common-data-types/featureset-object.htm).
#'
#' @details
#'
#' The `_esri_` infix indicates that the input object will be converted directly into
#' the Esri JSON representation. If there is no `_esri_` infix, the object will be
#' converted into the appropriate list structure requiring only
#' `jsonify::to_json(x, unbox = TRUE)` to convert to Esri JSON.
#'
#' - `as_esri_geometry()` converts an `sfg` object to a geometry object
#' - `as_esri_features()` converts an `sfc` or `sf` object to a list of features
#' - `as_esri_featureset()` converts an `sf`, `sfc`, or `data.frame` to a feature set object
#'
#' Geometry object contain the coordinates for a geometry. Features are geometries that
#' have associated attributes with them. This would be similar to a row in an sf object.
#' FeatureSets are a list of features that have additional metadata fields such as
#' `spatialReference`, `geomtryType`, and `fields`. FeatureSets correspond to an
#' `sf` object.
#'
#' Geometry objects are defined for 5 different types. These are:
#'
#'  - Point: `esriGeometryPoint`
#'  - Multipoint: `esriGeometryMultipoint`
#'  - Polyline: `esriGeometryPolyline`
#'    - note that polyline encompasses both LINESTRING and MULTILINESTRING
#'  - Polygon: `esriGeometryPolygon`
#'    - note that polygon encompasses both POLYGON and MULTIPOLYGON
#'  - Envelope: `esriGeometryEnvelope`
#'    - envelopes correspond with bounding boxes but can have a Z dimension
#'
#' Field handling:
#'
#' - Vectors that inherit `Date` or `POSIXt` are converted into milliseconds since
#'   the Unix epoch in UTC timezone using `date_to_ms()`.
#' - `factor`s are converted to character vectors using `as.character()` to match
#'   the behavior in `infer_esri_type()` which defines type mapping for Esri
#'   field types and R vector classes.
#'
#'
#' @param x an object of class `sfg`
#' @param crs a CRS ID, crs object, or a well-known text representation of CRS
#' @examples
#' library(sf)
#' as_esri_geometry(st_point(c(0, 1, 3, 4)))
#' as_esri_geometry(st_multipoint(x = matrix(runif(4), ncol = 2)))
#' as_esri_geometry(st_linestring(x = matrix(runif(2), ncol = 2)))
#' as_esri_geometry(st_linestring(x = matrix(runif(2), ncol = 2)))
#'
#' # polygon
#' m <- matrix(
#'   c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 2, 2, 1, 2, 3, 1, 3, 2, 0, 0, 0),
#'   ncol = 3,
#'   byrow = TRUE
#' )
#' as_esri_geometry(st_polygon(list(m)))
#' @export
#' @rdname esri_geometry
#' @returns a json Esri geometry object
as_esri_geometry <- function(x, crs = 4326, ..., call = caller_env()) {
  unclass(jsonify::to_json(as_geometry(x, crs, ..., call = call), unbox = TRUE))
}


# #' @export
# as_esri_geometry.envelope <- function(x, crs = sf::st_crs(x), ...) {
#   crs_text <- validate_crs(crs)
#   jsonify::to_json(c(as.list(x), crs_text), unbox = TRUE)
#
# }




# Esri Features Array -----------------------------------------------------

#' @export
#' @rdname esri_geometry
as_esri_features <- function(x, ..., call = caller_env()) {
  unclass(jsonify::to_json(as_features(x, ..., call = call), unbox = TRUE))
}


# Esri FeatureSets --------------------------------------------------------

#' @param x an sf or sfc class object
#' @param crs the coordinate reference system of the FeatureSet. Must be interpretable by `sf::st_crs()`
#' @param ... additional arguments passed on to methods.
#'
#' @rdname esri_geometry
#' @export
as_esri_featureset <- function(x, ...) {
  unclass(jsonify::to_json(as_featureset(x, ...), unbox = TRUE))
}


# -------------------------------------------------------------------------

#' @export
#' @rdname esri_geometry
as_geometry <- function(x, crs, ...) {
  UseMethod("as_geometry")
}

#' @export
as_geometry.POINT <- function(x, crs = 4326, ..., call = caller_env()) {

  crs_text <- validate_crs(crs, error_call = call)

  dims <- determine_dims(x)

  geometry <- switch(
    dims,
    "xy" = sfc_point_xy(list(x))[[1]],
    "xyz" = sfc_point_xyz(list(x))[[1]],
    "xyzm" = sfc_point_xyzm(list(x))[[1]],
  )

  c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
}

#' @export
as_geometry.MULTIPOINT <- function(x, crs = 4326, ..., call = caller_env()) {
  crs_text <- validate_crs(crs, error_call = call)
  geometry <- sfc_multipoint_impl(list(x))[[1]]
  c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
}

#' @export
as_geometry.LINESTRING <- function(x, crs = 4326, ..., call = caller_env()) {
  crs_text <- validate_crs(crs, error_call = call)
  geometry <- sfc_linestring_impl(list(x))[[1]]
  c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
}

#' @export
as_geometry.MULTILINESTRING <- function(x, crs = 4326, ..., call = caller_env()) {
  crs_text <- validate_crs(crs, error_call = call)
  geometry <- sfc_multilinestring_impl(list(x))[[1]]

  c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
}

#' @export
as_geometry.POLYGON <- function(x, crs = 4326, ..., call = caller_env()) {
  crs_text <- validate_crs(crs, error_call = call)
  geometry <- sfg_polygon_impl(x)
  c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)

}

#' @export
as_geometry.MULTIPOLYGON <- function(x, crs = 4326, ..., call = caller_env()) {
  crs_text <- validate_crs(crs, error_call = call)
  geometry <- sfc_multipolygon_impl(list(x))[[1]]
  res <- c(hasZ = has_z(x), hasM = has_m(x), geometry, crs_text)
  res
}



# #' @export
# as__geometry.envelope <- function(x, crs = sf::st_crs(x), ...) {
#   crs_text <- validate_crs(crs)
#   jsonify::to_json(c(as.list(x), crs_text), unbox = TRUE)
#
# }




# Esri Features Array -----------------------------------------------------

#' @export
#' @rdname esri_geometry
as_features <- function(x, ...) {
  UseMethod("as_features")
}



#' @export
as_features.sfc <- function(x, ..., call = caller_env()) {

  geoms <- featureset_geometry(x, call = call)

  res <- lapply(
    geoms[[1]],
    function(.x) c(list(attributes = c()), geometry = list(.x))
  )

  res
}

#' @export
as_features.sf <- function(x, ..., call = caller_env()) {

  geo <- sf::st_geometry(x)
  geom_list <- featureset_geometry(geo, call = call)
  x <- sf::st_drop_geometry(x)

  # handle dates
  are_dates <- which(vapply(x, is_date, logical(1)))
  for (col in are_dates) {
    x[[col]] <- date_to_ms(x[[col]])
  }

  # handle factors
  are_factors <- which(vapply(x, is.factor, logical(1)))
  for (col in are_factors) {
    x[[col]] <- as.character(x[[col]])
  }


  # if there are no attributes
  if (nrow(x) == 0) {
    rows <- lapply(
      geom_list[[1]],
      function(.x) c(list(attributes = c()), geometry = list(.x))
    )
  } else {
    # if attributes extract the fields
    rows <- mapply(
      function(.x, .y) c(list(attributes = c(.y)), geometry = list(.x)),
      geom_list[[1]],
      transpose(x),
      SIMPLIFY = FALSE
    )

  }

  rows
}

#' @export
as_features.data.frame <- function(x, ...) {

  # handle dates
  are_dates <- which(vapply(x, is_date, logical(1)))
  for (col in are_dates) {
    x[[col]] <- date_to_ms(x[[col]])
  }

  # handle factor
  are_factors <- which(vapply(x, is.factor, logical(1)))
  for (col in are_factors) {
    x[[col]] <- as.character(x[[col]])
  }


  # listify the fields
  fields <- transpose(x)

  # iterate over them and make them fit esri json format
  rows <- lapply(fields, function(.x) list(attributes = .x))

  rows

}


# Esri FeatureSets --------------------------------------------------------

#' @export
#' @rdname esri_geometry
as_featureset <- function(x, ...) {
  UseMethod("as_featureset")
}



#' @export
as_featureset.sfc <- function(x, crs = sf::st_crs(x), ..., call = caller_env()) {

  # check CRS first
  # TODO have better CRS handling. We prefer having _no_ crs over
  # a wrong one.
  if (is.na(sf::st_crs(x)) && is.na(sf::st_crs(crs))) {
    warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }
  crs_text <- validate_crs(crs, error_call = call)

  geoms <- featureset_geometry(x, call = call)

  res <- lapply(
    geoms[[1]],
    function(.x) c(list(attributes = c()), geometry = list(.x))
  )

  c(
    geometryType = names(geoms),
    crs_text,
    hasZ = has_z(x),
    hasM = has_m(x),
    list(features = res)
  )
}

#' @export
as_featureset.sf <- function(x, crs = sf::st_crs(x), ..., call = caller_env()) {

  # check CRS first
  if (is.na(sf::st_crs(crs))) {
    warning("CRS missing. Setting to EPSG:4326")
    crs <- 4326
  }

  crs_text <- validate_crs(crs, error_call = call)

  geo <- sf::st_geometry(x)
  geom_list <- featureset_geometry(geo, call = call)
  x <- sf::st_drop_geometry(x)

  # handle dates
  are_dates <- which(vapply(x, is_date, logical(1)))
  for (col in are_dates) {
    x[[col]] <- date_to_ms(x[[col]])
  }

  # handle factors
  are_factors <- which(vapply(x, is.factor, logical(1)))
  for (col in are_factors) {
    x[[col]] <- as.character(x[[col]])
  }


  fields <- transpose(x)

  # if there are no attributes
  if (nrow(x) == 0) {
    rows <- lapply(
      geom_list[[1]],
      function(.x) c(list(attributes = c()), geometry = list(.x))
    )
  } else {
    # if attributes extract the fields
    rows <- mapply(
      function(.x, .y) c(list(attributes = c(.y)), geometry = list(.x)),
      geom_list[[1]],
      fields,
      SIMPLIFY = FALSE
    )

  }

  c(
    geometryType = names(geom_list),
    crs_text,
    hasZ = has_z(geo),
    hasM = has_m(geo),
    list(features = rows)
  )
}

#' @export
as_featureset.data.frame <- function(x, ...) {

  # handle dates
  are_dates <- which(vapply(x, is_date, logical(1)))
  for (col in are_dates) {
    x[[col]] <- date_to_ms(x[[col]])
  }

  # handle factor
  are_factors <- which(vapply(x, is.factor, logical(1)))
  for (col in are_factors) {
    x[[col]] <- as.character(x[[col]])
  }

  fields <- transpose(x)
  rows <- lapply(fields, function(.x) list(attributes = .x))
  c(list(features = rows))

}



# featureset geometry helper
#' Convert an object to featureset list structure
#'
#' The output of this is intended to be passed to `jsonify::to_json()`
#'
#' @param x an object of class `sfc` or `sf`
#' @keywords internal
#' @noRd
featureset_geometry <- function(x, call = caller_env()) {
  # extract geometry
  x <- sf::st_geometry(x)

  # get class of geometry
  geom_type <- as.character(sf::st_geometry_type(x, by_geometry = FALSE))

  # identify geometry type
  # TODO this duplicates the above call..maybe this can be simplified
  esri_geo_type <- determine_esri_geo_type(x, call = call)

  # error out if not one of the 6 types above
  if (is.null(esri_geo_type)) {
    cli::cli_abort(
      paste0("`", geom_type, "` is not a supported Esri geometry type"),
      call = call)
  }

  # convert geometry

  geo_conversion_fn <- switch(
    geom_type,
    "POINT" = sfc_point_impl,
    "MULTIPOINT" = sfc_multipoint_impl,
    "LINESTRING" = sfc_linestring_impl,
    "MULTILINESTRING" = sfc_multilinestring_impl,
    "POLYGON" = sfc_polygon_impl,
    "MULTIPOLYGON" = sfc_multipolygon_impl
  )

  rlang::set_names(list(geo_conversion_fn(x)), esri_geo_type)

}







#| Notes -------------------------------------------------------------------
#|
#| There are 3 representations of data that we need to concern ourselves with.
#| 1. geometry objects
#| 2. features
#| 3. feature sets
#|
#| Geometry objects are broken down into 5 types:
#|  - Point: `esriGeometryPoint`
#|  - Multipoint: `esriGeometryMultipoint`
#|  - Polyline: `esriGeometryPolyline`
#|    - note that polyline encompasses both LINESTRING and MULTILINESTRING
#|  - Polygon: `esriGeometryPolygon`
#|    - note that polygon encompasses both POLYGON and MULTIPOLYGON
#|  - Envelope: `esriGeometryEnvelope`
#|
#| Every geometry object is required to have the geometry and a `spatialReference`.
#| The spatial reference can have either `wkid` which is the well-known ID of the
#| spatial reference e.g 4326 or wkt.
#| Esri spatial references can be found at https://spatialreference.org/ref/esri/
#|
#| 3D geometries are also possible for point, multipoint, polygon, and envelope.
#| 3D geometries have a Z field and an optional M field (for measurement).
#| 3D geometries have top level fields `hasZ` and `hasM` which are used to
#| identify if the provided geometry is 3D.


# sfg object conversion ---------------------------------------------------


