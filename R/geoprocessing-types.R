#' Geoprocessing Parameter Types
#'
#' Functions for converting R objects to and from ArcGIS geoprocessing parameter
#' types. These functions handle the serialization and parsing of various data
#' types used in ArcGIS geoprocessing services.
#'
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' This package provides support for the following geoprocessing parameter types:
#'
#' ## Implemented Types
#'
#' - **GPFeatureRecordSetLayer**: Feature collections with geometry and attributes
#' - **GPRecordSet**: Tabular data without geometry
#' - **GPRasterDataLayer**: Raster datasets from Portal items, Image Servers, or URLs
#' - **GPLinearUnit**: Linear distance measurements with units
#' - **GPArealUnit**: Area measurements with units
#' - **GPDate**: Date/time values in milliseconds since epoch
#' - **GPSpatialReference**: Coordinate reference systems
#'
#' ## Not Yet Implemented
#'
#' The following types are planned for future implementation:
#'
#' - **GPField**: Field definitions with name, type, and properties
#' - **GPMultiValue**: Arrays of values for a single data type
#' - **GPValueTable**: Flexible table-like objects with rows and columns
#' - **GPComposite**: Parameters that accept multiple data types
#' - **GPEnvelope**: Bounding box extents (use `as_extent()` for GPExtent)
#'
#' @section Usage Patterns:
#'
#' Most functions follow a consistent pattern:
#' - `as_gp_*()`: Convert R objects to geoprocessing parameter JSON
#' - `parse_gp_*()`: Parse geoprocessing response JSON to R objects
#' - Constructor functions (e.g., `gp_linear_unit()`, `gp_areal_unit()`) create typed S7 objects
#'
#' @section Examples:
#'
#' ```r
#' # Create a linear unit
#' distance <- gp_linear_unit(distance = 100, units = "esriMeters")
#'
#' # Convert spatial data to feature record set
#' as_gp_feature_record_set(my_sf_data)
#'
#' # Parse a geoprocessing response
#' parse_gp_feature_record_set(response_json)
#' ```
#'
#' @references [API Documentation](https://developers.arcgis.com/rest/services-reference/enterprise/gp-data-types/)
#'
#' @name gp_params
#' @export
#' @family geoprocessing
#' @param json raw json to parse
parse_gp_feature_record_set <- function(json) {
  check_string(json, allow_empty = FALSE)

  geo <- parse_esri_json(
    json,
    query = "/0/value"
  )

  res <- RcppSimdJson::fparse(
    json,
    query = c(param_name = "/0/paramName", data_type = "/0/dataType")
  )

  # inject the geometry
  res[["geometry"]] <- geo
  res
}

#' @name gp_params
#' @export
#' @param x the object to convert into json
as_gp_feature_record_set <- function(x) {
  # this handles sf objects, data.frames, tbl, and tibble
  if (inherits(x, "data.frame")) {
    return(as_esri_featureset(x))
  }

  # this handles the other case of layers or item ids
  switch(
    class(x)[1],
    "PortalItem" = yyjsonr::write_json_str(
      list(
        itemID = x[["id"]]
      ),
      list(auto_unbox = TRUE)
    ),
    "FeatureLayer" = yyjsonr::write_json_str(
      list(url = x[["url"]]),
      list(auto_unbox = TRUE)
    ),
    "Table" = yyjsonr::write_json_str(
      list(url = x[["url"]]),
      list(auto_unbox = TRUE)
    ),
    NULL = cli::cli_abort(
      "Cannot convert {obj_type_friendly(x)} to a GPFeatureRecordSet"
    )
  )
}


# as_gp_feature_record_set(x)
# as_gp_feature_record_set(sfdep::guerry)
# x <- arc_item("35bfb71133704d89b62d33dc9282fdb4")
# x$id

# GPRecordSet ------------------------------------------------------------

#' @name gp_params
#' @export
parse_gp_record_set <- function(json) {
  res <- yyjsonr::read_json_str(json)
  res$value$fields <- data_frame(res$value$fields)
  res$value$features <- data_frame(res$value$fields)
  res
}


#' @name gp_params
#' @export
as_record_set <- function(x) {
  # this handles sf objects, data.frames, tbl, and tibble
  if (inherits(x, "data.frame")) {
    feats <- as_features(x)
    fields <- infer_esri_type(x)

    record_set <- yyjsonr::write_json_str(
      list(fields = fields, features = feats),
      auto_unbox = TRUE
    )

    return(record_set)
  }
  switch(
    class(x)[1],
    "PortalItem" = yyjsonr::write_json_str(
      list(
        itemID = x[["id"]]
      ),
      list(auto_unbox = TRUE)
    ),
    "FeatureLayer" = yyjsonr::write_json_str(
      list(url = x[["url"]]),
      list(auto_unbox = TRUE)
    ),
    "Table" = yyjsonr::write_json_str(
      list(url = x[["url"]]),
      list(auto_unbox = TRUE)
    ),
    "sf" = as_esri_featureset(x),
    NULL = cli::cli_abort(
      "Cannot convert {obj_type_friendly(x)} to a GPFeatureRecordSet"
    )
  )
}


# GPRasterDataLayer ------------------------------------------------------

#' @name gp_params
#' @export
as_gp_raster_layer <- function(x) {
  # must be either PortalItem, ImageServer, or URL

  # use item ID
  if (inherits(x, "PortalItem")) {
    res <- yyjsonr::write_json_str(
      list(
        itemID = x[["id"]]
      ),
      auto_unbox = TRUE
    )
    return(res)
  }

  # use image server
  if (inherits(x, "ImageServer")) {
    return(
      yyjsonr::write_json_str(
        list(url = x[["url"]]),
        auto_unbox = TRUE
      )
    )
  }

  # return URL
  if (rlang::is_string(x)) {
    if (is_url(x)) {
      return(
        yyjsonr::write_json_str(
          list(url = x, auto_unbox = TRUE)
        )
      )
    }
  }
}


# GPLinearUnit -----------------------------------------------------------

#' @name gp_params
#' @export
#' @param units a valid unit name. Must be one of "esriUnknownUnits", "esriInches", "esriPoints", "esriFeet", "esriYards", "esriMiles", "esriNauticalMiles", "esriMillimeters", "esriCentimeters", "esriMeters", "esriKilometers", "esriDecimalDegrees", "esriDecimeters", "esriIntInches", "esriIntFeet", "esriIntYards", "esriIntMiles", "esriIntNauticalMiles".
#' @param distance a scalar number of the distance.
gp_linear_unit <- S7::new_class(
  "GPLinearUnit",
  package = "arcgisutils",
  properties = list(
    distance = S7::class_numeric,
    units = S7::class_character
  ),
  validator = function(self) {
    check_number_decimal(self@distance)
    check_string(self@units, allow_empty = FALSE)
    valid_units <- c(
      "esriUnknownUnits",
      "esriInches",
      "esriPoints",
      "esriFeet",
      "esriYards",
      "esriMiles",
      "esriNauticalMiles",
      "esriMillimeters",
      "esriCentimeters",
      "esriMeters",
      "esriKilometers",
      "esriDecimalDegrees",
      "esriDecimeters",
      "esriIntInches",
      "esriIntFeet",
      "esriIntYards",
      "esriIntMiles",
      "esriIntNauticalMiles"
    )

    if (!self@units %in% valid_units) {
      cli::cli_abort("{.arg units} must be one of {.val {valid_units}}")
    }
  }
)

#' @name gp_params
#' @export
as_gp_linear_unit <- function(x) {
  if (!rlang::inherits_any(x, "GPLinearUnit")) {
    cli::cli_abort(
      c(
        "Expected {.cls GPLinearUnit} found {obj_type_friendly(x)}",
        "i" = "Create one with {.fn gp_linear_unit}"
      )
    )
  }

  yyjsonr::write_json_str(
    list(distance = x@distance, units = x@units),
    auto_unbox = TRUE
  )
}

#' @name gp_params
#' @export
parse_gp_linear_unit <- function(json) {
  rlang::inject(
    gp_linear_unit(!!!yyjsonr::read_json_str(json))
  )
}


#' @name gp_params
#' @export
#' @param area a scalar number of the measurement.
#' @param units the unit of the measurement. Must be one of "esriUnknownAreaUnits", "esriSquareInches", "esriSquareFeet", "esriSquareYards", "esriAcres", "esriSquareMiles", "esriSquareMillimeters", "esriSquareCentimeters", "esriSquareDecimeters", "esriSquareMeters", "esriAres", "esriHectares", "esriSquareKilometers", "esriSquareInchesUS", "esriSquareFeetUS", "esriSquareYardsUS", "esriAcresUS", "esriSquareMilesUS".
gp_areal_unit <- S7::new_class(
  "GPArealUnit",
  package = "arcgisutils",
  properties = list(
    area = S7::class_numeric,
    units = S7::class_character
  ),
  validator = function(self) {
    check_string(self@units, allow_empty = FALSE)
    valid_units <- c(
      "esriUnknownAreaUnits",
      "esriSquareInches",
      "esriSquareFeet",
      "esriSquareYards",
      "esriAcres",
      "esriSquareMiles",
      "esriSquareMillimeters",
      "esriSquareCentimeters",
      "esriSquareDecimeters",
      "esriSquareMeters",
      "esriAres",
      "esriHectares",
      "esriSquareKilometers",
      "esriSquareInchesUS",
      "esriSquareFeetUS",
      "esriSquareYardsUS",
      "esriAcresUS",
      "esriSquareMilesUS"
    )

    if (!self@units %in% valid_units) {
      cli::cli_abort("{.arg units} must be one of {.val {valid_units}}")
    }
  }
)

# GPArealUnit ------------------------------------------------------------

#' @name gp_params
#' @export
as_gp_areal_unit <- function(x) {
  if (!rlang::inherits_any(x, "GPArealUnit")) {
    cli::cli_abort(
      c(
        "Expected {.cls GPArealUnit} found {obj_type_friendly(x)}",
        "i" = "Create one with {.fn gp_areal_unit}"
      )
    )
  }

  yyjsonr::write_json_str(
    list(area = x@area, units = x@units),
    auto_unbox = TRUE
  )
}

#' @name gp_params
#' @export
parse_gp_areal_unit <- function(json) {
  rlang::inject(
    gp_areal_unit(!!!yyjsonr::read_json_str(json))
  )
}


# GPDate -----------------------------------------------------------------

#' @name gp_params
#' @export
as_gp_date <- function(x) {
  date_to_ms(x)
}

#' @name gp_params
#' @export
parse_gp_date <- function(json) {
  res <- yyjsonr::read_json_str(json)
  res$value <- from_esri_date(as.numeric(res$value))
  res
}

# json <- r"({
#   "paramName": "Output_Date",
#   "dataType": "GPDate",
#   "value": 1199145600000
# })"

# parse_gp_date(json)

# GPSpatialReference and GPCoordinateSystem ------------------------------

# TODO: Handle OUTPUT

#' @name gp_params
#' @export
as_spatial_reference <- function(x) {
  crs <- validate_crs(x)
  yyjsonr::write_json_str(crs, auto_unbox = TRUE)
}


#' @name gp_params
#' @export
parse_spatial_reference <- function(json) {
  sr <- yyjsonr::read_json_str(json)
  crs <- sr[["latestWkid"]] %||% sr[["wkid"]] %||% sr[["wkt"]]
  sf::st_crs(crs)
}
