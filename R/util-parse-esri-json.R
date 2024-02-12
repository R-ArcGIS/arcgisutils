#' Parse Esri JSON
#'
#' Parses an Esri FeatureSet JSON object into an R object. If there is no
#' geometry present, a data.frame is returned. If there is geometry, an sf object is returned.
#'
#' @param string the raw Esri JSON string.
#' @param ... additional arguments passed to [`RcppSimdJson::fparse`]
#'
#' @examples
#'
#' esri_json <- '{
#'     "geometryType": "esriGeometryPolygon",
#'     "spatialReference": {
#'         "wkid": 4326
#'     },
#'     "hasZ": false,
#'     "hasM": false,
#'     "features": [
#'         {
#'             "attributes": {
#'                 "id": 1
#'             },
#'             "geometry": {
#'                 "rings": [
#'                     [
#'                         [0.0, 0.0],
#'                         [1.0, 0.0],
#'                         [1.0, 1.0],
#'                         [0.0, 1.0],
#'                         [0.0, 0.0]
#'                     ]
#'                 ]
#'             }
#'         }
#'     ]
#' }'
#'
#' parse_esri_json(esri_json)
#'
#'@export
#'@returns
#' A data.frame. If geometry is found, returns an sf object.
parse_esri_json <- function(string, ...) {

  # parse the string
  # ensure any json nulls are NAs
  b_parsed <- RcppSimdJson::fparse(
    string,
    empty_object = NA,
    empty_array = NA,
    single_null = NA,
    ...
  )

  # DEVELOPER NOTE: the rings are not reversed in accordance with
  # the SFA Standard. This doesn't matter to {sf} but it may be useful.
  # Winding can be fixed using `sf::st_sfc(check_winding = TRUE)` at the end

  # extract the geometry features
  fts_raw <- b_parsed[["features"]]

  if (is.null(fts_raw)) {
    report_errors(b_parsed)
    return(data.frame())
  }

  # if this is a logical vector of length one we need to abort
  if (is.logical(fts_raw) && length(fts_raw) == 1L) return(data.frame())

  # bind all of them together into a single data frame
  # TODO do call strips class. So any integer64 classes need to be restored
  fields <- do.call(rbind.data.frame, fts_raw[["attributes"]])

  # check if fields are present in the featureset
  # they are optional
  if (!is.null(b_parsed[["fields"]])) {
    # sanitize dates if there are any
    field_types <- b_parsed[[c("fields", "type")]]
    are_dates <- which(field_types == "esriFieldTypeDate")
    # field position might not match must grab by name
    date_cols <- b_parsed[[c("fields", "name")]][are_dates]

    if (length(are_dates) > 0) {
      for (col in date_cols) {
        fields[[col]] <- from_esri_date(fields[[col]])
      }
    }
  }

  # early return if geometry is missing skips parsing
  if (is.null(b_parsed$features$geometry)) {
    return(fields)
  }

  # extract geometry
  # this allows us to modify in place via for loop
  geo_raw <- fts_raw[["geometry"]]

  # identify what it's first element's first element's class is.
  # If it is a list then its a MULTI variant otherwise
  # its a singular linestring or polygon
  # this value is passed to `identify_class`
  list_ele_class <- class(geo_raw[[1]][[1]])

  sfg_class <- switch(
    b_parsed[["geometryType"]],
    esriGeometryPoint = "POINT",
    esriGeometryMultipoint = "MULTIPOINT",
    esriGeometryPolyline = identify_class("LINESTRING", list_ele_class),
    esriGeometryPolygon = identify_class("POLYGON", list_ele_class)
  )

  # TODO what about xyz and xyzm?
  obj_classes <- c("XY", sfg_class, "sfg")

  # manually apply the sfg class
  for (i in seq_along(geo_raw)) {
    if (sfg_class == "POINT") {
      geo_raw[[i]] <- unlist(geo_raw[[i]])
    } else if (sfg_class %in% c("MULTILINESTRING", "MULTIPOINT")) {
      geo_raw[[i]] <- geo_raw[[i]][[1]]
    } else if (sfg_class == "MULTIPOLYGON") {
      geo_raw[[i]][["spatialReference"]] <- NULL
    }
    class(geo_raw[[i]]) <- obj_classes
  }

  sr <- b_parsed[["spatialReference"]]

  crs_raw <- if (!all(is.na(sr))) {
    sr[["latestWkid"]] %||% sr[["wkid"]]
  } else {
    NA
  }

  sf::st_sf(
    fields, geometry = sf::st_sfc(geo_raw, crs = sf::st_crs(crs_raw))
  )

}


# identifies if an esriGeometryPolyline or esriGeometryPolygon
# is a MULTILINESTRING or MULTIPOLYGON
# requires
identify_class <- function(object_type, inner_class) {
  object_type <- toupper(object_type)
  match.arg(object_type, c("POLYGON", "LINESTRING"))
  switch(
    inner_class[1],
    list = paste0("MULTI", object_type),
    object_type
  )
}
