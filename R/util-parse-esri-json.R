#' Parse Esri JSON
#'
#' Parses an Esri FeatureSet JSON object into an R object. If there is no
#' geometry present, a data.frame is returned. If there is geometry, an sf object is returned.
#'
#' @param string the raw Esri JSON string.
#' @param ... additional arguments passed to [`RcppSimdJson::fparse`]
#' @inheritParams cli::cli_abort
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
parse_esri_json <- function(string, ..., call = rlang::caller_env()) {

  # parse the string
  # ensure any json nulls are NAs
  b_parsed <- RcppSimdJson::fparse(
    string,
    empty_object = NA,
    empty_array = NULL,
    single_null = NA,
    ...
  )

  # DEVELOPER NOTE: the rings are not reversed in accordance with
  # the SFA Standard. This doesn't matter to {sf} but it may be useful.
  # Winding can be fixed using `sf::st_sfc(check_winding = TRUE)` at the end

  # extract the geometry features
  fts_raw <- b_parsed[["features"]]

  if (is.null(fts_raw)) {
    report_errors(b_parsed, error_call = call)

    # return an empty data.frame with the appropriate fields
    return(ptype_tbl(b_parsed[["fields"]]))
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

    # handle dates if present
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
    esriGeometryPolygon = identify_class("POLYGON", list_ele_class),
    # Error if unknown geometryType
    cli::cli_abort(
      "Unsupported geometry type {.val {b_parsed[[\"geometryType\"]]}}",
      call = call
    )
  )

  # TODO what about xyz and xyzm?
  obj_classes <- c("XY", sfg_class, "sfg")

  # manually apply the sfg class
  for (i in seq_along(geo_raw)) {
    if (rlang::is_scalar_integer(geo_raw[[i]])) {
      geo_raw[[i]] <- rlang::eval_bare(
        rlang::parse_expr(paste0("sf::st_", tolower(sfg_class), "()"))
      )
    } else if (is.null(geo_raw[[i]][[1]])) {
      # use empty geometry collection to represent NULL geometry
      geo_raw[[i]] <- sf::st_geometrycollection()
      next
    } else if (sfg_class == "POINT") {
      geo_raw[[i]] <- unlist(geo_raw[[i]])
    } else if (sfg_class %in% c("MULTILINESTRING", "MULTIPOINT")) {

      # weird edge case where we get a single matrix instead of a list
      # of matrix we need to put it into a list
      if (sfg_class == "MULTILINESTRING" && is.matrix(geo_raw[[i]])) {
        geo_raw[[i]] <- list(geo_raw[[i]])
      }

      geo_raw[[i]] <- geo_raw[[i]][[1]]

    } else if (sfg_class == "MULTIPOLYGON") {

      # remove spatial reference field from multipolygon
      geo_raw[[i]][["spatialReference"]] <- NULL

    }

    # add class to geometry in place
    class(geo_raw[[i]]) <- obj_classes
  }

  # extract spatial reference
  sr <- b_parsed[["spatialReference"]]

  # find the provided spatial reference
  crs_raw <- if (!all(is.na(sr))) {
    sr[["latestWkid"]] %||% sr[["wkid"]]
  } else {
    NA
  }

  # create an sf object
  sf::st_sf(
    fields,
    # create an sfc object
    geometry = sf::st_sfc(geo_raw, crs = sf::st_crs(crs_raw))
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
