#' @rdname features
#' @export
as_esri_features <- function(x, crs = sf::st_crs(x), call = rlang::caller_env()) {
  # class check
  valid_sfg_classes <- c(
    "sf",
    "data.frame",
    "sfc"
  )

  # exit if an invalid geometry type is provided
  if (!rlang::inherits_any(x, valid_sfg_classes)) {
    cli::cli_abort(
      "{.arg x} must inherit one of the following classes {.cls {valid_sfg_classes}}",
      call = call
    )
  }

  # store the class name to be used in a switch statement
  x_class <- inherits_which(x, valid_sfg_classes)[1]

  sr <- validate_crs(crs)[[1]] %||% list()

  switch(x_class,
    "sf" = as_esri_features_sf(x, sr, call = call),
    "data.frame" = as_esri_features_sf(x, sr, call = call),
    "sfc" = as_esri_features_sfc(x, sr, call = call)
  )
}


as_esri_features_sfc <- function(x, crs = NULL, call = rlang::caller_env()) {
  # class check
  valid_sfg_classes <- c(
    "sfc_POINT",
    "sfc_MULTIPOINT",
    "sfc_LINESTRING",
    "sfc_MULTILINESTRING",
    "sfc_POLYGON",
    "sfc_MULTIPOLYGON"
  )

  # exit if an invalid geometry type is provided
  if (!rlang::inherits_any(x, valid_sfg_classes)) {
    cli::cli_abort(
      "{.arg x} must inherit one of the following classes {.cls {valid_sfg_classes}}",
      call = call
    )
  }

  # store the class name to be used in a switch statement
  sfc_class <- inherits_which(x, valid_sfg_classes)

  # dimension check
  z <- has_z(x)
  m <- has_m(x)

  # abort if 4D
  if (z && m) {
    cli::cli_abort(
      c(
        "{.cls XYZM} geometries detected. Only {.cls XY}, {.cls XYZ}, and {.cls XYM} geometries supported. ",
        ">" = "{.href [please make an issue on GitHub](https://github.com/r-arcgis/arcgisutils)}"
      ),
      call = call
    )
  }

  # determine if 3D
  three_dim <- z || m

  # switch based on dimensions
  if (three_dim) {
    switch(sfc_class,
      "sfc_POINT" = sfc_point_features_3d_string(x, crs),
      "sfc_MULTIPOINT" = sfc_multipoint_features_3d_string(x, crs),
      "sfc_LINESTRING" = sfc_linestring_features_3d_string(x, crs),
      "sfc_MULTILINESTRING" = sfc_multilinestring_features_3d_string(x, crs),
      "sfc_POLYGON" = sfc_polygon_features_3d_string(x, crs),
      "sfc_MULTIPOLYGON" = sfc_multipolygon_features_3d_string(x, crs),
    )
  } else {
    switch(sfc_class,
      "sfc_POINT" = sfc_point_features_2d_string(x, crs),
      "sfc_MULTIPOINT" = sfc_multipoint_features_2d_string(x, crs),
      "sfc_LINESTRING" = sfc_linestring_features_2d_string(x, crs),
      "sfc_MULTILINESTRING" = sfc_multilinestring_features_2d_string(x, crs),
      "sfc_POLYGON" = sfc_polygon_features_2d_string(x, crs),
      "sfc_MULTIPOLYGON" = sfc_multipolygon_features_2d_string(x, crs),
    )
  }
}

as_esri_features_sf <- function(x, crs = NULL, call = rlang::caller_env()) {
  # must be a data.frame
  check_data_frame(x)

  # get the geometry column
  geom_col <- attr(x, "sf_column") %||% ""
  # if it is null then make it into a list
  .geoms <- x[[geom_col]] %||% list()

  # determine dims
  if (rlang::is_empty(.geoms)) {
    z <- FALSE
    m <- FALSE
  } else {
    z <- has_z(.geoms)
    m <- has_m(.geoms)
  }

  # abort if 4D
  if (z && m) {
    cli::cli_abort(
      c(
        "{.cls XYZM} geometries detected. Only {.cls XY}, {.cls XYZ}, and {.cls XYM} geometries supported. ",
        ">" = "{.href [please make an issue on GitHub](https://github.com/r-arcgis/arcgisutils)}"
      ),
      call = call
    )
  }

  # check if three dim
  three_dim <- z || m

  # extract the data.frame without using sf
  # we would otherwise use st_drop_geometry
  .data <- x
  .data[[geom_col]] <- NULL
  # we've verified that x is a data.frame so here we remove any other subclasses
  attr(.data, "class") <- "data.frame"

  # find any factors and convert them to character
  factor_check <- which(vapply(.data, is.factor, logical(1)))

  # convert to factor
  # this wont execute if there aren't any factors
  for (idx in factor_check) {
    .data[[idx]] <- as.character(.data[[idx]])
  }

  # handle dates
  are_dates <- which(vapply(.data, is_date, logical(1)))
  for (col in are_dates) {
    .data[[col]] <- date_to_ms(x[[col]])
  }

  # find columns that cannot be supported
  esri_types <- infer_esri_type(.data)
  invalid_types <- is.na(esri_types$type)
  bad_names <- esri_types$name[invalid_types]

  # here we extract the friendly label for error printing
  bad_types <- lapply(.data[, invalid_types], obj_type_friendly)

  # report an error in the case that theres a list column or something else weird
  if (!rlang::is_empty(bad_types)) {
    cli::cli_abort(
      c(
        "{.var {bad_names}} cannot be converted into EsriJSON. Found {bad_types}"
      ),
      call = call
    )
  }

  # use the appropriate function based on if there is a Z or M value
  # note that st_as_features_2d is used in the case of data.frame
  if (three_dim) {
    sf_as_features_3d_string(.data, .geoms, nrow(.data), crs)
  } else {
    sf_as_features_2d_string(.data, .geoms, nrow(.data), crs)
  }
}
