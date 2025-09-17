#' Esri Field Type Mapping
#'
#' Infers Esri field types from R objects. Use [`as_fields()`] to create a
#' data.frame of valid [Esri Field Types](https://developers.arcgis.com/web-map-specification/objects/field/)
#' from an `sf` object or `data.frame`.
#'
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' - `as_fields()` takes a data frame-like object and infers the Esri field type from it.
#' - `fields_as_pytpe_df()` takes a list with `type` and `name` and creates an empty `data.frame` with the corresponding column names and types.

#' - `get_ptype()` takes a scalar character containing the Esri field type and returns a prototype of the pertinent R type

#'
#' ### Field type mapping:
#'
#' Esri field types are mapped as
#'
#' - `esriFieldTypeSmallInteger`: integer
#' - `esriFieldTypeSingle`: double
#' - `esriFieldTypeGUID`: integer
#' - `esriFieldTypeOID`: integer
#' - `esriFieldTypeInteger`: integer
#' - `esriFieldTypeBigInteger`: double
#' - `esriFieldTypeDouble`: double
#' - `esriFieldTypeString`: character
#' - `esriFieldTypeDate`: date
#'
#' R types are mapped as
#'
#' - `double`: esriFieldTypeDouble
#' - `integer`: esriFieldTypeInteger
#' - `character`: esriFieldTypeString
#' - `date`: esriFieldTypeDate
#' - `raw`: esriFieldTypeBlob
#'
#' @examples
#' inferred <- as_fields(iris)
#' inferred
#'
#' fields_as_ptype_df(inferred)
#'
#' @returns
#'
#' - `fields_as_ptype_df()` takes a `data.frame` with columns `name` and `type` and creates an empty `data.frame` with the corresponding columns and R types
#' - `as_fields()` returns a `data.frame` with columns `name`, `type`, `alias`, `nullable`, and `editable` columns
#'   - This resembles that of the `fields` returned by a FeatureService
#' @export
#' @rdname field_mapping
#' @param .data an object of class `data.frame`.
#' @inheritParams cli::cli_abort
#' @inheritParams rlang::caller_arg
as_fields <- function(
  .data,
  arg = rlang::caller_arg(.data),
  call = rlang::caller_env()
) {
  if (!inherits(.data, "data.frame")) {
    cli::cli_abort(
      "Expected {.cls data.frame} found {.obj_type_friendly {(.data)}}.",
      call = call,
      arg = arg
    )
  }

  if (inherits(.data, "sf")) {
    .data <- sf::st_drop_geometry(.data)
  }

  if (nrow(.data) == 0) {
    empty_fields <- data.frame(
      name = character(),
      type = character(),
      alias = character(),
      nullable = logical(),
      editable = logical()
    )

    return(empty_fields)
  }

  # field mappings
  field_base_types <- vapply(.data, typeof, character(1))
  date_check <- vapply(.data, is_date, logical(1))
  factor_check <- vapply(.data, is.factor, logical(1))

  field_base_types[date_check] <- "date"
  field_base_types[factor_check] <- "factor"

  data.frame(
    name = colnames(.data),
    type = vec_mapping[field_base_types],
    alias = colnames(.data),
    length = ifelse(factor_check, 255L, NA),
    nullable = TRUE,
    editable = TRUE
  )
}

#' @export
#' @rdname field_mapping
infer_esri_type <- function(
  .data,
  arg = rlang::caller_arg(.data),
  call = rlang::caller_env()
) {
  lifecycle::deprecate_soft("0.4.0", "infer_esri_type()", "as_fields()")
  as_fields(.data, arg, call)
}


get_ptype <- function(field_type, n = 1, call = rlang::caller_env()) {
  res <- switch(
    field_type,
    "esriFieldTypeSmallInteger" = integer(n),
    "esriFieldTypeSingle" = double(n),
    "esriFieldTypeGUID" = integer(n),
    "esriFieldTypeGlobalID" = character(n),
    "esriFieldTypeOID" = integer(n),
    "esriFieldTypeInteger" = integer(n),
    "esriFieldTypeBigInteger" = double(n),
    "esriFieldTypeDouble" = double(n),
    "esriFieldTypeString" = character(n),
    "esriFieldTypeDate" = rep(Sys.Date(), n),
    "esriFieldTypeGeometry" = numeric(n)
  )

  if (is.null(res)) {
    cli::cli_abort(
      "Column of type {.var field_type} cannot be mapped to an R vector",
      call = call
    )
  }

  res
}

#' @param fields a list or data.frame of field types. Requires the fields `type` and `name` to be present.
#' @export
#' @rdname field_mapping
#' @param n the number of rows to create in the prototype table
fields_as_ptype_df <- function(fields, n = 0, call = rlang::caller_env()) {
  if (!rlang::is_list(fields)) {
    cli::cli_abort(
      "{.arg fields} must be a list with fields `type` and `name`",
      call = call
    )
  }

  ftype <- fields[["type"]]
  fname <- fields[["name"]]

  if (rlang::is_null(ftype)) {
    cli::cli_abort("{.field type} is missing from {.arg fields}", call = call)
  }

  if (rlang::is_null(fname)) {
    cli::cli_abort("{.field name} is missing from {.arg fields}", call = call)
  }

  as.data.frame(
    lapply(
      rlang::set_names(ftype, fname),
      get_ptype,
      n = n,
      call = call
    )
  )
}

#' @export
#' @rdname field_mapping
ptype_tbl <- function(fields, n = 0, call = rlang::caller_env()) {
  lifecycle::deprecate_soft("0.4.0", "ptype_tbl()", "fields_as_ptype_df()")
  fields_as_ptype_df(fields = fields, n = n, call = call)
}


vec_mapping <- c(
  "double" = "esriFieldTypeDouble",
  "integer" = "esriFieldTypeInteger",
  "character" = "esriFieldTypeString",
  "factor" = "esriFieldTypeString",
  # date will be manually defined as being Date or POSIX
  "date" = "esriFieldTypeDate",
  # FIXME actually should be `blob::blob.`
  "raw" = "esriFieldTypeBlob"
)
