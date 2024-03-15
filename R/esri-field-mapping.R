#' Esri field type mapping
#'
#' Infers Esri field types from R objects.
#'
#' @details
#'
#' - `get_ptype()` takes a scalar character containing the Esri field type and returns a prototype of the pertinent R type
#' - `infer_esri_type()` takes a data frame-like object and infers the Esri field type from it.
#' - `remote_ptype_tbl()` takes a data frame of fields as derived from `list_fields()` and
#' creates a lazy table proto type intended to be used with `dbplyr` integration
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
#'
#' get_ptype("esriFieldTypeDouble")
#' inferred <- infer_esri_type(iris)
#' ptype_tbl(inferred)
#'
#' @returns
#'
#' - `get_pytpe()` returns an object of the class of the prototype.
#' - `ptype_tbl()` takes a `data.frame` with columns `name` and `type` and creates an empty `data.frame` with the corresponding columns and R types
#' - `remote_ptype_tbl()` provides the results of `ptype_tbl()` as a lazy data frame from the `dbplyr` package.
#' - `infer_esri_ptype()` returns a `data.frame` with columns `name`, `type`, `alias`, `nullable`, and `editable` columns
#'   - This resembles that of the `fields` returned by a FeatureService
#' @export
#' @rdname field_mapping
#' @param .data an object of class `data.frame`.
#' @inheritParams cli::cli_abort
#' @inheritParams rlang::caller_arg
infer_esri_type <- function(.data, arg = rlang::caller_arg(.data), call = rlang::caller_env()) {

  if (!inherits(.data, "data.frame")) {
    cli::cli_abort(
      "Expected {.cls data.frame} found {.obj_type_friendly {(.data)}}.",
      call = call,
      arg = arg
    )
  }

  if (inherits(.data, "sf")) .data <- sf::st_drop_geometry(.data)

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
#' @param field_type a character of a desired Esri field type. See details for more.
get_ptype <- function(field_type, call = rlang::caller_env()) {
  res <- switch(
    field_type,
    "esriFieldTypeSmallInteger" = integer(1),
    "esriFieldTypeSingle" = double(1),
    "esriFieldTypeGUID" = integer(1),
    "esriFieldTypeGlobalID" = character(1),
    "esriFieldTypeOID" = integer(1),
    "esriFieldTypeInteger" = integer(1),
    "esriFieldTypeBigInteger" = double(1),
    "esriFieldTypeDouble" = double(1),
    "esriFieldTypeString" = character(1),
    "esriFieldTypeDate" = Sys.Date(),
    "esriFieldTypeGeometry" = numeric(1)
  )

  if (is.null(res)) {
    cli::cli_abort(
      "Column of type {.var field_type} cannot be mapped to an R vector",
      call = call
    )
  }


  res
}

#' @export
#' @rdname field_mapping
ptype_tbl <- function(fields, call = rlang::caller_env()) {
  ftype <- fields[["type"]]
  fname <- fields[["name"]]

  tbl <- as.data.frame(
    lapply(
      rlang::set_names(ftype, fname),
      get_ptype,
      call = call
    )
  )

  # select no rows from it
  tbl[0,]
}


#' @export
#' @rdname field_mapping
#' @param fields a data.frame containing, at least, the columns `type` and `name`.
#'  Typically retrieved from the `field` metadata from a `FeatureLayer` or `Table`.
#'  Also can use the output of `infer_esri_type()`.
remote_ptype_tbl <- function(fields, call = rlang::caller_env()) {

  rlang::check_installed("dbplyr")

  ftype <- fields[["type"]]
  fname <- fields[["name"]]

  dbplyr::lazy_frame(
    ptype_tbl(fields, call = call)
  )
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




# notes -------------------------------------------------------------------


# fields is a dateframe

# users are to provide a character vector name of the
# OID column esriFieldTypeOID
# global ID would be inferred by the feature layer or
# provided by the user I suspect esriFieldTypeGlobalID

# list columns will be omitted and a warning emitted

# field types that will be ignored
# esriFieldTypeSmallInteger
# esriFieldTypeSingle
# esriFieldTypeGeometry (not sure when this would be used)
# esriFieldTypeRaster (not sure when this would be used)
# esriFieldTypeGUID (not sure when this would be used)
# esriFieldTypeXML (oh boy i hope no one has to use this lol)
# esriFieldTypeBigInteger (not sure how this would be supported)

# by default when adding new feature only fields in the feature
# layer should be snet up because they will be ignored
# if there are non-matching field names emit a warning and
# suggest them to use update_fields
