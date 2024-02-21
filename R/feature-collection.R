#' Create Esri layer objects
#'
#' These functions are used to generate list objects that can be converted into
#' json objects that are used in REST API requests. Notably they are used for adding
#' R objects as items to a portal.
#'
#' A `featureCollection` defines a layer of features that will be stored on a web map.
#' It consists of an array of `layer`s. The `layer` contains the features
#' (attributes and geometries) as a `featureSet` (see [as_featureset()]) and
#' additional metadata which is stored in the `layerDefinition`object. The
#' `layerDefinition` most importantly documents the fields in the object, the object ID,
#' and additional metadata such as name, title, and display scale.
#'
#' Additional documentation for these json object:
#'
#' - [`layer`](https://developers.arcgis.com/documentation/common-data-types/layer.htm)
#' - [`layerDefinition`](https://developers.arcgis.com/documentation/common-data-types/layerdefinition.htm)
#' - [`featureCollection`](https://developers.arcgis.com/documentation/common-data-types/featurecollection.htm)
#'
#' @export
#' @rdname layer_json
#' @param x an object of class `data.frame`. This can be an `sf` object or `tibble` or
#'   any other subclass of `data.frame`.
#' @param name a scalar character of the name of the layer. Must be unique.
#' @param title A user-friendly string title for the layer that can be used in
#'   a table of contents.
#' @param layer_definition a layer definition list as created by `as_layer_definition()`.
#'   A default is derived from `x` and the `name` object.
#' @param id A number indicating the index position of the layer in the WMS or map service.
#' @param layer_url default `NULL`. A string URL to a service that should be used for all queries
#'   against the layer. Used with hosted tiled map services on ArcGIS Online when
#'   there is an associated feature service that allows for queries.
#' @param legend_url default `NULL`. A string URL to a legend graphic for the layer.
#'   Used with WMS layers. The URL usually contains a GetLegendGraphic request.
#' @param popup_info default `NULL`. A list that can be converted into a
#'   [popupInfo](https://developers.arcgis.com/documentation/common-data-types/popupinfo.htm)
#'   object defining the pop-up window content for the layer. There is no helper for
#'   popupInfo objects.
#'
#' @returns
#'
#' A list object containing the required fields for each respective json type.
#' The results can be converted to json using `jsonify::to_json(x, unbox = TRUE)`
#' @examples
#' ld <- as_layer_definition(iris, "iris", "objectID")
#' l <- as_layer(iris, "iris name", "Iris Title")
#' fc <- as_feature_collection(layers = list(l))
as_layer <- function(
    x,
    name,
    title,
    layer_definition = as_layer_definition(
      x,
      name,
      "object_id",
      infer_esri_type(x)
    ),
    id = NULL,
    layer_url = NULL,
    legend_url = NULL,
    popup_info = NULL,
    call = caller_env()
) {

  stopifnot(
    "`x` must have a data.frame base class" = inherits(x, "data.frame"),
    "`name` must be a scalar" = identical(length(name), 1L)
  )

  oid_field <- layer_definition[["objectIdField"]]
  # check that the OID field is present in x if not, create it.
  # if not numeric error
  # if numeric & not integer cast to int
  if (!oid_field %in% colnames(x)) {
    x[[oid_field]] <- 1:nrow(x)
  } else if (!is.numeric(x[[oid_field]])) {
    cli::cli_abort("`x` must have a numeric Object ID field",
                 call = call)
  } else if (!is.integer(x[[oid_field]])) {
    x[[oid_field]] <- as.integer(x[[oid_field]])
  }

  compact(
    list(
      featureSet = as_featureset(x),
      id = id,
      layerDefinition = layer_definition,
      layerUrl = layer_url,
      legendUrl = legend_url,
      name = name,
      title = title,
      popupInfo = popup_info
    )
  )

}




#' @param object_id_field a scalar character vector indicating the name of the
#'   object ID field in the dataset.
#' @param fields a data.frame describing the fields in `x`. These values are inferred
#'  by default via [`infer_esri_type()`].
#' @param display_field default `NULL`. A scalar character containing the name of the field that
#'   best summarizes the feature. Values from this field are used by default as
#'   the titles for pop-up windows.
#' @param has_attachments default `FALSE`.
#' @param drawing_info default `NULL`. See REST documentation in details for more.
#'   There are no helpers or validators for `drawingInfo` objects.
#' @param max_scale default `NULL`. A number representing the maximum scale at
#'   which the layer definition will be applied. The number is the scale's
#'   denominator; thus, a value of 2400 represents a scale of 1/2,400. A value
#'   of 0 indicates that the layer definition will be applied regardless of
#'   how far you zoom in.
#' @param min_scale default `NULL`. A number representing the minimum scale at which the layer definition will be applied.
#' @param templates default `NULL`. See REST documentation in details for more.
#' @param type_id_field default `NULL`. See REST documentation in details for more.
#' @param types An array of type objects available for the dataset.
#'   This is used when the `type_id_field` is populated. NOTE there are no
#'   helper functions to create type objects. Any type list objects must match the
#'   json structure when passed to `jsonify::to_json(x, unbox = TRUE)`.
#' @inheritParams rlang::args_error_context
#' @export
#' @rdname layer_json
as_layer_definition <- function(
    x,
    name,
    object_id_field,
    fields = infer_esri_type(x),
    display_field = NULL,
    drawing_info = NULL,
    has_attachments = FALSE,
    max_scale = 0,
    min_scale = 0,
    templates = NULL,
    type_id_field = NULL,
    types = NULL,
    call = caller_env()
) {

  stopifnot(
    "`x` must inherit `data.frame` class" = inherits(x, "data.frame"),
    "`object_id_field` must be a scalar" = identical(length(object_id_field), 1L)
  )

  geo_type <- determine_esri_geo_type(x, call = call)
  # get geo-type. If NULL `Table` else `Feature Layer`
  type <- if (is.null(geo_type)) {
    "Table"
  } else {
    "Feature Layer"
  }

  # check display field
  if (!is.null(display_field) && !(display_field %in% colnames(x))) {
    cli::cli_abort(
      "`display_field` must be a column in `x`",
      call = call)
  }

  # check OID / create if needed
  if (!object_id_field %in% fields[["name"]]) {
    oid_fields <- data.frame(
      name = object_id_field,
      type = "esriFieldTypeOID",
      alias = object_id_field,
      length = NA,
      editable = FALSE,
      nullable = FALSE
    )

    fields <- rbind(oid_fields, fields)

  } else {
    oid_position <- which(object_id_field %in% fields[["name"]])
    fields[["type"]][oid_position] <- "esriFieldTypeOID"
    # make sure its not editable or nullable
    fields[["editable"]][oid_position] <- FALSE
    fields[["nullable"]][oid_position] <- FALSE
    # remove length if present
    fields[["length"]][oid_position] <- NA
  }



  layer_def_body <- list(
    name = name,
    displayField = display_field,
    # https://developers.arcgis.com/documentation/common-data-types/drawinginfo.htm
    drawingInfo = drawing_info,
    # https://developers.arcgis.com/documentation/common-data-types/field.htm
    objectIdField = object_id_field,
    geometryType = geo_type,
    fields = fields,
    hasAttachments = FALSE,
    maxScale = max_scale,
    minScale = min_scale,
    # https://developers.arcgis.com/documentation/common-data-types/template.htm
    templates = templates,
    # Must be "Feature Layer" or "Table"
    type = type,
    # field that determine the geometry type
    typeIdField = type_id_field,
    # https://developers.arcgis.com/documentation/common-data-types/type.htm
    types = types,
    # this is not documented anywhere but is required from what i can tell
    extent = as_extent(x)
  )

  compact(layer_def_body)

}


#' @export
#' @seealso [as_featureset()]
#' @rdname layer_json
#'
#' @param layers a list of layers as created by `as_layer()`.
#' @param show_legend default `FALSE`. Logical scalar indicating if this layer
#'   should be shown in the legend in client applications.
as_feature_collection <- function(layers = list(), show_legend = TRUE) {
  stopifnot("`layers` must be a list" = is.list(layers))
  c(list(layers = layers), showLegend = show_legend)
}

