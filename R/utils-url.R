#' Parse an ArcGIS service or content URL into its components
#'
#' [arc_url_parse()] uses [httr2::url_parse()] to parse URL components and
#' combine the components with a service or content URL `type` and a `layer`
#' number. A `layer` component is only included if the `type` is "MapServer" or
#' "FeatureServer" and the URL includes a trailing digit. A full `url` value is
#' also included in the returned list. The `url`, `type`, and `layer` components
#' are not part of the `httr2_url` class object returned by
#' [httr2::url_parse()].
#'
#' @inheritParams httr2::url_parse
#' @examples
#' arc_url_parse(
#'   "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer/0"
#' )
#' arc_url_parse(
#'   "https://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer"
#' )
#' arc_url_parse(
#'   "https://services.arcgisonline.com/arcgis/rest/services/WorldElevation3D/Terrain3D/ImageServer"
#' )
#'
#' @returns A named list with the following components: scheme, hostname, username, password, port, path, query, fragment, url, type, and layer.
#' @export
arc_url_parse <- function(url, base_url = NULL) {
  # Parse URL into components
  httr2_url <- httr2::url_parse(url = url, base_url = base_url)

  # Extract the service or content type from URL
  type <- arc_url_type(url)

  # Extract trailing layer number from URL path
  layer <- NULL
  if (has_arc_url_layer(httr2_url[["path"]])) {
    layer <- str_extract(
      httr2_url[["path"]],
      "(?<=/)([0-9]+(?=/)|[0-9]+$)"
    )
  }

  c(
    unclass(httr2_url),
    list(
      url = httr2::url_build(httr2_url),
      type = type,
      layer = layer
    )
  )
}

#' Does the path have a layer at the end?
#' @noRd
has_arc_url_layer <- function(
  url,
  services = c("FeatureServer", "MapServer"),
  layer = NULL
) {
  layer <- layer %||% "[0-9]+"
  pattern <- paste0(
    "(",
    paste0(services, collapse = "|"),
    ")/",
    layer,
    "/?$"
  )

  grepl(pattern, url)
}

#' Vector of valid ArcGIS service types
#'
#' Length 9 character vector
#' @noRd
arc_service_types <- c(
  "FeatureServer",
  "MapServer",
  "ImageServer",
  "GeoDataServer",
  "GeocodeServer",
  "GeometryServer",
  "GPServer",
  "WFSServer",
  "WFCServer",
  "SceneServer"
)

#' Vector of valid ArcGIS layer types
#'
#' Length 3 character vector
#' @noRd
arc_layer_types <- c(
  "Feature Layer",
  "Table",
  "Group Layer"
)

#' Named vector of ArcGIS Content URL patterns
#'
#' Length 10 character vector of item or content URL patterns.
#' @noRd
arc_content_types <- c(
  "content" = "/content\\.html\\?view=",
  "search" = "/search\\.html",
  "item" = "/item\\.html\\?id=",
  "group" = "/group\\.html\\?id=",
  "user" = "/user\\.html\\?user=",
  "webscene" = "/home/webscene/viewer\\.html\\?webscene=",
  "webmap" = "(webmap/viewer|mapviewer/index)\\.html\\?webmap=",
  "app" = "/index\\.html\\?appid=",
  "notebook" = "/notebook/notebook\\.html\\?id=",
  "experience" = "/experience/[a-zA-Z0-9]+",
  "storymap" = "/stories/[a-zA-Z0-9]+",
  "dashboard" = "/dashboards/[a-zA-Z0-9]+",
  "datapipeline" = "/datapipelines/editor\\?item\\=",
  "webapp" = "webappviewer/index.html\\?id=[a-zA-Z0-9]+"
)

#' What type of ArcGIS service or content URL is this?
#' @noRd
arc_url_type <- function(url) {
  check_string(url)

  pattern <- c(
    rlang::set_names(
      arc_service_types,
      arc_service_types
    ),
    arc_content_types
  )

  # TODO: Add check for "rest/services" or other patterns to ensure the URL is a
  # valid ArcGIS service or content url and warn otherwise
  matches <- vapply(pattern, grepl, x = url, FUN.VALUE = logical(1))

  if (!any(matches)) {
    return(NULL)
  }

  names(pattern[matches])
}

#' Does x match the pattern of a URL?
#' @noRd
is_url <- function(
  x,
  pattern = NULL,
  ...
) {
  if (!rlang::is_vector(x) || rlang::is_empty(x)) {
    return(FALSE)
  }

  url_pattern <-
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  if (is.null(pattern)) {
    return(grepl(url_pattern, x))
  }

  pattern <- paste0(pattern, collapse = "|")

  grepl(url_pattern, x) & grepl(pattern, x, ...)
}


#' Extract layer number (w/ or w/o trailing slash)
#' @noRd
str_extract_layer <- function(x) {
  trailing_txt <- str_extract(
    x,
    "(?:\\/)([[:digit:]]+)(?:\\/?)$",
    perl = FALSE
  )

  str_extract(trailing_txt, "[[:digit:]]+", perl = FALSE)
}

#' Convert different types of ArcGIS URLs
#'
#' @description
#'  A collection of utility functions that convert between different types of
#'  URLs by using query arguments from the URL and the Sharing API and REST API
#'  to extract item, service, or layer metadata.
#'
#' - [as_arc_service_url()] converts an item or layer URL to a service URL
#'  - [as_arc_layer_url()] converts an item or service URL to a layer or table URL
#'  - [as_arc_sharing_url()] converts an item, user, or group URL to a Sharing API URL
#'
#' @name as_arc_url
#' @examples
#'
#' item_url <- "https://www.arcgis.com/home/item.html?id=373a421a19564b0996ff30d554be964b"
#'
#' as_arc_service_url(item_url)
#'
#' as_arc_layer_url(item_url)
#'
#' item_layer_url <- "https://www.arcgis.com/home/item.html?id=5192f89d7c55444ba3a4933e014c9eb8&sublayer=5"
#'
#' as_arc_layer_url(item_layer_url)
#'
#' layer_url <- "https://services1.arcgis.com/fBc8EJBxQRMcHlei/arcgis/rest/services/NPS_Fee_Boundary/FeatureServer/0"
#'
#' as_arc_service_url(layer_url)
#'
#'
#' @param url A valid input URL.
#' @param layer A layer ID value. Layer names are not yet supported.
#' @inheritParams fetch_layer_metadata
#' @seealso [arc_url_parse()]
#' @returns A URL of the relevant type.
NULL

#' @rdname as_arc_url
#' @export
as_arc_service_url <- function(
  url,
  token = NULL,
  call = rlang::caller_env()
) {
  # Return service URLs or layer URLs (without layer number)
  if (grepl(paste0(arc_service_types, collapse = "|"), url)) {
    # TODO: Remove the layer ID if included

    if (has_arc_url_layer(url)) {
      url <- sub("[[:digit:]]+/?$", "", url)
    }

    return(url)
  }

  # Require an item URL otherwise
  # TODO: Explore if anything other than item URLs can work
  if (arc_url_type(url) != "item") {
    cli::cli_abort(
      "{.arg url} must be an item or service URL.",
      call = call
    )
  }

  sharing_url <- as_arc_sharing_url(url, call = call)

  url_meta <- fetch_layer_metadata(
    sharing_url,
    token = token,
    call = call
  )

  # TODO: Explore if additional item types are allowed
  allowed_item_type <- c("Feature Service", "Map Service")

  if (!(url_meta[["type"]] %in% allowed_item_type)) {
    cli::cli_abort(
      "{.arg url} must be a {.or {allowed_item_type}} item type to return as a service URL.",
      call = call
    )
  }

  url_meta[["url"]]
}

#' @rdname as_arc_url
#' @export
as_arc_layer_url <- function(
  url,
  layer = NULL,
  token = NULL,
  call = rlang::caller_env()
) {
  # Validate layer argument
  # TODO: Consider splitting layer into name/id per arcgislayers::get_all_layers
  check_string(layer, allow_empty = FALSE, allow_null = TRUE, call = call)

  # Parse URL
  parsed_url <- arc_url_parse(url)

  # Return existing layer URLs as is
  if (has_arc_url_layer(parsed_url[["path"]], layer = layer)) {
    return(url)
  }

  # Validate type
  url_type <- parsed_url[["type"]]

  if (!rlang::is_string(url_type)) {
    cli::cli_abort(
      "The {.arg url} type can't be determined.",
      call = call
    )
  }

  if (!(url_type %in% c("item", "FeatureServer", "MapServer"))) {
    cli::cli_abort(
      "{.arg url} must be an item, FeatureServer, or MapServer URL.",
      call = call
    )
  }

  if (url_type == "item") {
    sharing_url <- as_arc_sharing_url(url, call = call)

    url_meta <- fetch_layer_metadata(
      sharing_url,
      token = token,
      call = call
    )

    # Return item metadata URL if it is a layer URL
    if (has_arc_url_layer(url_meta[["url"]], layer = layer)) {
      url <- url_meta[["url"]]
      if (!is.null(layer)) {
        url <- paste0(url, "/", layer)
      }

      return(url)
    }

    # Use sublayer ID from item URL if available
    layer <- layer %||% parsed_url[["query"]][["sublayer"]]

    # Set layer value to 0 if single layer service found
    is_service <- is_arc_service_url(url_meta[["url"]])
    is_single_layer <- "Singlelayer" %in% url_meta[["typeKeywords"]]

    if (is_service && is_single_layer) {
      # TODO: Validate layer if provided (this allows for invalid URLs)
      layer <- layer %||% 0
    }

    # Return URL if layer could be derived
    if (has_arc_url_layer(url_meta[["url"]], layer = layer)) {
      url <- url_meta[["url"]]
      if (!is.null(layer)) {
        url <- paste0(url, "/", layer)
      }

      return(url)
    }

    # TODO: Error when item URLs can't be converted to service URLs to avoid coercing to invalid URLs
    url <- url_meta[["url"]]
    url_type <- parsed_url[["type"]]
  }

  url_meta <- fetch_layer_metadata(
    url,
    token = token,
    call = call
  )

  # TODO: Add (optional?) validation for layer value
  layer <- layer %||%
    c(url_meta[["layers"]][["id"]], url_meta[["tables"]][["id"]])

  if (length(layer) > 1) {
    cli::cli_abort(
      c(
        "{.arg url} is for a {url_type} url with multiple layers.",
        "A single {.arg layer} value must be provided."
      ),
      call = call
    )
  } else if (is.null(layer)) {
    cli::cli_abort(
      "{.arg layer} can't be derived from the supplied {.arg url}.",
      call = call
    )
  }

  paste0(url, "/", layer)
}

#' @rdname as_arc_url
#' @export
as_arc_sharing_url <- function(url, call = rlang::caller_env()) {
  # Return sharing content URLs as is
  if (is_url(url, "sharing/rest/content")) {
    return(url)
  }

  # Parse other URLs and extract item, group, or user ID
  url_components <- arc_url_parse(url)
  url_type <- url_components[["type"]]
  if (url_type %in% c("item", "group")) {
    id <- url_components[["query"]][["id"]]
  } else if (url_type == "user") {
    id <- url_components[["query"]][["user"]]
  } else {
    cli::cli_abort(
      "{.arg url} must be an ArcGIS item, group, or user URL.",
      call = call
    )
  }

  if (!rlang::is_string(id)) {
    cli::cli_abort(
      "{.arg url} must contain an item, group, or user ID.",
      call = call
    )
  }

  # Build URL
  paste0(
    "https://www.arcgis.com/sharing/rest/content/",
    url_type,
    "s/",
    id
  )
}
