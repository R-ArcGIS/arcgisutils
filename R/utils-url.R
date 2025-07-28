#' Parse an ArcGIS service or content URL into its components
#'
#' [arc_url_parse()] uses [httr2::url_parse()] to parse URL components and
#' combine the components with a service or content URL `type` and a `layer`
#' number if applicable. A `layer` component is only included if the `type` is `"MapServer"` or
#' `"FeatureServer"` and the URL includes a trailing digit. A full `url` value is
#' also included in the returned list. The `url`, `type`, and `layer` components
#' are not part of the `httr2_url` class object returned by
#' [httr2::url_parse()].
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams httr2::url_parse
#' @inheritParams arc_base_url
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
#' @name url
arc_url_parse <- function(
  url,
  base_url = NULL,
  error_call = rlang::caller_call()
) {
  check_string(url, call = error_call)
  check_string(base_url, allow_null = TRUE, call = error_call)

  # url encode the base url
  if (!is.null(base_url)) {
    base_url <- utils::URLencode(base_url)
  }

  # Parse URL into components
  httr2_url <- httr2::url_parse(
    url = utils::URLencode(url),
    base_url = base_url
  )

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


#' @export
#' @name url
arc_url_type <- function(url, error_call = rlang::caller_call()) {
  check_string(url, call = error_call)

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

#' @export
#' @name url
#' @inheritParams arc_base_req
is_url <- function(
  url,
  error_call = rlang::caller_call()
) {
  check_character(url, call)
  if (!rlang::is_vector(url) || rlang::is_empty(url)) {
    return(FALSE)
  }

  url_pattern <-
    "http[s]?://(?:[[:alnum:]]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

  grepl(url_pattern, url)
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

#' @noRd
str_extract_layer <- function(x) {
  trailing_txt <- str_extract(
    x,
    "(?:\\/)([[:digit:]]+)(?:\\/?)$",
    perl = FALSE
  )

  str_extract(trailing_txt, "[[:digit:]]+", perl = FALSE)
}
