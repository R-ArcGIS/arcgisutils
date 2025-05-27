#' Parse aan ArcGIS service or content URL into its components
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
    "(?<=(",
    paste0(services, collapse = "|"),
    ")/)",
    layer,
    "/?$"
  )
  grepl(pattern, url, perl = TRUE)
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
  "WFCServer"
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
  "content" = "/home/content\\.html\\?view=",
  "search" = "/home/search\\.html",
  "item" = "/home/item\\.html\\?id=",
  "group" = "/home/group\\.html\\?id=",
  "user" = "/home/user\\.html\\?user=",
  "webscene" = "/home/webscene/viewer\\.html\\?webscene=",
  "webmap" = "(webmap/viewer|mapviewer/index)\\.html\\?webmap=",
  "app" = "/index\\.html\\?appid=",
  "notebook" = "/notebook/notebook\\.html\\?rid=",
  "experience" = "/experience/[a-zA-Z0-9]+"
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
