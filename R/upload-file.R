#' Upload a File to a Portal
#'
#' Uploads a local file to a portal as a new content item.
#'
#' @param path String. Path to the file.
#' @param title String. Title of the new item.
#' @param type String. Item type. Inferred from the file extension by default.
#' @param tags Character vector. Tags to apply to the item.
#' @param description String. Description of the item.
#' @param snippet String. Summary of the item.
#' @param folder String. Folder to upload into. Defaults to the root folder.
#' @inheritParams arc_item
#' @inheritParams arc_base_req
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/add-item/)
#' @export
#' @family portal item
#' @returns An object of class `PortalItem`.
#' @examples
#' \dontrun{
#' path <- tempfile(fileext = ".csv")
#' write.csv(head(iris), path, row.names = FALSE)
#' upload_file(path, "Iris sample")
#' }
upload_file <- function(
  path,
  title,
  type = NULL,
  tags = NULL,
  description = NULL,
  snippet = NULL,
  folder = NULL,
  host = arc_host(),
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_string(path, allow_empty = FALSE, call = error_call)
  check_string(title, allow_empty = FALSE, call = error_call)
  check_character(tags, allow_null = TRUE, call = error_call)
  check_string(description, allow_null = TRUE, call = error_call)
  check_string(snippet, allow_null = TRUE, call = error_call)
  check_string(folder, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  if (!file.exists(path)) {
    cli::cli_abort(
      "{.arg path} does not exist: {.file {path}}",
      call = error_call
    )
  }

  type <- type %||% infer_item_type(path, error_call = error_call)

  type <- item_type(type)@value

  req_path <- compact(c(
    "sharing/rest/content/users",
    token[["username"]],
    folder,
    "addItem"
  ))

  body <- compact(list(
    file = curl::form_file(path),
    title = title,
    type = type,
    tags = paste0(tags, collapse = ","),
    description = description,
    snippet = snippet
  ))

  resp <- arc_base_req(
    host,
    token,
    path = paste0(req_path, collapse = "/"),
    query = c("f" = "json"),
    error_call = error_call
  ) |>
    httr2::req_body_multipart(!!!body) |>
    httr2::req_perform()

  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
  detect_errors(res)

  arc_item(res[["id"]], host = host, token = token)
}

# https://developers.arcgis.com/rest/users-groups-and-items/items-and-item-types/
item_type_extensions <- c(
  csv = "CSV",
  geojson = "GeoJson",
  geodatabase = "SQLite Geodatabase",
  gpkg = "GeoPackage",
  parquet = "Apache Parquet",
  kml = "KML",
  kmz = "KML",
  xls = "Microsoft Excel",
  xlsx = "Microsoft Excel",
  doc = "Microsoft Word",
  docx = "Microsoft Word",
  ppt = "Microsoft Powerpoint",
  pptx = "Microsoft Powerpoint",
  pdf = "PDF",
  key = "iWork Keynote",
  numbers = "iWork Numbers",
  pages = "iWork Pages",
  vsd = "Visio Document",
  gif = "Image",
  jpg = "Image",
  jpeg = "Image",
  png = "Image",
  tif = "Image",
  tiff = "Image",
  ipynb = "Notebook",
  ecd = "Esri Classifier Definition",
  epk = "Export Package",
  sd = "Service Definition",
  mapx = "Pro Map",
  pagx = "Layout",
  lyrx = "Layer",
  lpkx = "Layer Package",
  mpkx = "Map Package",
  mmpk = "Mobile Map Package",
  mspk = "Mobile Scene Package",
  ppkx = "Project Package",
  aptx = "Project Template",
  slpk = "Scene Package",
  tpk = "Tile Package",
  tpkx = "Compact Tile Package",
  vtpk = "Vector Tile Package",
  stylx = "Desktop Style",
  proconfig = "ArcGIS Pro Configuration",
  dlpk = "Deep Learning Package",
  gpk = "Geoprocessing Package",
  gpkx = "Geoprocessing Package (Pro version)",
  gcpk = "Locator Package",
  rptx = "Pro Report",
  rptt = "Pro Report Template",
  wpk = "Workflow Manager Package"
)

infer_item_type <- function(path, error_call = rlang::caller_env()) {
  ext <- tolower(tools::file_ext(path))
  type <- unname(item_type_extensions[ext])

  if (is.na(type)) {
    cli::cli_abort(
      c(
        "{.arg type} cannot be inferred from {.file {basename(path)}}.",
        "i" = "Set {.arg type} to one of {.fn portal_item_types}."
      ),
      call = error_call
    )
  }

  type
}
