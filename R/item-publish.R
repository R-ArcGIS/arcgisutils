#' Analyze a File for Publishing
#'
#' Asks the portal to inspect a CSV, Excel, shapefile, GeoPackage, GeoJSON,
#' or service definition file and to derive the parameters needed to publish
#' it as a hosted layer.
#'
#' @param item optional. A `PortalItem` from [`arc_item()`] or a scalar
#'   character item ID of a file already in the portal.
#' @param path optional. A scalar character path to a local file.
#' @param file_type optional. One of `"csv"`, `"excel"`, `"shapefile"`,
#'   `"geoPackage"`, `"geojson"`, or `"Service Definition"`. Inferred from the
#'   item type or the file extension when `NULL`.
#' @param analyze_parameters optional. A named list of analysis settings, such
#'   as `list(locationType = "address", sourceCountry = "US")`. See the API
#'   reference for the full list.
#' @inheritParams arc_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Supply exactly one of `item` or `path`. When you supply `path`, the
#' portal analyzes the uploaded file without adding it to your content.
#' The portal limits shapefiles to 4,000 records and 2 MB and other files to
#' 10 MB.
#'
#' Pass the `publishParameters` element of the result to
#' `arcgislayers::publish_item()`.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/analyze/)
#' @export
#' @family portal publish
#' @returns A list with the elements `publishParameters`, `records`, and,
#'   when the portal reports them, `fileUrl` and `fileSize`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' path <- tempfile(fileext = ".csv")
#' write.csv(data.frame(lon = -117.19, lat = 34.06), path, row.names = FALSE)
#'
#' analysis <- analyze_file(path = path)
#' analysis$publishParameters
#' }
analyze_file <- function(
  item = NULL,
  path = NULL,
  file_type = NULL,
  analyze_parameters = NULL,
  host = arc_host(),
  token = arc_token()
) {
  if (is.null(item) == is.null(path)) {
    cli::cli_abort("Supply exactly one of {.arg item} or {.arg path}.")
  }

  check_string(path, allow_null = TRUE, allow_empty = FALSE)
  check_string(file_type, allow_null = TRUE, allow_empty = FALSE)

  if (!is.null(analyze_parameters) && !rlang::is_named(analyze_parameters)) {
    cli::cli_abort("{.arg analyze_parameters} must be a named list.")
  }

  file_type <- analyze_file_type(file_type, item, path)

  params <- compact(list(
    f = "json",
    filetype = file_type,
    analyzeParameters = if (!is.null(analyze_parameters)) {
      yyjsonr::write_json_str(analyze_parameters, auto_unbox = TRUE)
    }
  ))

  req <- arc_base_req(
    host,
    token,
    path = c("sharing", "rest", "content", "features", "analyze")
  )

  req <- if (is.null(path)) {
    httr2::req_url_query(req, itemid = item_id(item), !!!params)
  } else {
    check_file_exists(path)
    httr2::req_body_multipart(req, file = curl::form_file(path), !!!params)
  }

  # keep arrays of objects as lists so publishParameters round trips to JSON
  req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yyjsonr::read_json_str(arr_of_objs_to_df = FALSE) |>
    detect_errors()
}

analyze_file_types <- c(
  csv = "csv",
  xls = "excel",
  xlsx = "excel",
  zip = "shapefile",
  gpkg = "geoPackage",
  geojson = "geojson",
  json = "geojson",
  sd = "Service Definition"
)

analyze_item_types <- c(
  "CSV" = "csv",
  "Microsoft Excel" = "excel",
  "Shapefile" = "shapefile",
  "GeoPackage" = "geoPackage",
  "GeoJson" = "geojson",
  "Service Definition" = "Service Definition"
)

analyze_file_type <- function(file_type, item, path, call = rlang::caller_env()) {
  if (!is.null(file_type)) {
    return(rlang::arg_match(
      file_type,
      unique(unname(analyze_file_types)),
      error_arg = "file_type",
      error_call = call
    ))
  }

  inferred <- if (!is.null(path)) {
    analyze_file_types[tolower(tools::file_ext(path))]
  } else if (inherits(item, "PortalItem")) {
    analyze_item_types[item[["type"]] %||% ""]
  } else {
    NA_character_
  }

  if (rlang::is_empty(inferred) || is.na(inferred)) {
    cli::cli_abort(
      "Can't infer {.arg file_type}. Supply it, or pass a {.cls PortalItem}.",
      call = call
    )
  }

  unname(inferred)
}

#' Check the Status of an Item Job
#'
#' `item_status()` reports the status of an asynchronous operation on an item,
#' such as a publish, an export, or an upload. `await_item()` checks the
#' status repeatedly until the operation finishes.
#'
#' @param job_id optional. A scalar character job ID returned by a publish,
#'   export, or generate features request.
#' @param job_type optional. One of `"publish"`, `"export"`,
#'   `"generateFeatures"`, or `"createService"`. Required with `job_id`.
#' @param interval default `2`. The number of seconds to wait between checks.
#' @param timeout default `Inf`. The maximum number of seconds to wait.
#' @param user the username that owns `item`. Taken from `item` when it is a
#'   `PortalItem`, otherwise from `token`.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The status is one of `"processing"`, `"partial"`, `"completed"`, or
#' `"failed"`. `"partial"` means a multipart upload has not been committed.
#' `await_item()` returns when the status is `"completed"` and raises an error
#' when it is `"failed"` or when `timeout` elapses.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/status/)
#' @export
#' @family portal publish
#' @returns A list with the elements `status`, `statusMessage`, and `itemId`.
#'   `await_item()` returns it invisibly.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' export <- export_item("b512083cd1b64e2da1d3f66dbb135956", "csv")
#' item_status(export$exportItemId, export$jobId, "export")
#' await_item(export$exportItemId, export$jobId, "export")
#' }
item_status <- function(
  item,
  job_id = NULL,
  job_type = NULL,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  parts <- item_owner(item, own_item_user(item, user, token))
  check_string(job_id, allow_null = TRUE, allow_empty = FALSE)

  if (!is.null(job_type)) {
    job_type <- rlang::arg_match(
      job_type,
      c("publish", "export", "generateFeatures", "createService")
    )
  }

  if (!is.null(job_id) && is.null(job_type)) {
    cli::cli_abort("Supply {.arg job_type} with {.arg job_id}.")
  }

  item_owner_req(parts, "status", host, token) |>
    httr2::req_url_query(!!!compact(list(
      f = "json",
      jobId = job_id,
      jobType = job_type
    ))) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()
}

#' @export
#' @rdname item_status
await_item <- function(
  item,
  job_id = NULL,
  job_type = NULL,
  interval = 2,
  timeout = Inf,
  ...,
  user = NULL,
  host = arc_host(),
  token = arc_token()
) {
  rlang::check_dots_empty()
  check_number_decimal(interval, min = 0)
  check_number_decimal(timeout, min = 0, allow_infinite = TRUE)

  started <- Sys.time()

  repeat {
    status <- item_status(
      item,
      job_id,
      job_type,
      user = user,
      host = host,
      token = token
    )

    if (identical(status[["status"]], "completed")) {
      return(invisible(status))
    }

    if (identical(status[["status"]], "failed")) {
      cli::cli_abort(c(
        "The item job failed.",
        "i" = "{status[['statusMessage']] %||% 'The portal gave no reason.'}"
      ))
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed + interval > timeout) {
      cli::cli_abort(
        "The item job did not finish within {timeout} second{?s}."
      )
    }

    Sys.sleep(interval)
  }
}

#' Export a Hosted Layer
#'
#' Exports a hosted feature layer or table to a new file item, such as a CSV,
#' GeoPackage, shapefile, or file geodatabase.
#'
#' @param export_format one of `"csv"`, `"excel"`, `"shapefile"`,
#'   `"file geodatabase"`, `"geoPackage"`, `"geojson"`, `"kml"`,
#'   `"feature collection"`, `"mobile geodatabase"`, `"Scene Package"`, or
#'   `"Vector Tile Package"`.
#' @param title optional. A scalar character title for the exported item.
#' @param export_parameters optional. A list describing which layers to export
#'   and how, such as `list(layers = list(list(id = 0, where = "pop > 1000")))`.
#'   The portal exports every layer when `NULL`.
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The export runs in the background and creates a new item in the signed in
#' user's content. Pass `exportItemId` and `jobId` from the result to
#' [`await_item()`] with `job_type = "export"` to wait for it, then download
#' the file with [`arc_item_data()`].
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/export-item/)
#' @export
#' @family portal publish
#' @returns A list with the elements `exportItemId`, `jobId`, `serviceItemId`,
#'   `exportFormat`, `type`, and `size`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' export <- export_item(
#'   "b512083cd1b64e2da1d3f66dbb135956",
#'   "geoPackage",
#'   title = "Parcels export"
#' )
#' await_item(export$exportItemId, export$jobId, "export")
#' }
export_item <- function(
  item,
  export_format,
  title = NULL,
  export_parameters = NULL,
  ...,
  token = arc_token()
) {
  rlang::check_dots_empty()
  check_token_has_user(token)
  id <- item_id(item)
  check_string(title, allow_null = TRUE, allow_empty = FALSE)

  export_format <- rlang::arg_match(
    export_format,
    c(
      "csv",
      "excel",
      "shapefile",
      "file geodatabase",
      "geoPackage",
      "geojson",
      "kml",
      "feature collection",
      "mobile geodatabase",
      "Scene Package",
      "Vector Tile Package"
    )
  )

  if (!is.null(export_parameters) && !is.list(export_parameters)) {
    cli::cli_abort("{.arg export_parameters} must be a list.")
  }

  body <- compact(list(
    f = "json",
    itemId = id,
    exportFormat = export_format,
    title = title,
    exportParameters = if (!is.null(export_parameters)) {
      yyjsonr::write_json_str(export_parameters, auto_unbox = TRUE)
    }
  ))

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      token[["username"]],
      "export"
    )
  ) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors()
}

# a bare item ID is most often the signed in user's own item
own_item_user <- function(item, user, token) {
  if (!is.null(user) || inherits(item, "PortalItem")) {
    return(user)
  }
  token[["username"]]
}
