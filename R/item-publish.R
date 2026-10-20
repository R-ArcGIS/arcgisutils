#' Analyze a File for Publishing
#'
#' `analyze_file()` asks the portal to inspect a CSV, Excel, shapefile,
#' GeoPackage, GeoJSON, or service definition file. The portal returns the
#' parameters you need to publish the file as a hosted layer.
#'
#' @param item optional. A `PortalItem` from [`arc_item()`] or a scalar
#'   character item ID of a file already in the portal.
#' @param path optional. A scalar character path to a local file.
#' @param file_type optional. One of `"csv"`, `"excel"`, `"shapefile"`,
#'   `"geoPackage"`, `"geojson"`, or `"Service Definition"`. When `NULL`,
#'   `analyze_file()` infers it from the item type or the file extension.
#' @param analyze_parameters optional. A named list of analysis settings, such
#'   as `list(locationType = "address", sourceCountry = "US")`. The API
#'   reference lists every setting.
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Supply exactly one of `item` or `path`. When you supply `path`, the
#' portal analyzes the uploaded file without adding it to your content.
#' The portal accepts shapefiles of up to 4,000 records and 2 MB and other
#' files of up to 10 MB.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  obj_check_token(token, call = error_call)

  if (is.null(item) == is.null(path)) {
    cli::cli_abort(
      "Supply exactly one of {.arg item} or {.arg path}.",
      call = error_call
    )
  }

  check_string(path, allow_null = TRUE, allow_empty = FALSE, call = error_call)
  check_string(
    file_type,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )

  if (!is.null(analyze_parameters) && !rlang::is_named(analyze_parameters)) {
    cli::cli_abort(
      "{.arg analyze_parameters} must be a named list.",
      call = error_call
    )
  }

  if (!is.null(file_type)) {
    file_type <- rlang::arg_match(
      file_type,
      c("csv", "excel", "shapefile", "geoPackage", "geojson", "Service Definition"),
      error_call = error_call
    )
  } else if (!is.null(path)) {
    file_type <- switch(
      tolower(tools::file_ext(path)),
      csv = "csv",
      xls = "excel",
      xlsx = "excel",
      zip = "shapefile",
      gpkg = "geoPackage",
      geojson = "geojson",
      json = "geojson",
      sd = "Service Definition"
    )
  } else if (inherits(item, "PortalItem")) {
    file_type <- switch(
      item[["type"]] %||% "",
      "CSV" = "csv",
      "Microsoft Excel" = "excel",
      "Shapefile" = "shapefile",
      "GeoPackage" = "geoPackage",
      "GeoJson" = "geojson",
      "Service Definition" = "Service Definition"
    )
  }

  if (is.null(file_type)) {
    cli::cli_abort(
      "Can't infer {.arg file_type}. Supply it, or pass a {.cls PortalItem}.",
      call = error_call
    )
  }

  params <- compact(list(
    f = "json",
    filetype = file_type,
    analyzeParameters = if (!is.null(analyze_parameters)) {
      yyjsonr::write_json_str(analyze_parameters, auto_unbox = TRUE)
    }
  ))

  req <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c("sharing", "rest", "content", "features", "analyze"),
    error_call = error_call
  )

  req <- if (is.null(path)) {
    httr2::req_url_query(
      req,
      itemid = item_id(item, call = error_call),
      !!!params
    )
  } else {
    if (!file.exists(path)) {
      cli::cli_abort(
        "{.arg path} does not exist: {.file {path}}",
        call = error_call
      )
    }
    httr2::req_body_multipart(req, file = curl::form_file(path), !!!params)
  }

  req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    yyjsonr::read_json_str(arr_of_objs_to_df = FALSE) |>
    detect_errors(error_call = error_call)
}

#' Check the Status of an Item Job
#'
#' `item_status()` reports the status of an asynchronous operation on an item,
#' such as a publish, an export, or an upload. `await_item()` checks the
#' status repeatedly until the operation finishes.
#'
#' @param job_id optional. A scalar character job ID from a publish, export,
#'   or generate features request.
#' @param job_type optional. One of `"publish"`, `"export"`,
#'   `"generateFeatures"`, or `"createService"`. Required with `job_id`.
#' @param interval default `2`. The number of seconds to wait between checks.
#' @param timeout default `Inf`. The maximum number of seconds to wait.
#' @inheritParams share_item
#' @inheritParams upload_file
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal reports one of `"processing"`, `"partial"`, `"completed"`, or
#' `"failed"`. `"partial"` means that nobody has committed a multipart upload
#' yet. `await_item()` returns when the status is `"completed"` and raises an
#' error when the status is `"failed"` or when `timeout` elapses.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/status/)
#' @export
#' @family portal publish
#' @returns A list with the elements `status`, `statusMessage`, and `itemId`.
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  obj_check_token(token, call = error_call)
  if (is.null(user) && !inherits(item, "PortalItem")) {
    user <- token[["username"]]
  }

  parts <- item_owner(item, user, call = error_call)
  check_string(job_id, allow_null = TRUE, allow_empty = FALSE, call = error_call)

  if (!is.null(job_type)) {
    job_type <- rlang::arg_match(
      job_type,
      c("publish", "export", "generateFeatures", "createService"),
      error_call = error_call
    )
  }

  if (!is.null(job_id) && is.null(job_type)) {
    cli::cli_abort(
      "Supply {.arg job_type} with {.arg job_id}.",
      call = error_call
    )
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "content",
      "users",
      parts$owner,
      "items",
      parts$id,
      "status"
    ),
    error_call = error_call
  ) |>
    httr2::req_url_query(
      !!!compact(list(
        f = "json",
        jobId = job_id,
        jobType = job_type
      ))
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_number_decimal(interval, min = 0, call = error_call)
  check_number_decimal(
    timeout,
    min = 0,
    allow_infinite = TRUE,
    call = error_call
  )

  started <- Sys.time()

  repeat {
    status <- item_status(
      item,
      job_id,
      job_type,
      user = user,
      token = token,
      error_call = error_call
    )

    if (identical(status[["status"]], "completed")) {
      return(status)
    }

    if (identical(status[["status"]], "failed")) {
      cli::cli_abort(
        c(
          "The item job failed.",
          "i" = "{status[['statusMessage']] %||% 'The portal gave no reason.'}"
        ),
        call = error_call
      )
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed + interval > timeout) {
      cli::cli_abort(
        "The item job did not finish within {timeout} second{?s}.",
        call = error_call
      )
    }

    Sys.sleep(interval)
  }
}

#' Export a Hosted Layer
#'
#' `export_item()` exports a hosted feature layer or table to a new file item,
#' such as a CSV, GeoPackage, shapefile, or file geodatabase.
#'
#' @param export_format one of `"csv"`, `"excel"`, `"shapefile"`,
#'   `"file geodatabase"`, `"geoPackage"`, `"geojson"`, `"kml"`,
#'   `"feature collection"`, `"mobile geodatabase"`, `"Scene Package"`, or
#'   `"Vector Tile Package"`.
#' @param export_parameters optional. A list that names the layers to export
#'   and how to export them, such as
#'   `list(layers = list(list(id = 0, where = "pop > 1000")))`. When `NULL`,
#'   the portal exports every layer.
#' @inheritParams upload_file
#' @inheritParams share_item
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal runs the export in the background and creates a new item in the
#' signed in user's content. Pass `exportItemId` and `jobId` from the result
#' to [`await_item()`] with `job_type = "export"` to wait for it, then
#' download the file with [`arc_item_data()`].
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
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_token_has_user(token, call = error_call)
  id <- item_id(item, call = error_call)
  check_string(title, allow_null = TRUE, allow_empty = FALSE, call = error_call)

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
    ),
    error_call = error_call
  )

  if (!is.null(export_parameters) && !is.list(export_parameters)) {
    cli::cli_abort("{.arg export_parameters} must be a list.", call = error_call)
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
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(!!!body) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}
