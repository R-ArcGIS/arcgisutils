#' Retrieve metadata
#'
#' Utility functions for feature service metadata.
#'
#' @details
#'
#' - `fetch_layer_metadata()` given a request, fetches the metadata by setting
#'    the query parameter `f=json`
#' - `count_features()` is a simple helper that returns the total number of
#'    features in a feature service ignoring any saved query parameters.
#'
#' @param request an [`httr2::request`] object. Should be the `base_req`
#'   object that is created from a provided feature / server url
#' @param token scalar string the `access_token` e.g. from `auth_code()`
#' or `auth_client()`.
#' @export
#' @name requests
#' @examples
#' # url is broken into parts to fit within 100 characters to avoid CRAN notes
#' url_parts <- c(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services",
#'   "/USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' )
#'
#' furl <- paste0(url_parts, collapse = "")
#' req <- httr2::request(furl)
#' meta <- fetch_layer_metadata(req, "")
#' head(names(meta))
#' count_features(req, "")
#' @returns
#' - `fetch_layer_metadata()` returns a list object
#' - `count_features()` returns a scalar integer
fetch_layer_metadata <- function(request, token) {
  req_url <- httr2::req_body_form(
    request,
    f = "json",
    token = token
  )

  resp_string <- httr2::resp_body_string(
    httr2::req_perform(req_url)
  )

  meta <- RcppSimdJson::fparse(resp_string)

  detect_errors(meta)

  meta
}

#' @export
#' @name requests
count_features <- function(request, token) {

  req_url <- httr2::req_body_form(
    httr2::req_url_path_append(request, "query"),
    returnCountOnly = "true",
    where = "1 = 1",
    f = "json",
    token = token
  )

  resp <- httr2::req_perform(req_url)

  RcppSimdJson::fparse(
    httr2::resp_body_string(resp),
    query = "/count"
  )
}


#' Detect errors in parsed json response
#'
#' The requests responses from ArcGIS don't return the status code
#' in the response itself but rather from the body in the json.
#' This function checks for the existence of an error. If it is found
#' the contents of the error message are bubbled up.
#'
#' @param response a [`httr2::response`] object.
#' @param error_call default [`rlang::caller_env()`]. The environment from which
#'  to throw the error from.
#' @returns
#'
#' Nothing. Used for it's side effect. If an error code is encountered in the
#' response an error is thrown with the error code and the error message.
#' @export
#' @family requests
#' @examples
#'
#' if (interactive()) {
#'   response <- list(
#'     error = list(
#'       code = 400L,
#'       message = "Unable to generate token.",
#'       details = "Invalid username or password."
#'     )
#'   )
#'
#'   detect_errors(response)
#' }
detect_errors <- function(response, error_call = rlang::caller_env()) {

  e <- response[["error"]]

  if (!is.null(e)) {

    err_msg <- strwrap(
      paste0("  Error ", e$messageCode, ": ", e$message),
      prefix = "    ",
      initial = ""
    )

    full_msg <- c(
      "Status code: ",
      response[["error"]][["code"]],
      "\n",
      paste0(err_msg, collapse = "\n")
    )

    rlang::abort(
      paste0(full_msg, collapse = ""),
      call = error_call
    )
  }
}



