#' Retrieve metadata
#'
#' Utility functions for feature service metadata.
#'
#' @details
#'
#' - `fetch_layer_metadata()` given a request, fetches the metadata by setting
#'    the query parameter `f=json`
#'
#' @param url the url of the item.
#' @param token an `httr2_token` from one of the provided `auth_` functions
#' @param call default [`rlang::caller_env()`]. The calling environment passed to `detect_errors()`.
#' @export
#' @examples
#' # url is broken into parts to fit within 100 characters to avoid CRAN notes
#' url_parts <- c(
#'   "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services",
#'   "/USA_Counties_Generalized_Boundaries/FeatureServer/0"
#' )
#'
#' furl <- paste0(url_parts, collapse = "")
#' meta <- fetch_layer_metadata(furl)
#' head(names(meta))
#' @returns returns a list object
fetch_layer_metadata <- function(url, token = NULL, call = rlang::caller_env()) {
  req <- arc_base_req(url, token, error_call = call)

  # add f=json to the url for querying
  req <- httr2::req_url_query(req, f = "json")

  # process the request and capture the response string
  resp_string <- httr2::resp_body_string(
    httr2::req_perform(req, error_call = call)
  )

  # process the response string
  meta <- RcppSimdJson::fparse(resp_string)

  # check if any errors occurred
  detect_errors(meta, error_call = call)

  # return the list
  meta
}

#' Detect errors in parsed json response
#'
#' The requests responses from ArcGIS don't return the status code
#' in the response itself but rather from the body in the json.
#' This function checks for the existence of an error. If an error is found,
#' the contents of the error message are bubbled up.
#'
#' @param response for `detect_errors()`, a list typically from `RcppSimdJson::fparse(httr2::resp_body_string(resp))`. For `catch_error()`, the string from `httr2::resp_body_string(resp)`.
#' @param error_call default [`rlang::caller_env()`]. The environment from which
#'  to throw the error from.
#' @returns
#'
#' Nothing. Used for it's side effect. If an error code is encountered in the
#' response an error is thrown with the error code and the error message.
#' @export
#' @family requests
#' @examples
#' \dontrun{
#' response <- list(
#'   error = list(
#'     code = 400L,
#'     message = "Unable to generate token.",
#'     details = "Invalid username or password."
#'   )
#' )
#'
#' detect_errors(response)
#' }
detect_errors <- function(response, error_call = rlang::caller_env()) {
  e_msg <- capture_message(response)

  if (is.null(e_msg)) {
    return(invisible(NULL))
  }

  rlang::abort(
    e_msg,
    call = error_call
  )
}

#' @keywords internal
#' @noRd
report_errors <- function(response, error_call = rlang::caller_env()) {
  e_msg <- capture_message(response)

  if (is.null(e_msg)) {
    return(invisible(NULL))
  }

  rlang::warn(
    e_msg,
    call = error_call
  )
}

#' Capture the message from a parsed error JSON object
#' This will print the error status code, message and details if present
#' The JSON must have already been parsed. It might make more sense to
#' try and parse the JSON in a safe way—e.g. use tryCatch
#' @keywords internal
#' @noRd
capture_message <- function(error) {
  e <- error[["error"]]

  if (!is.null(e)) {
    err_msg <- strwrap(
      paste0("  Error", e$messageCode, ": ", e$message),
      prefix = "    ",
      initial = ""
    )

    full_msg <- c(
      "Status code: ",
      e[["code"]],
      "\n",
      paste0(err_msg, collapse = "\n")
    )

    c(paste0(full_msg, collapse = ""), "i" = e$details)
  } else {
    invisible(NULL)
  }
}

#' Catch error will parse the json for you
#' This is so that it can be done safely
#'
#' @rdname detect_errors
#' @export
catch_error <- function(response, error_call = rlang::caller_env()) {
  if (rlang::is_empty(response)) {
    return(invisible(NULL))
  }
  check_string(response, allow_null = TRUE, allow_na = FALSE, call = error_call)

  rlang::catch_cnd(
    report_errors(RcppSimdJson::fparse(response), error_call = error_call)
  )
}

#' Set user-agent for arcgisutils
#'
#' Override the default user-agent set by httr2 to indicate that a request
#' came from arcgisutils.
#'
#' @param req an httr2 request
#' @return an httr2 request object
#' @export
#' @examples
#' req <- httr2::request("http://example.com")
#' arc_agent(req)
arc_agent <- function(req) {
  ver <- utils::packageVersion("arcgisutils")
  httr2::req_user_agent(req, paste0("arcgisutils v", ver))
}
