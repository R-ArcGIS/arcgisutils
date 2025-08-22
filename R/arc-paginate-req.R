#' Paginate ArcGIS Requests
#'
#' Many API endpoints provide common [pagination properties](https://developers.arcgis.com/rest/users-groups-and-items/common-parameters/#paging-properties). This `arc_paginate_request()` automatically applies pagination to an input request.
#'
#' @returns a list of `httr2_response`.
#'
#' @param req an `httr2_request` ideally created with `arc_base_req`
#' @param page_size a scalar integer between 1 and 100 indicating the number of responses per page.
#' @param `max_pages` the maximum number of pages to fetch. By default fetches all pages.
#' @param .progress default `TRUE`. Whether to display a progress bar for requests.
#'
#' @export
#' @references [API Documentation](https://developers.arcgis.com/rest/users-groups-and-items/common-parameters/#paging-properties)
#' @seealso [arc_base_req()]
arc_paginate_req <- function(
  req,
  page_size = 10,
  max_pages = Inf,
  .progress = TRUE
) {
  if (!inherits(x, "httr2_request")) {
    cli::cli_abort(
      c(
        "{.arg req} must be an {.cls httr2_request}",
        "i" = "Create a new request with {.fn arc_base_req}"
      )
    )
  }
  check_bool(.progress)
  check_number_whole(page_size, 1, 100)
  check_number_whole(max_pages, min = 1, allow_infinite = TRUE)
  httr2::req_perform_iterative(
    httr2::req_url_query(req, num = page_size),
    arc_next_req,
    max_reqs = max_pages,
    on_error = "return",
    progress = .progress
  )
}

# Internal function that is applied to req_perform_iterative
arc_next_req <- function(resp, req) {
  if (httr2::resp_is_error(resp)) {
    return(NULL)
  }

  resp_type <- httr2::resp_content_type(resp)

  if (!resp_type %in% c("text/plain", "application/json")) {
    cli::cli_warn("Response returned {.val {resp_type}} but expected json")
    return(NULL)
  }

  # extract next page
  next_start <- rlang::try_fetch(
    {
      RcppSimdJson::fparse(
        httr2::resp_body_string(resp),
        query = "/nextStart"
      )
    },
    error = function(cnd) {
      cli::cli_warn("Failed to extract next page—ending pagination")
      NULL
    }
  )

  # if we didnt get an integer warn and return NULL
  if (!rlang::is_integerish(next_start)) {
    cli::cli_warn(c(
      "Error extracting next page.",
      "i" = "Expected an integer found {obj_type_friendly(next_start)}"
    ))
    return(NULL)
  }

  # -1 when pagination is over
  if (next_start < 1) {
    return(NULL)
  }

  # next response
  httr2::req_url_query(req, start = next_start)
}
