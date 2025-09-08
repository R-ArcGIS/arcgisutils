# See https://rconsortium.github.io/S7/articles/packages.html#backward-compatibility
# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

#' Geoprocessing Job Status
#'
#' Represents the status of a geoprocessing job.
#'
#' @param status a scalar character. Must be one of `"esriJobSubmitted"`,
#' `"esriJobWaiting"`, `"esriJobExecuting"`, `"esriJobSucceeded"`, `"esriJobFailed"`,
#' `"esriJobTimedOut"`, `"esriJobCancelling"`, or `"esriJobCancelled`".
#'
#' @export
#' @family geoprocessing
#' @return an object of class `gp_job_status`
arc_job_status <- S7::new_class(
  "arc_job_status",
  package = "arcgisutils",
  properties = list(status = S7::class_character),
  validator = function(self) {
    job_statuses <- c(
      "esriJobSubmitted",
      "esriJobWaiting",
      "esriJobExecuting",
      "esriJobSucceeded",
      "esriJobFailed",
      "esriJobTimedOut",
      "esriJobCancelling",
      "esriJobCancelled"
    )

    # permit length 0
    if (length(self@status) == 0L) {
      return(NULL)
    }
    # if it is longer than 0, it should have been set to something
    if (length(self@status) > 1) {
      cli::cli_abort("Job status must be a scalar value")
    } else if (!self@status %in% job_statuses) {
      cli::cli_abort("Job status must be one of {.val {job_statuses}}")
    }
  }
)


#' Form request parameters
#'
#' ArcGIS endpoints make extensive use of form encoded data for the body
#' of http requests. Form requests require that each element has a name
#' and is encoded as a single string—often as json.
#'
#' The `arc_form_params` class provides validation of form body parameters
#' ensuring that each element is a scalar string. It uses a named list
#' internally to store the parameters.
#'
#' The helper function `as_form_params()` converts a named list to form
#' parameters by automatically JSON-encoding each element using
#' `yyjsonr::write_json_str()` with `auto_unbox = TRUE`.
#'
#' @export
#' @family geoprocessing
#' @return an object of class `arc_form_params`
#' @param params a named list with scalar character elements
#' @param x for `as_form_params()`, a named list to convert to form parameters
arc_form_params <- S7::new_class(
  "arc_form_params",
  package = "arcgisutils",
  properties = list(params = S7::class_list),
  validator = function(self) {
    lapply(
      self@params,
      check_string,
      allow_null = TRUE,
      allow_na = FALSE,
      call = rlang::caller_call()
    )
    # must return null
    return(NULL)
  }
)

#' @export
#' @family geoprocessing
#' @rdname arc_form_params
as_form_params <- function(x) {
  if (!rlang::is_list(x)) {
    cli::cli_abort("{.arg x} must be a list.")
  }

  if (!rlang::is_named(x)) {
    cli::cli_abort("{.arg x} must be a named list.")
  }

  params <- lapply(x, \(.x) yyjsonr::write_json_str(.x, auto_unbox = TRUE))
  arc_form_params(params)
}

#' @title Create a Geoprocessing Service Job
#'
#' @description
#' The `arc_gp_job` class is used to interact with Geoprocessing Services in
#' ArcGIS Online and Enterprise.
#'
#' @details
#'
#' The `arc_gp_job` uses S7 classes for the job request parameters and job status
#' via [arc_form_params()] and [arc_job_status()] respectively. Importantly,
#' [arc_form_params()] ensures that parameters provided to a geoprocessing
#' service are all character scalars as required by the form body.
#'
#' @importFrom R6 R6Class
#' @export
#' @returns
#' An object of class `arc_gp_job`.
#' @family geoprocessing
#' @rdname gp_job
#' @examples
#' url <- paste0(
#'   "https://logistics.arcgis.com/arcgis/",
#'   "rest/services/World/ServiceAreas/",
#'   "GPServer/GenerateServiceAreas"
#' )
#' job <- new_gp_job(url, list(f = "json"))
#' job
#'
#' # extract params S7 class
#' params <- job$params
#' params
#'
#' # view underlying list
#' params@params
#'
arc_gp_job <- R6::R6Class(
  "arc_gp_job",
  #' @field base_url the URL of the job service (without `/submitJob`)
  #' @field id the ID of the started job. `NULL` `self$start()` has not been called.
  #' @field params returns an S7 object of class `arc_form_params` (see [`arc_form_params()`]) the list can be accessed via `self$params@params`.
  #' @field status returns the status of the geoprocessing job as an S7 object of class `gp_job_status` (see [arc_job_status()]) by querying the `/jobs/{job-id}` endpoint.
  #' @field results returns the current results of the job by querying the `/jobs/{job-id}/results` endpoint.
  public = list(
    base_url = NULL,
    id = NULL,
    #' @param base_url the URL of the job service (without `/submitJob`)
    #' @param params a named list where each element is a scalar character
    #' @param result_fn Default `NULL`. An optional function to apply to the results JSON. By default parses results using `RcppSimdJson::fparse()`.
    #' @param token default [arc_token()]. The token to be used with the job.
    #' @param error_call default `rlang::caller_call()` the calling environment.
    initialize = function(
      base_url,
      params = list(),
      result_fn = NULL,
      token = arc_token(),
      error_call = rlang::caller_call()
    ) {
      # use S7 to validate the form parameters
      if (!is_url(base_url)) {
        cli::cli_abort("{.arg base_url} is not a valid URL.", call = error_call)
      }

      if (!rlang::is_null(result_fn)) {
        check_function(result_fn)
        private$.result_fn <- result_fn
      }

      self$base_url <- base_url
      if (inherits(params, "arcgisutils::arc_form_params")) {
        private$.params <- params
      } else {
        private$.params <- arc_form_params(params)
      }

      private$token <- token
      self
    },
    #' @description  Starts the job by calling the `/submitJob` endpoint. This also sets the public field `id`.
    start = function() {
      # TODO make it possible to only do this once
      resp <- arc_base_req(
        self$base_url,
        token = private$token,
        path = "submitJob"
      ) |>
        httr2::req_body_form(!!!private$.params@params) |>
        httr2::req_error(is_error = function(e) FALSE) |>
        httr2::req_perform()

      res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
      detect_errors(res)
      self$id <- res$jobId
      self
    },
    #' @description Cancels a job by calling the `/cancel` endpoint.
    cancel = function() {
      resp <- arc_base_req(
        self$base_url,
        token = private$token,
        path = c("jobs", self$id, "cancel"),
        query = c(f = "json")
      ) |>
        httr2::req_body_form(!!!private$.params@params) |>
        httr2::req_error(is_error = function(e) FALSE) |>
        httr2::req_perform()

      res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

      # check for errors
      detect_errors(res)

      self$status <- arc_job_status(status = res$jobStatus)
      self$id <- res$jobId
      self
    },
    #' @description Waits for job completion and returns results.
    #' @param interval polling interval in seconds (default 0.1)
    #' @param verbose whether to print status messages (default FALSE)
    await = function(interval = 0.1, verbose = FALSE) {
      if (is.null(self$id)) {
        cli::cli_abort("Job has not been started.")
      }

      is_complete <- FALSE
      Sys.sleep(0.5) # initial sleep 500ms

      while (!is_complete) {
        cur_status <- self$status@status
        if (verbose) {
          cli::cli_alert_info(
            "Job status is {.val {cur_status}}. Waiting before polling again..."
          )
        }

        is_complete <- cur_status %in%
          c("esriJobFailed", "esriJobCancelled", "esriJobSucceeded")

        if (cur_status %in% c("esriJobCancelled", "esriJobFailed")) {
          cli::cli_alert_danger(
            "Job ID {.val {self$id}} ended with {.val {cur_status}}",
            ">" = "Returning GP results"
          )
          return(self$results)
        } else if (is_complete) {
          return(self$results)
        }
        Sys.sleep(interval)
      }
    }
  ),
  private = list(
    .params = NULL,
    .status = function() {
      # if there is a NULL job ID we abort
      if (is.null(self$id)) {
        cli::cli_abort(
          c(
            "There is no job ID present.",
            ">" = " Have you started the job with `x$start()`?"
          )
        )
      }

      # check the status
      resp <- arc_base_req(
        self$base_url,
        token = private$token,
        path = c("jobs", self$id),
        query = c(f = "json")
      ) |>
        httr2::req_error(is_error = function(e) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # read the string
      res <- RcppSimdJson::fparse(resp)

      detect_errors(res)

      arc_job_status(res[["jobStatus"]])
    },
    .result_fn = NULL,
    token = NULL
  ),
  active = list(
    params = function() {
      private$.params
    },
    status = function() private$.status(),
    results = function() {
      # if there is a NULL job ID we abort
      if (is.null(self$id)) {
        cli::cli_abort(
          c(
            "There is no job ID present.",
            ">" = " Have you started the job with `x$start()`?"
          )
        )
      }

      # check the status
      resp <- arc_base_req(
        self$base_url,
        token = private$token,
        path = c("jobs", self$id, "results"),
        query = c(f = "json")
      ) |>
        httr2::req_error(is_error = function(e) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_string()

      # optional .fn arg which is used to parse the results
      # of the geoprocessing body
      if (!rlang::is_null(private$.result_fn)) {
        return(private$.result_fn(resp))
      }

      # read the string
      res <- RcppSimdJson::fparse(resp)
      detect_errors(res)

      if (!is.null(res) && is.data.frame(res)) {
        return(data_frame(res))
      } else {
        res
      }
    }
  ),
  lock_class = TRUE
)


#' @export
#' @rdname gp_job
#' @param base_url the URL of the job service (without `/submitJob`)
#' @param params a named list where each element is a scalar character
#' @param token default [arc_token()]. The token to be used with the job.
new_gp_job <- function(
  base_url,
  params = list(),
  token = arc_token()
) {
  check_string(base_url)
  if (!rlang::is_null(token)) {
    obj_check_token(token)
  }
  arc_gp_job$new(base_url, params, token)
}

#' @export
print.arc_gp_job <- function(x, ...) {
  x
  contents <- c(
    sprintf("<%s>", class(x)[1]),
    sprintf("Job ID: %s", x$id %||% "not initiated"),
    sprintf("Status: %s", x$.status %||% "not started"),
    sprintf("Resource: /%s", utils::tail(strsplit(x$base_url, "/")[[1]], 1)),
    "Params:",
    cli::cli_fmt(cli::cli_ul(names(compact(x$params@params))))
  )
  cat(contents, sep = "\n")
  invisible(x)
}
