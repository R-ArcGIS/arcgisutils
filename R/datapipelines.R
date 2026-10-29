#' List Data Pipeline Runs
#'
#' `arc_pipeline_list()` lists the data pipeline runs that the signed in user
#' can see.
#'
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' `arc_pipeline_list()` and the other `arc_pipeline_*()` functions read the
#' data pipelines host and the organization ID from [`arc_portal_self()`].
#' @export
#' @family data pipelines
#' @returns `arc_pipeline_list()` returns a `data.frame` with one row per run
#'   and the columns `id`, `itemId`, `username`, `computeResourceId`,
#'   `status`, `timeoutInMinutes`, and a timestamp column for each state a
#'   run reached, such as `submittedAt`. `arc_pipeline_list()` leaves the
#'   timestamps as ISO 8601 strings. Zero rows if there are none.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' arc_pipeline_list()
#' }
arc_pipeline_list <- function(
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  check_token_has_user(token, call = error_call)
  self <- arc_portal_self(token, error_call = error_call)

  resp <- arc_base_req(
    self[["helperServices"]][["dataPipelines"]][["url"]],
    token,
    path = c(
      self[["id"]],
      "arcgis",
      "rest",
      "services",
      "datapipelines",
      "runs"
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  if (length(resp[["results"]]) == 0L) {
    return(data_frame(data.frame()))
  }

  data_frame(resp[["results"]])
}

#' Run a Data Pipeline
#'
#' `arc_pipeline_submit()` starts a new run of a data pipeline item.
#'
#' @param pipeline a `PortalItem` from [`arc_item()`] or a scalar character
#'   item ID of a Data Pipeline item.
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' Each run consumes credits until it finishes. Call [`arc_pipeline_run()`]
#' to check the status of the run and [`arc_pipeline_cancel()`] to stop it.
#' @export
#' @family data pipelines
#' @returns `arc_pipeline_submit()` returns a `PipelineRun` object with the
#'   properties `id`, `itemId`, `username`, `computeResourceId`,
#'   `timeoutInMinutes`, `status`, and `submittedAt`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' run <- arc_pipeline_submit("867b967c125a4c75a3777fdcd219875d")
#' arc_pipeline_run(run)
#' }
arc_pipeline_submit <- function(
  pipeline,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  id <- item_id(pipeline, call = error_call)
  check_token_has_user(token, call = error_call)
  self <- arc_portal_self(token, error_call = error_call)

  arc_base_req(
    self[["helperServices"]][["dataPipelines"]][["url"]],
    token,
    path = c(
      self[["id"]],
      "arcgis",
      "rest",
      "services",
      "datapipelines",
      "runs"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(itemId = id, f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call) |>
    from_vector()
}

#' Get a Data Pipeline Run
#'
#' `arc_pipeline_run()` gets the details of one data pipeline run, including
#' its status.
#'
#' @param run a `PipelineRun` from [`arc_pipeline_submit()`] or
#'   [`arc_pipeline_run()`], or a scalar character run ID such as the `id`
#'   column of [`arc_pipeline_list()`].
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' ArcGIS returns any user's run to an administrator. Other users can only
#' get their own runs.
#' @export
#' @family data pipelines
#' @returns `arc_pipeline_run()` returns a `PipelineRun` object with the
#'   properties `id`, `itemId`, `username`, `timeoutInMinutes`,
#'   `computeResourceId`, `status`, and a timestamp for each state the run
#'   reached, such as `submittedAt` and `completedAt`. `taskId` and
#'   `taskRunId` are `NA` unless a scheduled task started the run.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' run <- arc_pipeline_submit("867b967c125a4c75a3777fdcd219875d")
#' arc_pipeline_run(run)
#' }
arc_pipeline_run <- function(
  run,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  if (S7::S7_inherits(run, PipelineRun)) {
    run <- run@id
  }

  check_string(run, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)
  self <- arc_portal_self(token, error_call = error_call)

  arc_base_req(
    self[["helperServices"]][["dataPipelines"]][["url"]],
    token,
    path = c(
      self[["id"]],
      "arcgis",
      "rest",
      "services",
      "datapipelines",
      "runs",
      run
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call) |>
    from_vector()
}

#' Cancel a Data Pipeline Run
#'
#' `arc_pipeline_cancel()` asks ArcGIS to cancel a data pipeline run.
#'
#' @inheritParams arc_pipeline_run
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' ArcGIS accepts the request and cancels the run asynchronously. Call
#' [`arc_pipeline_run()`] to check the status of the run.
#' @export
#' @family data pipelines
#' @returns `arc_pipeline_cancel()` returns the `httr2_response`. ArcGIS
#'   responds with `202 Accepted` and an empty body.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' run <- arc_pipeline_submit("867b967c125a4c75a3777fdcd219875d")
#' arc_pipeline_cancel(run)
#' }
arc_pipeline_cancel <- function(
  run,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  if (S7::S7_inherits(run, PipelineRun)) {
    run <- run@id
  }

  check_string(run, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)
  self <- arc_portal_self(token, error_call = error_call)

  arc_base_req(
    self[["helperServices"]][["dataPipelines"]][["url"]],
    token,
    path = c(
      self[["id"]],
      "arcgis",
      "rest",
      "services",
      "datapipelines",
      "runs",
      run,
      "cancel"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform()
}

#' Get the Results of a Data Pipeline Run
#'
#' `arc_pipeline_results()` gets the outcome of a data pipeline run,
#' including its status, result items, and the messages the run wrote.
#'
#' @inheritParams arc_pipeline_run
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @export
#' @family data pipelines
#' @returns `arc_pipeline_results()` returns a list with the elements `id`,
#'   `status`, `messages`, and `version`, among others. A failed run also has
#'   a `failure` element with the error `code` and `message`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' run <- arc_pipeline_submit("867b967c125a4c75a3777fdcd219875d")
#' arc_pipeline_results(run)
#' }
arc_pipeline_results <- function(
  run,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  if (S7::S7_inherits(run, PipelineRun)) {
    run <- run@id
  }

  check_string(run, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)
  self <- arc_portal_self(token, error_call = error_call)

  res <- arc_base_req(
    self[["helperServices"]][["dataPipelines"]][["url"]],
    token,
    path = c(
      self[["id"]],
      "arcgis",
      "rest",
      "services",
      "datapipelines",
      "runs",
      run,
      "result"
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  res[["messages"]] <- data_frame(res[["messages"]])
  res[["results"]][["outputs"]] <- data_frame(res[["results"]][["outputs"]])
  res
}


# represents a single pipeline run
PipelineRun <- S7::new_class(
  "PipelineRun",
  package = "arcgisutils",
  properties = list(
    id = s7x::class_string,
    username = s7x::class_string,
    computeResourceId = s7x::class_string,
    status = s7x::class_string,
    itemId = s7x::class_string,
    timeoutInMinutes = s7x::class_int,
    submittedAt = s7x::class_string,
    waitingAt = s7x::class_string,
    runningAt = s7x::class_string,
    completedAt = s7x::class_string,
    terminatedAt = s7x::class_string,
    taskId = s7x::class_string,
    taskRunId = s7x::class_string,
    machineName = s7x::class_string
  )
)

from_vector <- function(x, error_call = rlang::caller_env()) {
  rlang::inject(PipelineRun(!!!x), env = error_call)
}
