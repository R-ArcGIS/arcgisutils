#' List Scheduled Tasks
#'
#' `arc_user_tasks()` lists the scheduled tasks owned by the signed in user.
#'
#' @param type optional. A character vector of one or more of `"ExecuteNotebook"`,
#'   `"UpdateInsightsWorkbook"`, `"ExecuteSceneCook"`, `"ExecuteWorkflowManager"`,
#'   `"ExecuteReport"`, or `"GPService"`. The portal returns only tasks of these types.
#' @param state optional. A scalar character, one of `"active"`, `"completed"`,
#'   `"disabled"`, `"failed"`, or `"paused"`. The portal returns only tasks in this state.
#' @inheritParams arc_paginate_req
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' A scheduled task runs a notebook, Insights workbook, scene cook, Workflow Manager job,
#' administrative report, or geoprocessing service on a schedule. This endpoint requires
#' ArcGIS Enterprise 10.8.1 or later or ArcGIS Online.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/all-user-scheduled-tasks/)
#' @export
#' @family portal task
#' @returns `arc_user_tasks()` returns a `data.frame` with one row per task. It converts
#'   the `created`, `updated`, `startDate`, `lastStart`, and `nextStart` columns to date-times.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' arc_user_tasks()
#' arc_user_tasks(type = "ExecuteNotebook", state = "active")
#' }
arc_user_tasks <- function(
  type = NULL,
  state = NULL,
  page_size = 50,
  max_pages = Inf,
  .progress = TRUE,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_token_has_user(token, call = error_call)

  if (!is.null(type)) {
    type <- rlang::arg_match(
      type,
      c(
        "ExecuteNotebook",
        "UpdateInsightsWorkbook",
        "ExecuteSceneCook",
        "ExecuteWorkflowManager",
        "ExecuteReport",
        "GPService"
      ),
      multiple = TRUE,
      error_call = error_call
    )
    type <- paste0(type, collapse = ",")
  }

  if (!is.null(state)) {
    state <- rlang::arg_match(
      state,
      c("active", "completed", "disabled", "failed", "paused"),
      error_call = error_call
    )
  }

  resps <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks"
    ),
    error_call = error_call
  ) |>
    httr2::req_url_query(
      !!!compact(list(f = "json", types = type, taskState = state))
    ) |>
    arc_paginate_req(
      page_size = page_size,
      max_pages = max_pages,
      .progress = .progress
    )

  tasks <- lapply(resps, function(.resp) {
    resp <- httr2::resp_body_string(.resp) |>
      RcppSimdJson::fparse() |>
      detect_errors(error_call = error_call)
    resp[["tasks"]]
  })

  res <- data_frame(rbind_results(tasks))

  for (col in c("created", "updated", "startDate", "lastStart", "nextStart")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
}

#' Create Scheduled Task
#'
#' `arc_create_task()` creates a scheduled task owned by the signed in user.
#'
#' @param item a `PortalItem` from [`arc_item()`] or a scalar character item ID of the notebook,
#'   workbook, or other item that the task runs. Pass `NULL` for `"ExecuteReport"` tasks, which
#'   run on no item.
#' @param type a scalar character, one of `"ExecuteNotebook"`, `"UpdateInsightsWorkbook"`,
#'   `"ExecuteSceneCook"`, `"ExecuteWorkflowManager"`, `"ExecuteReport"`, or `"GPService"`.
#'   The value sets the type of task that `arc_create_task()` creates.
#' @param schedule a scalar character cron expression with 5 fields: minute, hour, day of month,
#'   month, and day of week, such as `"0 6 ? * 2"`. The portal reads hours and days in UTC.
#'   Use 1 for Sunday through 7 for Saturday in the day of week field.
#' @param title optional. A scalar character title for the task.
#' @param parameters optional. A named list that `arc_create_task()` sends as the task's JSON
#'   parameters. Required for `"ExecuteSceneCook"`, `"ExecuteReport"`, and `"GPService"`. For
#'   `"GPService"`, supply the geoprocessing tool's inputs.
#' @param task_url optional. A scalar character URL of an asynchronous geoprocessing service task
#'   on a federated server. Required for `"GPService"`.
#' @param start_date,end_date optional. A `Date` or `POSIXct`. `start_date` sets when the task
#'   starts, and `end_date` sets when it stops running.
#' @param max_occurrences optional. A scalar integer, the maximum number of times the task runs.
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The portal requires ArcGIS Enterprise 10.8.1 or later or ArcGIS Online. Call
#' `arc_user_tasks()` to list the created tasks.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/create-new-scheduled-task/)
#' @export
#' @family portal task
#' @returns `arc_create_task()` returns a list with the elements `success` and `taskId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2", title = "Weekly refresh")
#' }
arc_create_task <- function(
  item,
  type,
  schedule,
  title = NULL,
  parameters = NULL,
  task_url = NULL,
  start_date = NULL,
  end_date = NULL,
  max_occurrences = NULL,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_token_has_user(token, call = error_call)

  type <- rlang::arg_match(
    type,
    c(
      "ExecuteNotebook",
      "UpdateInsightsWorkbook",
      "ExecuteSceneCook",
      "ExecuteWorkflowManager",
      "ExecuteReport",
      "GPService"
    ),
    error_call = error_call
  )

  check_string(schedule, allow_empty = FALSE, call = error_call)
  check_string(title, allow_null = TRUE, call = error_call)
  check_string(
    task_url,
    allow_null = TRUE,
    allow_empty = FALSE,
    call = error_call
  )
  check_number_whole(
    max_occurrences,
    min = 1,
    allow_null = TRUE,
    call = error_call
  )

  cron <- strsplit(trimws(schedule), "\\s+")[[1]]

  if (length(cron) != 5L) {
    cli::cli_abort(
      "{.arg schedule} must have 5 fields: minute, hour, day of month, month, and day of week.",
      call = error_call
    )
  }

  if (type == "GPService" && is.null(task_url)) {
    cli::cli_abort(
      "Supply {.arg task_url} when {.arg type} is {.val GPService}.",
      call = error_call
    )
  }

  if (
    type %in%
      c("ExecuteSceneCook", "ExecuteReport", "GPService") &&
      is.null(parameters)
  ) {
    cli::cli_abort(
      "Supply {.arg parameters} when {.arg type} is {.val {type}}.",
      call = error_call
    )
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      "createTask"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      !!!compact(list(
        f = "json",
        type = type,
        itemId = if (!is.null(item)) {
          item_id(item, call = error_call)
        },
        title = title,
        taskURL = task_url,
        parameters = if (!is.null(parameters)) {
          yyjsonr::write_json_str(parameters, auto_unbox = TRUE)
        },
        minute = cron[1],
        hour = cron[2],
        dayOfMonth = cron[3],
        month = cron[4],
        dayOfWeek = cron[5],
        startDate = if (!is.null(start_date)) {
          date_to_ms(start_date)
        },
        endDate = if (!is.null(end_date)) {
          date_to_ms(end_date)
        },
        maxOccurrences = max_occurrences
      ))
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Get Scheduled Task
#'
#' `arc_task()` gets one scheduled task owned by the signed in user.
#'
#' @param task a scalar character task ID, such as the `taskId` from [arc_create_task()] or the
#'   `id` column of [arc_user_tasks()].
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/get-specific-task/)
#' @export
#' @family portal task
#' @returns `arc_task()` returns a list with the elements `id`, `itemId`, `type`, `title`, `userId`,
#'   `cronSchedule`, `parameters`, `active`, and `taskState`, among others. It converts the
#'   `created`, `updated`, `startDate`, `lastStart`, and `nextStart` elements to date-times.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' task <- arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2")
#' arc_task(task$taskId)
#' }
arc_task <- function(
  task,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  res <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  for (col in c("created", "updated", "startDate", "lastStart", "nextStart")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
}

#' Delete a Scheduled Task
#'
#' `arc_task_delete()` deletes a scheduled task owned by the signed in user.
#'
#' @inheritParams arc_task
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/delete-task/)
#' @export
#' @family portal task
#' @returns `arc_task_delete()` returns a list with the elements `success` and `taskId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' task <- arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2")
#' arc_task_delete(task$taskId)
#' }
arc_task_delete <- function(
  task,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task,
      "delete"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Disable a Scheduled Task
#'
#' `arc_task_disable()` stops a scheduled task owned by the signed in user from running until [arc_task_enable()] turns it back on.
#'
#' @inheritParams arc_task
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/disable-task/)
#' @export
#' @family portal task
#' @returns `arc_task_disable()` returns a list with the elements `success` and `taskId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' task <- arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2")
#' arc_task_disable(task$taskId)
#' }
arc_task_disable <- function(
  task,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task,
      "disable"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Enable a Scheduled Task
#'
#' `arc_task_enable()` turns a scheduled task owned by the signed in user back on after [arc_task_disable()] stopped it.
#'
#' @inheritParams arc_task
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/enable-task/)
#' @export
#' @family portal task
#' @returns `arc_task_enable()` returns a list with the elements `success` and `taskId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' task <- arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2")
#' arc_task_disable(task$taskId)
#' arc_task_enable(task$taskId)
#' }
arc_task_enable <- function(
  task,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task,
      "enable"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(f = "json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' Update a Scheduled Task
#'
#' `arc_task_update()` replaces the settings of a scheduled task owned by the signed in user.
#'
#' @inheritParams arc_task
#' @inheritParams arc_create_task
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#'
#' The API reference lists `item`, `type`, and `schedule` as required on every update, so `arc_task_update()` takes them even when they stay the same. The API reference lists `parameters` as required only for `"ExecuteReport"` tasks. Call [arc_task()] to read the current settings first.
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/edit-existing-task/)
#' @export
#' @family portal task
#' @returns `arc_task_update()` returns a list with the elements `success` and `taskId`.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' task <- arc_create_task("029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 6 ? * 2")
#' arc_task_update(task$taskId, "029b61ca770e428d9468a2a55f40b9fb", "ExecuteNotebook", "0 18 ? * 2", title = "Weekly evening refresh")
#' }
arc_task_update <- function(
  task,
  item,
  type,
  schedule,
  title = NULL,
  parameters = NULL,
  start_date = NULL,
  end_date = NULL,
  max_occurrences = NULL,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  type <- rlang::arg_match(
    type,
    c(
      "ExecuteNotebook",
      "UpdateInsightsWorkbook",
      "ExecuteSceneCook",
      "ExecuteWorkflowManager",
      "ExecuteReport",
      "GPService"
    ),
    error_call = error_call
  )

  check_string(schedule, allow_empty = FALSE, call = error_call)
  check_string(title, allow_null = TRUE, call = error_call)
  check_number_whole(
    max_occurrences,
    min = 1,
    allow_null = TRUE,
    call = error_call
  )

  cron <- strsplit(trimws(schedule), "\\s+")[[1]]

  if (length(cron) != 5L) {
    cli::cli_abort(
      "{.arg schedule} must have 5 fields: minute, hour, day of month, month, and day of week.",
      call = error_call
    )
  }

  if (type == "ExecuteReport" && is.null(parameters)) {
    cli::cli_abort(
      "Supply {.arg parameters} when {.arg type} is {.val ExecuteReport}.",
      call = error_call
    )
  }

  arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task,
      "update"
    ),
    error_call = error_call
  ) |>
    httr2::req_body_form(
      !!!compact(list(
        f = "json",
        type = type,
        itemId = if (!is.null(item)) {
          item_id(item, call = error_call)
        },
        title = title,
        parameters = if (!is.null(parameters)) {
          yyjsonr::write_json_str(parameters, auto_unbox = TRUE)
        },
        minute = cron[1],
        hour = cron[2],
        dayOfMonth = cron[3],
        month = cron[4],
        dayOfWeek = cron[5],
        startDate = if (!is.null(start_date)) {
          date_to_ms(start_date)
        },
        endDate = if (!is.null(end_date)) {
          date_to_ms(end_date)
        },
        maxOccurrences = max_occurrences
      ))
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)
}

#' List the Runs of a Scheduled Task
#'
#' `arc_task_history()` lists the past runs of a scheduled task owned by the signed in user.
#'
#' @inheritParams arc_task
#' @inheritParams delete_items
#' @inheritParams arc_base_req
#' @details
#' `r lifecycle::badge("experimental")`
#' @references [API Reference](https://developers.arcgis.com/rest/users-groups-and-items/runs-for-task/)
#' @export
#' @family portal task
#' @returns `arc_task_history()` returns a `data.frame` with one row per run and the columns
#'   `runId`, `url`, `status`, `result`, `created`, and `modified`. The portal reports the job
#'   output in `result` as a JSON string. `arc_task_history()` converts `created` and `modified`
#'   to date-times and drops the `task` column because [arc_task()] returns the same task.
#' @examples
#' \dontrun{
#' set_arc_token(auth_user())
#'
#' tasks <- arc_user_tasks()
#' arc_task_history(tasks$id[1])
#' }
arc_task_history <- function(
  task,
  ...,
  token = arc_token(),
  error_call = rlang::caller_env()
) {
  rlang::check_dots_empty()
  check_string(task, allow_empty = FALSE, call = error_call)
  check_token_has_user(token, call = error_call)

  resp <- arc_base_req(
    token[["arcgis_host"]],
    token,
    path = c(
      "sharing",
      "rest",
      "community",
      "users",
      token[["username"]],
      "tasks",
      task,
      "runs"
    ),
    query = c(f = "json"),
    error_call = error_call
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    RcppSimdJson::fparse() |>
    detect_errors(error_call = error_call)

  res <- data_frame(resp[["runs"]])
  res[["task"]] <- NULL

  for (col in c("created", "modified")) {
    if (!is.null(res[[col]])) {
      res[[col]] <- from_esri_date(res[[col]])
    }
  }

  res
}
