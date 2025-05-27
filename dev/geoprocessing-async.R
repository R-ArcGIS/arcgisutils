#' @export
poll_gp_job <- coro::async(
function(
  job,
  on_completion = identity,
  poll_interval = 0.5,
  verbose = TRUE,
  error_call = rlang::caller_call(),
  error_arg = rlang::caller_arg(job)
) {
  if (is.null(job$id)) {
    cli::cli_abort("Job has not been started.", call = error_call)
  }
  if (!inherits(job, "arc_gp_job")) {
    cli::cli_abort("{.arg {error_arg}} is not an {.cls gp_job} object. Found {obj_type_friendly(job)}", call = error_call)
  }

  check_function(on_completion)
  if (length(formals(on_completion)) != 1) {
    cli::cli_abort(
      "The function provided to {.arg on_completion} must have only one argument",
      call = error_call
    )
  }

  is_complete <- FALSE
  coro::await(coro::async_sleep(0.5)) # initial sleep 500ms
  while (!is_complete) {
    cur_status <- job$status@status
    if (verbose) {
      cli::cli_alert_info("Job status is {.val {cur_status}}. Waiting before polling again...")
    }

    is_complete <- cur_status %in% c("esriJobFailed", "esriJobCancelled", "esriJobSucceeded")
    if (cur_status %in% c("esriJobCancelled", "esriJobFailed")) {
      cli::cli_alert_danger(
        "Job ID {.val {job$id}} ended with {.val {cur_status}}",
        ">" = "Returning GP results",
        call = error_call
      )
      return(job$results)
    } else if (is_complete) {
      res <- on_completion(job)
      return(res)
    }
    coro::await(coro::async_sleep(poll_interval))
  }
})
