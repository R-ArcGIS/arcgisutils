#' General utility functions
#'
#' @details
#'
#' - `compact()` removes any `NULL` list elements
#' - `%||%` is a special pipe operator that returns `b` if `a` is `NULL`
#'
#' @examples
#'
#' # remove null elements
#' compact(list(a = NULL, b = 1))
#'
#' # if NULL return rhs
#' NULL %||% 123
#'
#' # if not NULL return lhs
#' 123 %||% NULL
#'
#' @param .x a list
#' @export
#' @rdname utilities
#' @returns
#' - `compact()` a list
#' - `%||%` the first non-null item or `NULL` if both are `NULL`
compact <- function(.x) Filter(length, .x)

#' @export
#' @rdname utilities
#' @param a an R object
#' @param b an R object
`%||%` <- function(a, b) if (is.null(a)) b else a


#' @param dots a list collected from dots via `rlang::list2(...)`
#' @param call default `rlang::caller_env()`. The caller environment passed to `cli::cli_abort()`
#' @export
#' @rdname utilities
check_dots_named <- function(dots, call = rlang::caller_env()) {
  if (!rlang::is_named2(dots)) {
    cli::cli_abort(
      "All arguments provided to {.arg ...} must be named",
      call = call
    )
  }
  invisible(dots)
}
