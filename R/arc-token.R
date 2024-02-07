# create a package level environment that will be used to store the tokens
# This is where all tokens are going to go
token_env <- rlang::env()

#' Manage authorization tokens
#'
#' These functions are used to set, fetch, and check authorization tokens.
#'
#' @param token for `arc_token()`, the name of a token to fetch. For `set_arc_token()`,
#'   it is an `httr2_token` that will be set.
#' @param ... named arguments to set `httr2_token`. Must be valid names and must be an `httr2_token`.
#' @inheritParams cli::cli_abort
#'
#' @details
#'
#' It is possible to have multiple authorization tokens in one session. These
#' functions assist you in managing them.
#'
#' `arc_token()` is used to fetch tokens by name. The default token is `ARCGIS_TOKEN`.
#' However, they can be any valid character scalar. `set_arc_token()` will create
#' store a token with the name `ARCGIS_TOKEN`. However, you can alternatively
#' set the tokens by name using a key-value pair. The key is what you would pass
#' to `arc_token()` to fetch the `httr2_token` object.
#'
#' `obj_check_token()` is a developer oriented function that can be used to check
#' if an object is indeed an `httr2_token`. To check if a token has expired,
#' [`validate_or_refresh_token()`] will do so.
#'
#' For developers:
#'
#' `set_arc_token()` uses a package level environment to store the tokens. The
#' tokens are fetched from the environment using `arc_token()`.
#'
#' @export
#' @rdname token
#' @examples
#' # create a fake token
#' token <- httr2::oauth_token("1234")
#'
#' # set token to the default location
#' set_arc_token(token)
#'
#' # fetch token from the default location
#' arc_token()
#'
#' # set token by name
#' set_arc_token(org_a = token, org_b = token)
#'
#' # fetch token by name
#' arc_token("org_a")
arc_token <- function(token = "ARCGIS_TOKEN") {
  # returns NULL if not found
  token_env[[token]]
}


#' @export
#' @rdname token
set_arc_token <- function(token, ...) {

  # one of the two must be provided
  if (rlang::is_missing(token) && rlang::dots_n(...) == 0) {
    cli::cli_abort("Must provide {.arg token} or {.arg ...}")
  }

  # handle dots if present
  if (rlang::dots_n(...) > 0) {

    # capture dots
    dots <- rlang::list2(...)

    # check em!
    check_dots_named(dots)

    # check all tokens
    for (token in dots) {
      obj_check_token(token)
    }

    # bind tokens to token environment
    rlang::env_bind(token_env, ...)

    # NOTE Worry is being overly verbose here:
    # provide informative message on how to access
    cli::cli_alert_success("Named tokens set: {.var {rlang::names2(dots)}}\n")
    cli::cli_alert_info("Access named tokens with {.code arc_token(\"name\")}")
  }

  # if token arg is not missing set it
  if (!rlang::is_missing(token)) {
    # check if token is the right class
    obj_check_token(token)
    # set the environment ARCGIS_TOKEN
    rlang::env_bind(token_env, "ARCGIS_TOKEN" = token)
  }
}

#' @export
#' @rdname token
obj_check_token <- function(token, call = rlang::caller_env()) {
  if (!rlang::inherits_only(token, "httr2_token")) {
    cli::cli_abort(
      "{.arg token} must be an {.cls httr2_token} not {.cls {class(token)}}"
    )
  }
  invisible(token)
}
