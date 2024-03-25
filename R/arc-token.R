# create a package level environment that will be used to store the tokens
# This is where all tokens are going to go
token_env <- rlang::env()

#' Manage authorization tokens
#'
#' These functions are used to set, fetch, and check authorization tokens.
#'
#' @param token for `arc_token()`, the name of a token to fetch. For `set_arc_token()`,
#'   it is an `httr2_token` that will be set. For `unset_arc_token()`, a character
#'   vector of token names to be unset.
#' @param ... named arguments to set `httr2_token`. Must be valid names and must be an `httr2_token`.
#' @inheritParams cli::cli_abort
#' @details
#'
#' It is possible to have multiple authorization tokens in one session. These
#' functions assist you in managing them.
#'
#' `arc_token()` is used to fetch tokens by name. The default token is `ARCGIS_TOKEN`.
#' However, they can be any valid character scalar. `set_arc_token()` will create
#' store a token with the name `ARCGIS_TOKEN`. However, you can alternatively
#' set the tokens by name using a key-value pair. The key is what you would pass
#' to `arc_token()` to fetch the `httr2_token` object. To remove a token that has
#' been set, use `unset_arc_token()`.
#'
#' `obj_check_token()` is a developer oriented function that can be used to check
#' if an object is indeed an `httr2_token`. To check if a token has expired,
#' [`validate_or_refresh_token()`] will do so.
#'
#' `check_token_has_user()` is a developer oriented function that checks to see
#' if a token has a `username` field associated with it.
#'
#' For developers:
#'
#' `set_arc_token()` uses a package level environment to store the tokens. The
#' tokens are fetched from the environment using `arc_token()`.
#'
#' @export
#' @rdname token
#' @examples
#' # create fake tokens
#' token_a <- httr2::oauth_token("1234", arcgis_host = arc_host())
#' token_b <- httr2::oauth_token("abcd", arcgis_host = arc_host())
#'
#' # set token to the default location
#' set_arc_token(token_a)
#'
#' # fetch token from the default location
#' arc_token()
#'
#' # set token by name
#' set_arc_token(org_a = token_a, org_b = token_b)
#'
#' # fetch token by name
#' arc_token("org_a")
#' arc_token("org_b")
#'
#' # unset tokens
#' unset_arc_token()
#' unset_arc_token(c("org_a", "org_b"))
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

  # if token arg is not missing set it
  # note that this must come before the for loop because it
  # adjusts the scoped variable `token`
  if (!rlang::is_missing(token)) {
    # check if token is the right class
    obj_check_token(token)
    # set the environment ARCGIS_TOKEN
    rlang::env_bind(token_env, "ARCGIS_TOKEN" = token)
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

}

#' @rdname token
#' @export
unset_arc_token <- function(token = NULL) {
  check_character(token, allow_null = TRUE)
  if (is.null(token)) {
    token <- "ARCGIS_TOKEN"
    rlang::env_unbind(token_env, token)
  } else {
    rlang::env_unbind(token_env, token)
  }
  cli::cli_alert_success("Token{?s} {.var {token}} {?has/have} been unset.")
}

#' @export
#' @rdname token
obj_check_token <- function(token, call = rlang::caller_env()) {
  # if object is not a httr2 token
  if (!rlang::inherits_only(token, "httr2_token")) {
    cli::cli_abort(
      "{.arg token} must be an {.cls httr2_token} not {.cls {class(token)}}",
      call = call
    )
  }

  # if no host is set
  if (is.null(token[["arcgis_host"]])) {
    cli::cli_abort(
      c("{.arg token} does not have {.val arcgis_host}.",
        "i" = "was your token created using {.pkg arcgisutils}?"
      ),
      call = call
    )
  }

  # if more than one host is set, error
  if (length(token[["arcgis_host"]]) > 1) {
    cli::cli_abort(
      c("{.arg token} has more than one {.val arcgis_host}.",
        "i" = "was your token created using {.pkg arcgisutils}?"
      ),
      call = call
    )
  }

  invisible(token)
}

#' @export
#' @rdname token
check_token_has_user <- function(token, call = rlang::caller_env()) {
  obj_check_token(token, call = call)
  if (is.null(token[["username"]])) {
    cli::cli_abort(
      c(
        "{.arg token} does not have an associated {.val username}."
      )
    )
  }
  invisible(token)
}
