# Code flow ---------------------------------------------------------------


#' Authorization
#'
#' Authorize your R session to connect to an ArcGIS Portal. See details.
#'
#' @details
#'
#' ArcGIS Online and Enterprise Portals utilize OAuth2 authorization via their REST APIs.
#'
#' - `auth_code()` is the recommend OAuth2 workflow for interactive sessions
#' - `auth_client()` is the recommended OAuth2 workflow for non-interactive sessions
#' - `auth_user()` uses legacy username and password authorization using the `generateToken` endpoint. It is only recommended for legacy systems that do not implement OAuth2.
#' - `auth_binding()` fetches a token from the active portal set by `arcgisbinding`. Uses `arcgisbinding::arc.check_portal()` to extract the authorization token. Recommended if using arcgisbinding.
#'
#' @param client an OAuth 2.0 developer application client ID. By default uses the
#'  environment variable `ARCGIS_CLIENT`.
#' @param secret an OAuth 2.0 developer application secret. By default uses the environment
#'   variable `ARCGIS_SECRET`.
#' @param host default `"https://www.arcgis.com"`
#' @param expiration the duration of the token in minutes.
#' @inheritParams cli::cli_abort
#'
#' @rdname auth
#' @export
#' @examples
#' \dontrun{
#' auth_code()
#' auth_client()
#' auth_user()
#' auth_key()
#' auth_binding()
#' }
#' @returns an `httr2_token`
auth_code <- function(
    client = Sys.getenv("ARCGIS_CLIENT"),
    host = arc_host()) {
  check_string(client, allow_empty = FALSE)
  check_string(host, allow_empty = FALSE)

  token_url <- paste(host, "sharing", "rest", "oauth2", "token", sep = "/")

  auth_url <- paste(host, "sharing", "rest", "oauth2", "authorize", sep = "/")


  client <- httr2::oauth_client(
    id = client,
    token_url = token_url,
    name = "arcgisutils"
  )

  auth_url <- httr2::oauth_flow_auth_code_url(
    client,
    auth_url = auth_url,
    redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
  )

  # open browser
  utils::browseURL(auth_url)

  {
    code <- readline(prompt = "Enter code: ")
  }

  # use the internal function from httr2 to complete the token
  # transfer
  fx <- utils::getFromNamespace("oauth_client_get_token", "httr2")

  res <- fx(
    client,
    grant_type = "authorization_code",
    code = code,
    redirect_uri = "urn:ietf:wg:oauth:2.0:oob"
  )

  # add host to the token
  res[["arcgis_host"]] <- host
  res
}
# Client auth -------------------------------------------------------------
#' @export
#' @rdname auth
auth_client <- function(
    client = Sys.getenv("ARCGIS_CLIENT"),
    secret = Sys.getenv("ARCGIS_SECRET"),
    host = arc_host(),
    expiration = 120) {
  check_string(client, allow_empty = FALSE)
  check_string(secret, allow_empty = FALSE)
  check_string(host, allow_empty = FALSE)
  check_number_whole(expiration, min = 5, max = 20160)

  # https://developers.arcgis.com/documentation/mapping-apis-and-services/security/application-credentials/
  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )

  cln <- httr2::oauth_client(
    client,
    token_url,
    secret,
    name = "arcgisutils"
  )

  res <- httr2::oauth_flow_client_credentials(
    cln,
    token_params = list(expiration = expiration)
  )

  # add host to the token
  res[["arcgis_host"]] <- host
  res
}


# arcgisbinding -----------------------------------------------------------

#' @export
#' @rdname auth
auth_binding <- function() {
  rlang::check_installed("arcgisbinding")
  tkn <- utils::getFromNamespace("arc.check_portal", "arcgisbinding")()
  res <- httr2::oauth_token(tkn[["token"]])
  # add host to the token
  res[["arcgis_host"]] <- tkn[["url"]]
  # add the username to the token as well
  res[["username"]] <- tkn[["user"]]
  res
}



# legacy auth -------------------------------------------------------------


# uses request ip does not support other forms
# use oauth as the recommended approach
#' @export
#' @rdname auth
#' @param username default `Sys.getenv("ARCGIS_USER")`.
#'  Your username to login. **Do not** hard code this value.
#' @param password default `Sys.getenv("ARCGIS_PASSWORD")`.
#'   Your password to login. **Do not** hard code this value.
auth_user <- function(
    username = Sys.getenv("ARCGIS_USER"),
    password = Sys.getenv("ARCGIS_PASSWORD"),
    host = arc_host(),
    expiration = 60) {
  check_string(username, allow_empty = FALSE)
  check_string(password, allow_empty = FALSE)
  check_string(host, allow_empty = FALSE)
  check_number_whole(expiration, min = 5, max = 20160)

  if (expiration > 21600) {
    cli::cli_abort("{.arg expiration} cannot be more than 15 days (21600)")
  }

  burl <- paste0(host, "/sharing/rest/generateToken")
  b_req <- httr2::request(burl)

  req <- httr2::req_body_form(
    b_req,
    username = username,
    password = password,
    client = "referer",
    referer = host,
    expiration = expiration,
    f = "json"
  )

  # set the user agent
  req <- arc_agent(req)

  # perform request
  resp <- httr2::req_perform(req)

  # fetch token
  token_raw <- httr2::resp_body_string(resp)

  # parse the response
  token <- RcppSimdJson::fparse(token_raw)

  # detect the errors
  detect_errors(token)

  # return the token
  httr2::oauth_token(
    token[["token"]],
    expires_in = expiration,
    username = username,
    arcgis_host = host,
  )
}



# API Key ----------------------------------------------------------------

#' @export
#' @rdname auth
auth_key <- function(api_key = Sys.getenv("ARCGIS_API_KEY"), host = arc_host()) {
  check_string(
    api_key,
    allow_empty = FALSE,
    allow_na = FALSE,
    allow_null = FALSE
  )

  httr2::oauth_token(
    api_key,
    arcgis_host = host
  )
}

# refreshment mmm tasty --------------------------------------------------------

#' @param token an `httr2_token` as created by `auth_code()` or similar
#' @rdname auth
#' @export
refresh_token <- function(
    token,
    client = Sys.getenv("ARCGIS_CLIENT"),
    host = arc_host()) {
  # validate the object is a token
  obj_check_token(token)
  check_string(client, allow_empty = FALSE)

  # extract host from token
  host <- token[["arcgis_host"]]

  token_url <- paste(
    host,
    "sharing",
    "rest",
    "oauth2",
    "token",
    sep = "/"
  )


  cln <- httr2::oauth_client(client, token_url, name = "arcgisutils")

  # get the current time
  cur_time <- as.numeric(Sys.time())

  if (is.null(token[["refresh_token"]])) {
    cli::cli_abort("{.arg token} has expired and no {.field refresh_token} available")
  } else
  # if it has a refresh check to see if refresh hasn't expired
  if ((cur_time + token[["refresh_token_expires_in"]]) < cur_time) {
    cli::cli_abort("Token's {.field refresh_token} has expired.")
  }

  # should be able to refresh, go ahead.
  res <- httr2::oauth_flow_refresh(cln, token[["refresh_token"]])

  # set the host back into the token
  # Note we don't do this for username because only `auth_code()` provides
  # a refresh token
  res[["arcgis_host"]] <- host
  res
}

#' @rdname auth
#' @export
#' @param refresh_threshold default `0`. If token expiry is within this
#'  threshold (in seconds) the token will be refreshed only if a
#'  `refresh_token` is available. Token refreshing is only possible with
#'  `auth_code()` flow.
validate_or_refresh_token <- function(
    token,
    client = Sys.getenv("ARCGIS_CLIENT"),
    host = arc_host(),
    refresh_threshold = 0,
    call = rlang::caller_env()) {
  # validate the object is a token
  obj_check_token(token, call = call)
  check_string(client, allow_empty = FALSE)
  check_string(host, allow_empty = FALSE)
  check_number_whole(refresh_threshold, min = 0, max = 3600)

  cur_time <- as.numeric(Sys.time())
  # check if token is expired or expires within threshold
  # the idea being if the token is going to expire soon, refresh it
  if (token[["expires_at"]] - refresh_threshold <= cur_time) {
    # if it is refresh the token
    token <- refresh_token(client, host, token)
  } else {
    # if not return token
    token
  }
}

#' Determines Portal Host
#'
#' Returns a scalar character indicating the host to make requests to.
#'
#' By default, the host is ArcGIS Online <`https://www.arcgis.com`>. If the
#' environment variable `ARCGIS_HOST` is set, it will be returned.
#'
#' @export
#' @examples
#' arc_host()
#' @returns
#' A scalar character, `"https://www.arcgis.com"` by default.
arc_host <- function() {
  host <- Sys.getenv("ARCGIS_HOST")
  check_string(host, allow_empty = TRUE)
  if (host == "") {
    host <- "https://www.arcgis.com"
  }

  host
}
