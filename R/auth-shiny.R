#' Authenticate with Shiny
#' @export
#' @name auth_shiny
oauth_provider_arcgis <- function(
  host = arc_host()
) {
  ver <- utils::packageVersion("arcgisutils")
  arc_ver <- paste0("arcgisutils v", ver)

  oauth_provider(
    name = arc_ver,
    auth_url = paste0(host, "/sharing/rest/oauth2/authorize"),
    token_url = paste0(host, "/sharing/rest/oauth2/token"),
    token_auth_style = "body",
    userinfo_url = paste0(host, "/sharing/rest/portals/self?f=json"),
    userinfo_id_selector = function(x) x$id
  )
}

#' @param redirect_uri default `Sys.getenv("ARCGIS_REDIRECT_URI")`. The redirect URL after completing authentication flow.
#' @param ... additional arguments passed to [shinyOAuth::oauth_client()]
#' @inheritParams auth_client
#' @export
#' @name auth_shiny
auth_shiny <- function(
  client = Sys.getenv("ARCGIS_CLIENT"),
  secret = Sys.getenv("ARCGIS_SECRET"),
  redirect_uri = Sys.getenv("ARCGIS_REDIRECT_URI"),
  host = arc_host(),
  ...
) {
  dots <- rlang::list2(...)
  check_dots_named(dots)

  shinyOAuth::oauth_client(
    provider = oauth_provider_arcgis(host),
    client_id = client,
    client_secret = secret,
    redirect_uri = redirect_uri,
    !!!dots
  )
}
