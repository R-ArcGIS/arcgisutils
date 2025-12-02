#' Authenticate with Shiny
#'
#' @description
#'
#' `oath_provider_arcgis()` and `auth_shiny()` provide an integrated
#' authentication experience for `{shiny}` applications via the package
#' [shinyOAuth](https://github.com/lukakoning/shinyOAuth/). Applications that
#' use `auth_shiny()` will enable user-specific behavior and authentications.
#'
#' @details
#'
#' Authentication with `auth_shiny()` requires the package `shinyOAuth`.
#' When creating an OAuth app in ArcGIS Online / Enterprise, ensure that
#' the app has a valid redirect URI. This **must be** the same redirect URI
#' that is used by `shinyOAuth` and **must be** the same port as the
#' shiny application is served on.
#'
#' `oauth_provider_arcgis()` is a low-level provider function that mimics
#' other `oauth_provider_*()` functions from `shinyOAuth`. Use `auth_shiny()`
#' to create an OAuth client.
#'
#' **Important:** The client object returned by `auth_shiny()` must be created
#' outside of the server function to ensure a shared state across the OAuth flow.
#' Creating the client inside the server function will result in authentication failures.
#'
#' The user info returned from `auth$token@userinfo` has the same structure
#' as [arc_portal_self()]. See that function's documentation for available fields.
#'
#' The below example is derived from the shinyOAuth documentation.
#'
#' ```r
#' library(shiny)
#' library(shinyOAuth)
#' library(arcgisutils)
#'
#' # Simple UI
#' ui <- fluidPage(
#'   use_shinyOAuth(),
#'   uiOutput("login_information")
#' )
#'
#' client <- auth_shiny()
#'
#' server <- function(input, output, session) {
#'   # Set auto_redirect = FALSE for manual login
#'   auth <- shinyOAuth::oauth_module_server(
#'     "AGOL",
#'     client,
#'     auto_redirect = FALSE
#'   )
#'
#'   output$login_information <- renderUI({
#'     if (auth$authenticated) {
#'       user_info <- auth$token@userinfo
#'       tagList(
#'         tags$p("You are logged in! Your details:"),
#'         tags$pre(paste(capture.output(str(user_info)), collapse = "\n"))
#'       )
#'     } else {
#'       tagList(
#'         tags$p("You are not logged in."),
#'         actionButton("login_btn", "Login with ArcGIS")
#'       )
#'     }
#'   })
#'
#'   # Trigger login when button is clicked
#'   observeEvent(input$login_btn, {
#'     auth$request_login()
#'   })
#' }
#'
#' runApp(
#'   shinyApp(ui, server),
#'   port = 8100,
#'   launch.browser = FALSE
#' )
#'
#' # Open the app in your regular browser at http://localhost:8100
#' ```
#'
#' @export
#' @name auth_shiny
oauth_provider_arcgis <- function(
  host = arc_host()
) {
  ver <- utils::packageVersion("arcgisutils")
  arc_ver <- paste0("arcgisutils v", ver)

  rlang::check_installed("shinyOAuth")
  shinyOAuth::oauth_provider(
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
  rlang::check_installed("shinyOAuth")
  dots <- rlang::list2(...)
  check_dots_named(dots)

  shinyOAuth::oauth_client(
    provider = oauth_provider_arcgis(host),
    client_id = client,
    client_secret = secret,
    redirect_uri = redirect_uri,
    ...
  )
}
