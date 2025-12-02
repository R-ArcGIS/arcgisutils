auth_shiny <- function(
  client = Sys.getenv("ARCGIS_CLIENT"),
  secret = Sys.getenv("ARCGIS_SECRET"),
  host = arc_host(),
  redirect_uri = Sys.getenv("ARCGIS_REDIRECT_URI"),
  ...
) {
  ver <- utils::packageVersion("arcgisutils")
  arc_ver <- paste0("arcgisutils v", ver)

  provider <- oauth_provider(
    name = arc_ver,
    auth_url = paste0(host, "/sharing/rest/oauth2/authorize"),
    token_url = paste0(host, "/sharing/rest/oauth2/token"),
    token_auth_style = "body",
    userinfo_url = paste0(host, "/sharing/rest/portals/self?f=json"),
    userinfo_id_selector = function(x) x$id
  )

  shinyOAuth::oauth_client(
    provider = provider,
    client_id = client,
    client_secret = secret,
    redirect_uri = redirect_uri
  )
}

# library(shiny)
# library(shinyOAuth)
# library(arcgisutils)
# # Simple UI
# ui <- fluidPage(
#   use_shinyOAuth(),
#   uiOutput("login_information")
# )

# client <- auth_shiny_provider()
# oauth_provider_arcgis()

# server <- function(input, output, session) {

#   # Set auto_redirect = FALSE for manual login
#   auth <- shinyOAuth::oauth_module_server(
#     "auth",
#     client,
#     auto_redirect = FALSE
#   )

#   output$login_information <- renderUI({
#     if (auth$authenticated) {
#       user_info <- auth$token@userinfo
#       tagList(
#         tags$p("You are logged in! Your details:"),
#         tags$pre(paste(capture.output(str(user_info)), collapse = "\n"))
#       )
#     } else {
#       tagList(
#         tags$p("You are not logged in."),
#         actionButton("login_btn", "Login with ArcGIS")
#       )
#     }
#   })

#   # Trigger login when button is clicked
#   observeEvent(input$login_btn, {
#     auth$request_login()
#   })
# }

# runApp(
#   shinyApp(ui, server),
#   port = 8100,
#   launch.browser = FALSE
# )

# # Open the app in your regular browser at http://127.0.01:8100
# # (viewers in RStudio/Positron/etc. cannot perform necessary redirects)
