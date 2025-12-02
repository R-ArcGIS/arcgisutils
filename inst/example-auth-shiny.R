library(shiny)
library(shinyOAuth)
library(arcgisutils)

# Simple UI
ui <- fluidPage(
  use_shinyOAuth(),
  uiOutput("login_information")
)

client <- auth_shiny()

server <- function(input, output, session) {
  auth <- shinyOAuth::oauth_module_server(
    "auth",
    client,
    auto_redirect = FALSE
  )

  output$login_information <- renderUI({
    if (auth$authenticated) {
      user_info <- auth$token@userinfo
      tagList(
        tags$p("You are logged in! Your details:"),
        tags$pre(paste(capture.output(str(user_info)), collapse = "\n"))
      )
    } else {
      tagList(
        tags$p("You are not logged in."),
        actionButton("login_btn", "Login with ArcGIS")
      )
    }
  })

  # Trigger login when button is clicked
  observeEvent(input$login_btn, {
    auth$request_login()
  })
}

runApp(
  shinyApp(ui, server),
  port = 8100,
  launch.browser = FALSE
)

# Open the app in your regular browser at http://127.0.01:8100
