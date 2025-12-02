library(shiny)
library(calcite)
library(arcgisutils)

# create the shiny AGOL login provider
client <- auth_shiny()

ui <- calcite_shell(
  # Include the shinyOAuth module
  use_shinyOAuth(),
  # TODO fix this in {calcite}
  tags$script(HTML(
    "
    $(document).on('shiny:connected', function() {
      $(document).on('click', '#login_btn', function(e) {
        Shiny.setInputValue('login_btn_clicked', Math.random());
      });
    });
  "
  )),
  uiOutput("navigation")
)

server <- function(input, output, session) {
  auth <- shinyOAuth::oauth_module_server(
    "auth",
    client,
    auto_redirect = FALSE
  )

  # Render entire navigation based on authentication status
  output$navigation <- renderUI({
    calcite_navigation(
      slot = "header",
      calcite_menu(
        slot = "content-end",
        calcite_menu_item(
          text = "Drivers",
          `icon-start` = "license"
        ),
        calcite_menu_item(
          text = "Routes",
          `icon-start` = "road-sign"
        ),
        calcite_menu_item(
          text = "Forecast",
          `icon-start` = "snow"
        )
      ),
      # Conditionally render user slot
      if (auth$authenticated) {
        user_info <- auth$token@userinfo
        calcite_navigation_user(
          slot = "user",
          username = user_info$user$username,
          `full-name` = user_info$user$fullName,
          `user-id` = user_info$user$id
        )
      } else {
        calcite_button(
          id = "login_btn",
          slot = "user",
          "Login"
        )
      }
    )
  })

  # Trigger login when button is clicked
  observeEvent(input$login_btn_clicked, {
    auth$request_login()
  })
}

runApp(
  shinyApp(ui, server),
  port = 8100,
  launch.browser = FALSE
)
