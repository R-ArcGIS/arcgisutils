# Authenticate with Shiny

`oath_provider_arcgis()` and `auth_shiny()` provide an integrated
authentication experience for `{shiny}` applications via the package
[shinyOAuth](https://github.com/lukakoning/shinyOAuth/). Applications
that use `auth_shiny()` will enable user-specific behavior and
authentications.

## Usage

``` r
oauth_provider_arcgis(host = arc_host())

auth_shiny(
  client = Sys.getenv("ARCGIS_CLIENT"),
  secret = Sys.getenv("ARCGIS_SECRET"),
  redirect_uri = Sys.getenv("ARCGIS_REDIRECT_URI"),
  host = arc_host(),
  ...
)
```

## Arguments

- host:

  default `"https://www.arcgis.com"`. The host of your ArcGIS Portal.

- client:

  an OAuth 2.0 developer application client ID. By default uses the
  environment variable `ARCGIS_CLIENT`.

- secret:

  an OAuth 2.0 developer application secret. By default uses the
  environment variable `ARCGIS_SECRET`.

- redirect_uri:

  default `Sys.getenv("ARCGIS_REDIRECT_URI")`. The redirect URL after
  completing authentication flow.

- ...:

  additional arguments passed to `shinyOAuth::oauth_client()`

## Details

Authentication with `auth_shiny()` requires the package `shinyOAuth`.
When creating an OAuth app in ArcGIS Online / Enterprise, ensure that
the app has a valid redirect URI. This **must be** the same redirect URI
that is used by `shinyOAuth` and **must be** the same port as the shiny
application is served on.

`oauth_provider_arcgis()` is a low-level provider function that mimics
other `oauth_provider_*()` functions from `shinyOAuth`. Use
`auth_shiny()` to create an OAuth client.

**Important:** The client object returned by `auth_shiny()` must be
created outside of the server function to ensure a shared state across
the OAuth flow. Creating the client inside the server function will
result in authentication failures.

The user info returned from `auth$token@userinfo` has the same structure
as
[`arc_portal_self()`](https://github.com/R-ArcGIS/arcgisutils/reference/self.md).
See that function's documentation for available fields.

The below example is derived from the shinyOAuth documentation.

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
      # Set auto_redirect = FALSE for manual login
      auth <- shinyOAuth::oauth_module_server(
        "AGOL",
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

    # Open the app in your regular browser at http://localhost:8100
