test_that("Shiny OAuth identifies users within the same organization", {
  skip_if_not_installed("shinyOAuth", minimum_version = "0.6.0")
  provider <- oauth_provider_arcgis()
  select_id <- provider@userinfo_id_selector

  alice <- list(id = "organization-id", user = list(id = "alice-id"))
  bob <- list(id = "organization-id", user = list(id = "bob-id"))
  expect_identical(select_id(alice), "alice-id")
  expect_identical(select_id(bob), "bob-id")
  expect_null(select_id(list(id = "organization-id")))
  expect_null(select_id(list(id = "organization-id", user = list())))
})

test_that("Shiny login and refresh accept ArcGIS responses without token_type", {
  skip_if_not_installed("shinyOAuth", minimum_version = "0.6.0")
  client <- auth_shiny(
    client = "test-client",
    secret = "test-secret",
    redirect_uri = "http://localhost:8100"
  )
  expect_true(client@provider@allow_missing_token_type)
  expect_identical(client@provider@allowed_token_types, "Bearer")
  token_requests <- 0L
  userinfo_requests <- 0L
  authorization <- NULL
  httr2::local_mocked_responses(function(req) {
    if (identical(req$url, client@provider@token_url)) {
      token_requests <<- token_requests + 1L
      body <- if (token_requests == 1L) {
        '{"access_token":"first-access","expires_in":1800,"username":"alice","ssl":true,"refresh_token":"refresh","refresh_token_expires_in":604799}'
      } else {
        '{"access_token":"refreshed-access","expires_in":1800,"username":"alice","ssl":true}'
      }
    } else {
      userinfo_requests <<- userinfo_requests + 1L
      authorization <<- httr2::req_dry_run(
        req,
        quiet = TRUE,
        redact_headers = FALSE
      )$headers[["authorization"]]
      body <- '{"id":"organization-id","user":{"id":"alice-id","username":"alice"}}'
    }
    httr2::response(
      url = req$url,
      status_code = 200L,
      headers = list("Content-Type" = "application/json"),
      body = charToRaw(body)
    )
  })

  browser <- paste(rep("ab", 64), collapse = "")
  url <- shinyOAuth::prepare_call(client, browser_token = browser)
  state <- httr2::url_parse(url)$query$state
  token <- shinyOAuth::handle_callback(
    client,
    code = "test-code",
    state = state,
    browser_token = browser
  )
  expect_identical(token@access_token, "first-access")
  expect_identical(token@token_type, "Bearer")
  expect_identical(
    client@provider@userinfo_id_selector(token@userinfo),
    "alice-id"
  )
  expect_identical(authorization, "Bearer first-access")

  refreshed <- shinyOAuth::refresh_token(client, token)
  expect_identical(refreshed@access_token, "refreshed-access")
  expect_identical(refreshed@token_type, "Bearer")
  expect_identical(refreshed@refresh_token, "refresh")
  expect_identical(refreshed@userinfo, token@userinfo)
  expect_identical(authorization, "Bearer refreshed-access")
  expect_identical(token_requests, 2L)
  expect_identical(userinfo_requests, 2L)
})
