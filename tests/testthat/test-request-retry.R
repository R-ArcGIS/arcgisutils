mock_resp <- function(status = 200L, body = "{}", headers = list()) {
  httr2::response(
    status_code = status,
    headers = headers,
    body = charToRaw(body)
  )
}

esri_error <- function(code) {
  paste0('{"error":{"code":', code, ',"message":"a message","details":[]}}')
}

test_that("arc_base_req() attaches a retry policy", {
  req <- arc_base_req("https://arcgis.com")
  expect_identical(req$policies$retry_max_tries, 3)
  expect_true(is.function(req$policies$retry_is_transient))
})

test_that("max_tries = 1 disables retries", {
  req <- arc_base_req("https://arcgis.com", max_tries = 1)
  expect_null(req$policies$retry_max_tries)
})

test_that("arcgis.retries option sets the default", {
  old <- options(arcgis.retries = 5)
  on.exit(options(old))
  expect_identical(arc_base_req("https://arcgis.com")$policies$retry_max_tries, 5)
})

test_that("max_tries is validated", {
  expect_error(arc_base_req("https://arcgis.com", max_tries = 0))
  expect_error(arc_base_req("https://arcgis.com", max_tries = "3"))
})

test_that("transient http statuses are retried", {
  for (status in c(429L, 500L, 502L, 503L, 504L)) {
    expect_true(esri_is_transient(mock_resp(status)))
  }
})

test_that("permanent http statuses are not retried", {
  for (status in c(200L, 400L, 401L, 403L, 404L)) {
    expect_false(esri_is_transient(mock_resp(status)))
  }
})

test_that("transient esri errors in a 200 body are retried", {
  for (code in c(429L, 500L, 502L, 503L, 504L)) {
    expect_true(esri_is_transient(mock_resp(200L, esri_error(code))))
  }
})

test_that("permanent esri errors in a 200 body are not retried", {
  for (code in c(400L, 403L, 498L, 499L)) {
    expect_false(esri_is_transient(mock_resp(200L, esri_error(code))))
  }
})

test_that("successful and unparseable bodies are not retried", {
  expect_false(esri_is_transient(mock_resp(200L, '{"features":[]}')))
  expect_false(esri_is_transient(mock_resp(200L, '{"count":500}')))
  expect_false(esri_is_transient(mock_resp(200L, "<html>500</html>")))
  expect_false(esri_is_transient(mock_resp(200L, "")))
})

test_that("a large body is not parsed to look for an error", {
  big <- paste0('{"features":[', paste0(rep('{"attributes":{"a":500}}', 1000), collapse = ","), "]}")
  expect_null(esri_body_error_code(mock_resp(200L, big)))
})

test_that("httr2 is handed the transient check and retries connection failures", {
  req <- arc_base_req("https://arcgis.com")
  expect_identical(req$policies$retry_is_transient, esri_is_transient)
  expect_true(req$policies$retry_on_failure)
})

# httr2 mocking returns before the retry loop, so only one attempt is observable
test_that("max_tries = 1 performs a transient response without retrying", {
  n <- 0L
  httr2::local_mocked_responses(function(req) {
    n <<- n + 1L
    mock_resp(200L, esri_error(500))
  })

  resp <- httr2::req_perform(arc_base_req("https://arcgis.com", max_tries = 1))

  expect_identical(n, 1L)
  expect_identical(httr2::resp_body_string(resp), esri_error(500))
})

test_that("a binary body is not treated as an error body", {
  zip <- as.raw(c(0x50, 0x4b, 0x03, 0x04, 0x14, 0x00, 0x00, 0x00, 0x08, 0x00))
  resp <- httr2::response(
    status_code = 200L,
    headers = list(`Content-Type` = "application/zip"),
    body = zip
  )

  expect_null(esri_body_error_code(resp))
  expect_false(esri_is_transient(resp))
})
