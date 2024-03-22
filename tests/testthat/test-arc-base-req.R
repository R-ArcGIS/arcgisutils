test_that("agent is arcgisutils", {
  req <- arc_base_req("http://google.com")

  expect_true(
    grepl("^arcgisutils", req$options$useragent)
  )
})

test_that("token is added as header", {

  token_value <- "my-secret-token"

  # create fake token
  token <- httr2::oauth_token(
    token_value,
    arcgis_host = arc_host()
  )

  req <- arc_base_req("http://google.com", token)

  expect_identical(
    paste0("Bearer ", token_value),
    req$headers[["X-Esri-Authorization"]]
  )

})

test_that("path is appended", {
  path_vals <- c("a", "b", "c")

  # create a request
  req <- arc_base_req(
    "http://google.com",
    path = path_vals
  )

  # parse the url of the request
  req_url <- httr2::url_parse(req[["url"]])

  # split
  expect_identical(
    path_vals,
    strsplit(req_url$path, "/")[[1]][-1]
  )
})


test_that("query is appended", {
  query <- list(a = "1", b = "123")

  # create a request
  req <- arc_base_req(
    "http://google.com",
    query = query
  )

  # parse the url of the request
  req_url <- httr2::url_parse(req[["url"]])

  expect_identical(
    query,
    req_url$query
  )
})
