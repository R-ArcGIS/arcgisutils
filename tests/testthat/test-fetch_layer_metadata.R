test_that("fetch_layer_metadata() NULL and empty string", {
  skip()
  furl <- "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer"
  req <- httr2::request(furl)

  expect_equal(
    fetch_layer_metadata(req, NULL),
    fetch_layer_metadata(req, "")
  )
})

test_that("fetch_layer_metadata() known working MapServer", {
  skip()
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  req <- httr2::request(furl)

  # this works with no token provided as a null or nzchar
  expect_equal(
    fetch_layer_metadata(req, NULL),
    fetch_layer_metadata(req, "")
  )
})


test_that("fetch_layer_metadata() works for private content", {
  skip("must be done interactively")
  furl <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/North%20Carolina%20SIDS%20sample/FeatureServer"
  req <- httr2::request(furl)
  token <- auth_code()

  expect_no_error(fetch_layer_metadata(req, token$access_token))

})
