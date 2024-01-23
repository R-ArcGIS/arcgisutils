test_that("fetch_layer_metadata() NULL and empty string", {
  furl <- "http://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer"
  req <- httr2::request(furl)

  expect_equal(
    fetch_layer_metadata(req, NULL),
    fetch_layer_metadata(req, "")
  )
})

test_that("fetch_layer_metadata() known working MapServer", {
  furl <- "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
  req <- httr2::request(furl)

  # this works with no token provided as a null or nzchar
  expect_equal(
    fetch_layer_metadata(req, NULL),
    fetch_layer_metadata(req, "")
  )
})
