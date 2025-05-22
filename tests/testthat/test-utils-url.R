test_that("arc_url_parse works", {
  furl <- "https://services.arcgisonline.com/arcgis/rest/services/USA_Topo_Maps/MapServer"

  furl_parsed <- arc_url_parse(furl)

  expect_identical(furl_parsed[["type"]], "MapServer")
  expect_identical(furl_parsed[["url"]], furl)

  furl_alt <- "https://services1.arcgis.com/hLJbHVT9ZrDIzK0I/arcgis/rest/services/North%20Carolina%20SIDS%20sample/FeatureServer"

  expect_identical(arc_url_parse(furl_alt)[["type"]], "FeatureServer")
})
