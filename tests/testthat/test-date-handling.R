test_that("date handling is correct", {

  # create sf object
  x <- sf::st_sf(
    data.frame(today = as.POSIXct("2001-01-01", tz = "UTC")),
    geometry = sf::st_sfc(sf::st_point(c(0, 1)), crs = 4326)
  )

  # create json object
  l <- as_featureset(x)
  # add fields since they are not included by default
  l[["fields"]] <- infer_esri_type(x)

  # create featurset geometry
  json <- jsonify::to_json(l)

  parsed <- parse_esri_json(json)

  expect_identical(x$today, parsed$today)
})
