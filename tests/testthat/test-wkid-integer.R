# Regression tests for https://github.com/R-ArcGIS/arcgisutils/issues/87
#
# as_featureset() builds its result with extendr's serde serializer, which maps
# numbers onto R doubles because R has no unsigned or 64 bit integer type. The
# integer wkid came back as a double and serialized as `4326.0` instead of
# `4326`. The string variant uses serde_json directly and was never affected.

test_that("as_featureset() returns an integer wkid (#87)", {
  x <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(-120, 46)), crs = 4326)
  )

  fs <- as_featureset(x)
  expect_type(fs$spatialReference$wkid, "integer")
  expect_identical(fs$spatialReference$wkid, 4326L)
})

test_that("integer wkid survives JSON serialization (#87)", {
  skip_if_not_installed("yyjsonr")

  x <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(-120, 46)), crs = 4326)
  )

  json <- yyjsonr::write_json_str(as_featureset(x), auto_unbox = TRUE)
  expect_true(grepl('"wkid":4326', json, fixed = TRUE))
  expect_false(grepl("4326.0", json, fixed = TRUE))
})

test_that("as_featureset() without a CRS still works (#87)", {
  x <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(-120, 46)))
  )

  expect_no_error(as_featureset(x))
})

test_that("as_esri_featureset() string variant keeps wkid an integer", {
  x <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_point(c(-120, 46)), crs = 4326)
  )

  expect_true(grepl('"wkid":4326', as_esri_featureset(x), fixed = TRUE))
})
