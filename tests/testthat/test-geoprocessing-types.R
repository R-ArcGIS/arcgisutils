test_that("from_spatial_reference works for EPSG WKIDs", {
  sr <- list(wkid = 4326L)

  expect_identical(
    from_spatial_reference(sr),
    sf::st_crs(sr$wkid)
  )
})

test_that("from_spatial_reference works for ESRI WKIDs", {
  sr <- list(wkid = 102003L)

  expect_identical(
    from_spatial_reference(sr),
    sf::st_crs(paste0("ESRI:", sr$wkid))
  )
})

test_that("from_spatial_reference works for NA WKIDs", {
  sr <- list(wkid = NA_integer_)

  expect_identical(
    from_spatial_reference(sr),
    sf::NA_crs_
  )
})
