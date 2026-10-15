test_that("validate_crs() accepts crs, character, integer, and whole doubles", {
  expected <- list(spatialReference = list(wkid = 4326L))

  expect_identical(validate_crs(sf::st_crs(4326)), expected)
  expect_identical(validate_crs("EPSG:4326"), expected)
  expect_identical(validate_crs(4326L), expected)
  expect_identical(validate_crs(4326), expected)
})

test_that("validate_crs() errors on invalid input", {
  expect_snapshot(error = TRUE, {
    validate_crs(4326.5)
    validate_crs(TRUE)
    validate_crs(list(4326))
  })
})
