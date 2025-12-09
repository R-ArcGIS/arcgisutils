test_that("envelope processing works", {
  x <- list(
    xmin = -122.4195,
    ymin = 37.330219000000056,
    xmax = -122.030757,
    ymax = 37.77650360000007,
    spatialReference = list(wkid = 4326L, latestWkid = 4326L)
  )

  expect_identical(
    from_envelope(x),
    sf::st_bbox(
      c(
        xmin = -122.4195,
        ymin = 37.330219000000056,
        xmax = -122.030757,
        ymax = 37.77650360000007
      ),
      crs = 4326L
    )
  )
})
