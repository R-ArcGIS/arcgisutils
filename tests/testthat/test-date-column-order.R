test_that("a date column after the geometry column is not mis-indexed", {
  skip_if_not_installed("sf")
  skip_if_not_installed("RcppSimdJson")

  x <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 1)),
      sf::st_point(c(2, 3)),
      crs = 4326
    )
  )
  x$when <- as.POSIXct(c("2001-01-01", "2002-02-02"), tz = "UTC")

  expect_identical(names(x), c("id", "geometry", "when"))

  expect_identical(
    as_features(x)[[1]]$attributes$when,
    978307200000
  )
  expect_identical(
    as_featureset(x)$features[[2]]$attributes$when,
    1012608000000
  )

  features <- RcppSimdJson::fparse(
    as_esri_features(x),
    max_simplify_lvl = "list"
  )
  expect_identical(features[[1]]$attributes$when, 978307200000)
  expect_identical(features[[2]]$attributes$when, 1012608000000)

  featureset <- RcppSimdJson::fparse(
    as_esri_featureset(x),
    max_simplify_lvl = "list"
  )
  expect_identical(featureset$features[[1]]$attributes$when, 978307200000)
  expect_identical(featureset$features[[2]]$attributes$when, 1012608000000)
})

test_that("dates on both sides of the geometry column are converted", {
  skip_if_not_installed("sf")
  skip_if_not_installed("RcppSimdJson")

  x <- sf::st_sf(
    before = as.POSIXct(c("2001-01-01", "2002-02-02"), tz = "UTC"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 1)),
      sf::st_point(c(2, 3)),
      crs = 4326
    )
  )
  x$after <- as.POSIXct(c("2003-03-03", "2004-04-04"), tz = "UTC")

  expect_identical(names(x), c("before", "geometry", "after"))

  featureset <- RcppSimdJson::fparse(
    as_esri_featureset(x),
    max_simplify_lvl = "list"
  )

  expect_identical(featureset$features[[1]]$attributes$before, 978307200000)
  expect_identical(featureset$features[[1]]$attributes$after, 1046649600000)
  expect_identical(featureset$features[[2]]$attributes$before, 1012608000000)
  expect_identical(featureset$features[[2]]$attributes$after, 1081036800000)
})
