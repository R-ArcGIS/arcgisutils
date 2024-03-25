test_that("rbind sf objects", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  x <- list(nc, nc, NULL, nc)

  res <- rbind_results(x)

  # should be an sf object
  expect_true(inherits(res, "sf"))
  # should have 1 missing element
  expect_identical(attr(res, "null_elements"), 3L)
})

test_that("rbind mixed results", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  items <- list(as.data.frame(nc), nc)

  res <- rbind_results(items)
  expect_true(rlang::inherits_only(res, "data.frame"))
})

test_that("rbind data.frames", {
  res <- rbind_results(list(iris, NULL, iris))
  expect_true(rlang::inherits_only(res, "data.frame"))
})

test_that("rbind NULL & list(NULL)", {

  # should return empty df
  res <- rbind_results(NULL)
  expect_identical(res, data.frame())

  # one null
  res <- rbind_results(list(NULL))
  expect_identical(res, data.frame())

  # multiple
  res <- rbind_results(list(NULL, NULL))
  expect_identical(res, data.frame())
})

test_that("rbind errors on non-df objects", {
  expect_error(rbind_results(list(iris, NULL, "a")))
})


