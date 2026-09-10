test_that("rbind_results() combines zero column data.frames", {
  res <- rbind_results(list(
    data.frame(row.names = 1:3),
    data.frame(row.names = 1:2)
  ))

  expect_s3_class(res, "data.frame")
  expect_identical(dim(res), c(5L, 0L))
})

test_that("rbind_results() keeps zero column results with NULLs", {
  res <- rbind_results(list(data.frame(row.names = 1:3), NULL))

  expect_identical(dim(res), c(3L, 0L))
  expect_identical(attr(res, "null_elements"), 2L)
})

test_that("rbind_results() returns sf when zero column inputs are sf", {
  skip_if_not_installed("sf")

  pts <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))))
  pts$geometry <- NULL
  expect_no_error(rbind_results(list(data.frame(row.names = 1L))))
})

test_that("rbind_results() still combines populated data.frames", {
  res <- rbind_results(list(head(iris, 2), head(iris, 3)))

  expect_identical(nrow(res), 5L)
  expect_identical(ncol(res), ncol(iris))
})
