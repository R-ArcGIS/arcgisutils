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
  expect_identical(res, structure(data.frame(), null_elements = integer()))

  # one null
  res <- rbind_results(list(NULL))
  expect_identical(res, structure(data.frame(), null_elements = 1L))

  # multiple
  res <- rbind_results(list(NULL, NULL))
  expect_identical(res, structure(data.frame(), null_elements = 1:2))
})

test_that("rbind errors on non-df objects", {
  expect_error(rbind_results(list(iris, NULL, "a")))
})

test_that("rbind works with list columns and collapse", {
  # this object caused pain
  x <- list(
    loc_name = "World",
    extents = list(
      c(
        xmin = -51.129958180141,
        ymin = 0.023178982724,
        xmax = -51.127958180141,
        ymax = 0.025178982724
      )
    ),
    geometry = list(
      c(-51.128958180141, 0.024178982724) |>
        structure(class = c("XY", "POINT", "sfg"))
    ) |>
      structure(
        class = c("sfc_POINT", "sfc"),
        precision = 0,
        bbox = c(
          xmin = -51.128958180141,
          ymin = 0.024178982724,
          xmax = -51.128958180141,
          ymax = 0.024178982724
        ) |>
          structure(class = "bbox"),
        crs = list(input = NA_character_, wkt = NA_character_) |>
          structure(class = "crs"),
        n_empty = 0L
      )
  ) |>
    structure(
      row.names = 1L,
      sf_column = "geometry",
      agr = factor(
        c(loc_name = NA_character_, extents = NA_character_),
        levels = c("constant", "aggregate", "identity")
      ),
      class = c("sf", "data.frame")
    )

  to_bind <- list(NULL, x, NULL, x)

  res <- rbind_results(to_bind)
  expect_equal(rbind(x, x), res, ignore_attr = TRUE)
})
