test_that("as_bbox() accepts the shapes an extent arrives in", {
  expected <- c(0, 1, 2, 3)

  expect_equal(unname(unlist(unclass(as_bbox(c(0, 1, 2, 3))))), expected)
  expect_equal(
    unname(unlist(unclass(as_bbox(wk::rct(0, 1, 2, 3))))),
    expected
  )
  expect_equal(
    unname(unlist(unclass(
      as_bbox(sf::st_bbox(c(xmin = 0, ymin = 1, xmax = 2, ymax = 3)))
    ))),
    expected
  )
})

test_that("as_bbox() takes the bounding box of a geometry", {
  point <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))

  expect_equal(
    unname(unlist(unclass(as_bbox(point)))),
    c(0, 0, 1, 1)
  )
})

test_that("as_bbox() applies a crs when the extent has none", {
  expect_equal(wk::wk_crs(as_bbox(c(0, 1, 2, 3), crs = 4326)), 4326)
  expect_null(wk::wk_crs(as_bbox(c(0, 1, 2, 3))))
})

test_that("as_bbox() reprojects when the crs differs", {
  bbox <- as_bbox(c(-1, -1, 1, 1), crs = 4326)
  out <- as_bbox(bbox, crs = 3857)

  expect_true(wk::wk_crs_equal(wk::wk_crs(out), 3857))
  expect_true(wk::rct_xmin(out) < -100000)
})

test_that("as_bbox() keeps an extent already in the target crs", {
  bbox <- as_bbox(c(0, 1, 2, 3), crs = 3857)

  expect_equal(as_bbox(bbox, crs = 3857), bbox)
})

test_that("as_bbox() rejects what it cannot read", {
  expect_error(as_bbox(c(1, 2, 3)), "length four")
  expect_error(as_bbox("nope"), "length four")
})

test_that("fetch_layer_metadata() builds paths and queries", {
  req <- arc_base_req(
    "https://example.com/arcgis/rest/services/X/MapServer",
    path = c("tile", "1", "2"),
    query = list(a = "b")
  )

  expect_match(req$url, "/MapServer/tile/1/2")
  expect_match(req$url, "a=b")
})
