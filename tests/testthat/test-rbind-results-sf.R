# Both tests below fail on the collapse backend. `collapse::rowbind()` returns
# an object that already inherits sf, so the `st_as_sf()` repair in
# `rbind_results()` is skipped and the first piece's bbox survives. It also
# refuses columns whose class attributes disagree. The data.table and vctrs
# fallbacks handle both cases.

test_that("rbind_results() recomputes the bounding box of the result", {
  skip_if_not_installed("sf")

  first <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = sf::st_crs(4326))
  )

  second <- sf::st_sf(
    id = 2,
    geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = sf::st_crs(4326))
  )

  res <- rbind_results(list(first, second))

  # the coordinates bind correctly, it is only the bbox attribute that is stale
  expect_equal(unname(sf::st_coordinates(res)[, 1]), c(0, 1))
  expect_equal(unname(as.double(sf::st_bbox(res))), c(0, 0, 1, 1))
})

test_that("rbind_results() binds sf pieces whose geometry classes differ", {
  skip_if_not_installed("sf")

  line <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(0, 0), c(1, 1))),
      crs = sf::st_crs(4326)
    )
  )

  multiline <- sf::st_sf(
    id = 2,
    geometry = sf::st_sfc(
      sf::st_multilinestring(list(rbind(c(2, 2), c(3, 3)))),
      crs = sf::st_crs(4326)
    )
  )

  res <- rbind_results(list(line, multiline))

  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 2L)
})

test_that("the fallback backends handle both cases", {
  skip_if_not_installed("sf")
  skip_if_not_installed("vctrs")

  first <- sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(
      sf::st_linestring(rbind(c(0, 0), c(1, 1))),
      crs = sf::st_crs(4326)
    )
  )

  second <- sf::st_sf(
    id = 2,
    geometry = sf::st_sfc(
      sf::st_multilinestring(list(rbind(c(2, 2), c(3, 3)))),
      crs = sf::st_crs(4326)
    )
  )

  res <- sf::st_as_sf(vctrs::list_unchop(list(first, second)))

  expect_equal(nrow(res), 2L)
  expect_equal(unname(as.double(sf::st_bbox(res))), c(0, 0, 3, 3))
})
