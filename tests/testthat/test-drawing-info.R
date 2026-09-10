pts <- function() {
  sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  )
}

lns <- function() {
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1))), crs = 4326)
  )
}

polys <- function() {
  ring <- rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0))
  sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
}

def <- function(x) as_layer_definition(x, "test", "OBJECTID")

test_that("point layers get a marker renderer (#278)", {
  info <- def(pts())[["drawingInfo"]]

  expect_identical(info$renderer$type, "simple")
  expect_identical(info$renderer$symbol$type, "esriSMS")
})

test_that("line layers get a line renderer (#278)", {
  expect_identical(def(lns())[["drawingInfo"]]$renderer$symbol$type, "esriSLS")
})

test_that("polygon layers get a fill renderer (#278)", {
  expect_identical(def(polys())[["drawingInfo"]]$renderer$symbol$type, "esriSFS")
})

test_that("tables get no renderer (#278)", {
  info <- as_layer_definition(data.frame(id = 1L), "test", "OBJECTID")

  expect_null(info[["drawingInfo"]])
  expect_identical(info[["type"]], "Table")
})

test_that("an explicit drawing_info is respected (#278)", {
  mine <- list(renderer = list(type = "simple", symbol = list(type = "esriSMS")))

  expect_identical(
    as_layer_definition(pts(), "test", "OBJECTID", drawing_info = mine)[["drawingInfo"]],
    mine
  )
})
