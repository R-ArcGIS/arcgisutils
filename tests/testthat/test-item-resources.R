parquet_layer <- "fd6e506c77de46d5ad401528f74b5d5e"
parquet_file <- "6ab6f959c1684eb5a54db91644a89747"

test_that("arc_item_resources() validates sort arguments", {
  expect_error(
    arc_item_resources(parquet_layer, sort_field = "name", token = NULL),
    "ResourceSortField"
  )
  expect_error(
    arc_item_resources(parquet_layer, sort_order = "up", token = NULL),
    "SortOrder"
  )
})

test_that("arc_item_resources() lists an item's resources", {
  skip_on_cran()
  skip_if_offline()

  res <- arc_item_resources(parquet_layer, token = NULL)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_true(all(c("resource", "created", "size") %in% names(res)))
  expect_s3_class(res$created, "POSIXct")
})

test_that("arc_item_resources() returns zero rows when there are none", {
  skip_on_cran()
  skip_if_offline()

  res <- arc_item_resources(parquet_file, token = NULL)

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
})

test_that("item_type() accepts Parquet Feature Layer", {
  expect_identical(
    item_type("Parquet Feature Layer")@value,
    "Parquet Feature Layer"
  )
})
