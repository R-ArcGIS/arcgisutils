public_item <- "9df5e769bfe8412b8de36a2e618c7672"

test_that("item_id() accepts ids and PortalItems", {
  pi <- structure(list(id = "abc123"), class = c("PortalItem", "list"))

  expect_identical(item_id("abc123"), "abc123")
  expect_identical(item_id(pi), "abc123")
})

test_that("item_id() rejects anything else", {
  expect_error(item_id(1L), "item")
  expect_error(item_id(""), "item")
  expect_error(item_id(c("a", "b")), "item")
})

test_that("arc_related_items() validates direction and type", {
  expect_error(arc_related_items(public_item, direction = "backward"))
  expect_error(arc_related_items(
    public_item,
    relationship_type = "Service2Datum"
  ))
})

test_that("arc_related_items() returns related items", {
  skip_on_cran()
  skip_if_offline()

  res <- arc_related_items(public_item, token = NULL)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_true(all(c("id", "type", "title") %in% names(res)))
})

test_that("arc_related_items() parses dates", {
  skip_on_cran()
  skip_if_offline()

  res <- arc_related_items(public_item, token = NULL)
  expect_s3_class(res$created, "POSIXct")
})

test_that("arc_related_items() filters by type", {
  skip_on_cran()
  skip_if_offline()

  all_types <- arc_related_items(public_item, token = NULL)
  one_type <- arc_related_items(
    public_item,
    relationship_type = "Map2Service",
    token = NULL
  )

  expect_lte(nrow(one_type), nrow(all_types))
})

test_that("arc_related_items() returns zero rows when there are none", {
  skip_on_cran()
  skip_if_offline()

  res <- arc_related_items(
    public_item,
    relationship_type = "APIKey2Item",
    token = NULL
  )

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
})

test_that("enum errors name the offending argument", {
  expect_error(
    arc_related_items(public_item, direction = "backward"),
    "direction"
  )
  expect_error(
    arc_related_items(public_item, relationship_type = "Nope"),
    "relationship_type"
  )
})
