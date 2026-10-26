host <- "https://www.arcgis.com"
token <- httr2::oauth_token("1234", arcgis_host = host, username = "jdoe")
owned_item <- structure(
  list(id = "i1", owner = "owner1"),
  class = c("PortalItem", "list")
)

mock_resources <- function(body, env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  captured$reqs <- list()
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs[[length(captured$reqs) + 1]] <- req
      if (grepl("/content/items/[^/]+\\?f=json$", req$url)) {
        httr2::response_json(body = list(id = "i1", owner = "owner1"))
      } else {
        captured$req <- req
        httr2::response_json(body = body)
      }
    },
    env = env
  )
  captured
}

test_that("arc_item_resources() lists an item's resources", {
  captured <- mock_resources(list(
    total = 1,
    start = 1,
    num = 1,
    nextStart = -1,
    resources = list(list(
      resource = "run/part-00000.parquet",
      created = 1790035587000,
      size = 30286
    ))
  ))

  res <- arc_item_resources("i1", .progress = FALSE, token = token)

  expect_match(
    captured$req$url,
    "^https://www.arcgis.com/sharing/rest/content/items/i1/resources"
  )
  expect_identical(res$resource, "run/part-00000.parquet")
  expect_s3_class(res$created, "POSIXct")
})

test_that("arc_item_resources() returns zero rows when there are none", {
  mock_resources(list(
    total = 0,
    start = 1,
    num = 0,
    nextStart = -1,
    resources = list()
  ))

  res <- arc_item_resources("i1", .progress = FALSE, token = token)

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
})

test_that("arc_add_item_resource() posts to the item owner's addResources", {
  captured <- mock_resources(list(
    success = TRUE,
    itemId = "i1",
    owner = "owner1",
    folder = NULL
  ))
  tf <- tempfile(fileext = ".parquet")
  writeLines("x", tf)

  res <- arc_add_item_resource(
    owned_item,
    file = tf,
    file_name = "part-00001.parquet",
    prefix = "run",
    token = token
  )

  expect_length(captured$reqs, 1)
  expect_match(
    captured$req$url,
    "/sharing/rest/content/users/owner1/items/i1/addResources"
  )
  body <- captured$req$body$data
  expect_s3_class(body$file, "form_file")
  expect_identical(body$fileName, "part-00001.parquet")
  expect_identical(body$resourcesPrefix, "run")
  expect_identical(body$archive, "false")
  expect_identical(body$access, "inherit")
  expect_true(res$success)
})

test_that("arc_add_item_resource() fetches the owner when given an ID", {
  captured <- mock_resources(list(success = TRUE, itemId = "i1"))

  arc_add_item_resource(
    "i1",
    text = "{}",
    file_name = "config.json",
    token = token
  )

  expect_length(captured$reqs, 2)
  expect_match(captured$req$url, "/users/owner1/items/i1/addResources")
  expect_identical(captured$req$body$data$text, "{}")
})

test_that("arc_add_item_resource() validates its arguments", {
  tf <- tempfile(fileext = ".txt")
  writeLines("x", tf)

  expect_error(arc_add_item_resource(owned_item, token = token), "must be supplied")
  expect_error(
    arc_add_item_resource(owned_item, file = tf, text = "x", token = token),
    "Exactly one"
  )
  expect_error(
    arc_add_item_resource(owned_item, text = "x", token = token),
    "file_name"
  )
  expect_error(
    arc_add_item_resource(owned_item, file = tf, access = "public", token = token),
    "access"
  )
  expect_error(
    arc_add_item_resource(owned_item, file = "nope.parquet", token = token),
    "does not exist"
  )
})

test_that("item_type() accepts Parquet Feature Layer", {
  expect_identical(
    item_type("Parquet Feature Layer")@value,
    "Parquet Feature Layer"
  )
})
