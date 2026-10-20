host <- "https://www.arcgis.com"
token <- httr2::oauth_token("1234", arcgis_host = host, username = "jdoe")

mock_portal <- function(body, env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  httr2::local_mocked_responses(
    function(req) {
      captured$req <- req
      httr2::response_json(body = body)
    },
    env = env
  )
  captured
}

form_of <- function(req) {
  lapply(req$body$data, function(x) utils::URLdecode(as.character(x)))
}

portal_item <- function(id = "o1", owner = "jdoe") {
  structure(list(id = id, owner = owner), class = c("PortalItem", "list"))
}

test_that("add_relationship() posts to the origin owner's addRelationship", {
  captured <- mock_portal(list(success = TRUE))

  res <- add_relationship(
    portal_item(),
    "d1",
    "Map2Service",
    token = token
  )

  expect_true(res$success)
  expect_match(
    captured$req$url,
    "/sharing/rest/content/users/jdoe/addRelationship$"
  )
  form <- form_of(captured$req)
  expect_identical(form$originItemId, "o1")
  expect_identical(form$destinationItemId, "d1")
  expect_identical(form$relationshipType, "Map2Service")
})

test_that("delete_relationship() posts to deleteRelationship", {
  captured <- mock_portal(list(success = TRUE))

  delete_relationship(
    "o1",
    portal_item("d1"),
    "Service2Data",
    user = "asmith",
    token = token
  )

  expect_match(
    captured$req$url,
    "/sharing/rest/content/users/asmith/deleteRelationship$"
  )
  expect_identical(form_of(captured$req)$destinationItemId, "d1")
})

test_that("relationship functions validate their arguments", {
  expect_error(add_relationship("o1", "d1", "Map2Service", token = token), "user")
  expect_error(
    add_relationship(portal_item(), "d1", "NotAType", token = token),
    "RelationshipType"
  )
  expect_error(add_relationship(portal_item(), 1, "Map2Service", token = token), "item")
})

test_that("relationship functions surface portal errors", {
  mock_portal(list(error = list(code = 400, message = "Relationship exists")))

  expect_error(
    add_relationship(
      portal_item(),
      "d1",
      "Map2Service",
      token = token
    ),
    "Relationship exists"
  )
})

test_that("set_content_status() sets and clears the status", {
  captured <- mock_portal(list(success = TRUE, itemId = "i1"))

  res <- set_content_status(
    "i1",
    "deprecated",
    token = token
  )

  expect_identical(res$itemId, "i1")
  expect_match(captured$req$url, "/content/items/i1/setContentStatus$")
  expect_identical(form_of(captured$req)$status, "deprecated")
  expect_null(form_of(captured$req)$clearEmptyFields)

  captured <- mock_portal(list(success = TRUE, itemId = "i1"))
  set_content_status(
    "i1",
    "none",
    token = token
  )
  expect_identical(form_of(captured$req)$status, "")
  expect_identical(form_of(captured$req)$clearEmptyFields, "true")

  expect_error(set_content_status("i1", "authoritative", token = token), "ContentStatus")
})

test_that("set_item_categories() sends one entry per item", {
  captured <- mock_portal(list(
    results = list(
      list(itemId = "a1", success = TRUE),
      list(itemId = "b2", success = TRUE)
    )
  ))

  res <- set_item_categories(
    c("a1", "b2"),
    "/Categories/Water",
    token = token
  )

  expect_match(captured$req$url, "/sharing/rest/content/updateItems$")
  expect_identical(
    form_of(captured$req)$items,
    '[{"a1":{"categories":["/Categories/Water"]}},{"b2":{"categories":["/Categories/Water"]}}]'
  )
  expect_true(all(res$success))
})

test_that("set_item_categories() clears with character(0) and warns on failure", {
  captured <- mock_portal(list(
    results = list(list(itemId = "a1", success = FALSE))
  ))

  expect_warning(
    set_item_categories(
      "a1",
      character(0),
      token = token
    ),
    "a1"
  )
  expect_identical(
    form_of(captured$req)$items,
    '[{"a1":{"categories":[]}}]'
  )
})

test_that("set_item_categories() rejects more than 100 items", {
  expect_error(
    set_item_categories(paste0("i", 1:101), "/Categories/Water", token = token),
    "100 or fewer"
  )
})
