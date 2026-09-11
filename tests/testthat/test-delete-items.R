fake_token2 <- function() {
  httr2::oauth_token("1234", arcgis_host = arc_host(), username = "jdoe")
}

mock_delete <- function(results, env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  httr2::local_mocked_responses(
    function(req) {
      captured$req <- req
      httr2::response_json(body = list(results = results))
    },
    env = env
  )
  captured
}

ok <- function(...) lapply(c(...), function(i) list(itemId = i, success = TRUE))

test_that("delete_items() posts ids to deleteItems", {
  captured <- mock_delete(ok("a1", "b2"))

  res <- delete_items(c("a1", "b2"), token = fake_token2())

  expect_match(captured$req[["url"]], "/content/users/jdoe/deleteItems")
  expect_identical(URLdecode(as.character(captured$req[["body"]][["data"]][["items"]])), "a1,b2")
  expect_identical(res[["itemId"]], c("a1", "b2"))
  expect_true(all(res[["success"]]))
})

test_that("delete_items() sends permanentDelete", {
  captured <- mock_delete(ok("a1"))

  delete_items("a1", permanent = TRUE, token = fake_token2())
  expect_identical(as.character(captured$req[["body"]][["data"]][["permanentDelete"]]), "true")

  captured2 <- mock_delete(ok("a1"))
  delete_items("a1", token = fake_token2())
  expect_identical(as.character(captured2$req[["body"]][["data"]][["permanentDelete"]]), "false")
})

test_that("delete_items() accepts a PortalItem", {
  captured <- mock_delete(ok("a1"))
  item <- structure(list(id = "a1", owner = "asmith"), class = c("PortalItem", "list"))

  delete_items(item, token = fake_token2())

  expect_match(captured$req[["url"]], "/content/users/jdoe/deleteItems")
  expect_identical(as.character(captured$req[["body"]][["data"]][["items"]]), "a1")
})

test_that("delete_items() warns about items it could not delete", {
  mock_delete(list(
    list(itemId = "a1", success = TRUE),
    list(itemId = "b2", success = FALSE)
  ))

  expect_warning(delete_items(c("a1", "b2"), token = fake_token2()), "b2")
})

test_that("delete_items() caps the batch at 100", {
  expect_error(
    delete_items(as.character(seq_len(101)), token = fake_token2()),
    "100 or fewer"
  )
})

test_that("delete_items() validates its arguments", {
  expect_error(delete_items(character(0), token = fake_token2()), "item IDs")
  expect_error(delete_items(NA_character_, token = fake_token2()), "item IDs")
  expect_error(delete_items("a1", permanent = "yes", token = fake_token2()))
  expect_error(delete_items("a1", token = httr2::oauth_token("1234", arcgis_host = arc_host())), "username")
  expect_error(delete_items("a1", "extra", token = fake_token2()))
})
