mock_item_ops <- function(body, item = list(id = "i1", owner = "jdoe"), env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  captured$reqs <- list()
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs[[length(captured$reqs) + 1]] <- req
      if (grepl("/content/items/[^/]+\\?f=json$", req$url)) {
        httr2::response_json(body = item)
      } else {
        captured$req <- req
        httr2::response_json(body = body)
      }
    },
    env = env
  )
  captured
}

owned_item <- function(id = "i1", owner = "jdoe") {
  structure(list(id = id, owner = owner), class = c("PortalItem", "list"))
}

form_values <- function(req) {
  lapply(req$body$data, function(x) {
    if (inherits(x, "form_file")) {
      x
    } else {
      utils::URLdecode(as.character(x))
    }
  })
}

host <- "https://www.arcgis.com"
token <- httr2::oauth_token("1234", arcgis_host = host, username = "jdoe")

test_that("update_item() sends only the fields supplied", {
  captured <- mock_item_ops(list(success = TRUE, id = "i1"))

  res <- update_item(
    owned_item(),
    title = "Penguins",
    tags = c("a", "b"),
    token = token
  )

  expect_match(captured$req$url, "/content/users/jdoe/items/i1/update$")
  form <- form_values(captured$req)
  expect_identical(form$title, "Penguins")
  expect_identical(form$tags, "a,b")
  expect_null(form$description)
  expect_null(form$file)
  expect_s3_class(res, "PortalItem")
})

test_that("update_item() replaces the file", {
  captured <- mock_item_ops(list(success = TRUE, id = "i1"))
  path <- tempfile(fileext = ".csv")
  writeLines("a,b", path)

  update_item(owned_item(), path = path, token = token)

  expect_s3_class(form_values(captured$req)$file, "form_file")
})

test_that("update_item() requires a field and an existing file", {
  expect_error(update_item(owned_item()), "at least one field")
  expect_error(update_item(owned_item(), path = "nope.csv"), "does not exist")
  expect_error(update_item("i1", title = "x"), "user")
})

test_that("delete_item() posts permanentDelete to the owner's item", {
  captured <- mock_item_ops(list(success = TRUE, itemId = "i1"))

  res <- delete_item(owned_item(), permanent = TRUE, token = token)

  expect_identical(res$itemId, "i1")
  expect_match(captured$req$url, "/content/users/jdoe/items/i1/delete$")
  expect_identical(form_values(captured$req)$permanentDelete, "true")
})

test_that("protect_item() and unprotect_item() call their endpoints", {
  captured <- mock_item_ops(list(success = TRUE))
  protect_item(owned_item(), token = token)
  expect_match(captured$req$url, "/items/i1/protect$")

  captured <- mock_item_ops(list(success = TRUE))
  unprotect_item("i1", user = "jdoe", token = token)
  expect_match(captured$req$url, "/items/i1/unprotect$")
})

test_that("move_items() moves the signed in user's items", {
  captured <- mock_item_ops(list(
    results = list(
      list(itemId = "a1", success = TRUE),
      list(itemId = "b2", success = FALSE)
    )
  ))

  expect_warning(
    res <- move_items(c("a1", "b2"), "/", token = token),
    "b2"
  )

  expect_match(captured$req$url, "/content/users/jdoe/moveItems$")
  form <- form_values(captured$req)
  expect_identical(form$items, "a1,b2")
  expect_identical(form$folder, "/")
  expect_identical(res$success, c(TRUE, FALSE))
})

test_that("move_items() validates its arguments", {
  expect_error(move_items("a1", "", token = token), "folder")
  expect_error(move_items(paste0("i", 1:101), "/", token = token), "100")
})

test_that("copy_item() returns the copy from either id field", {
  captured <- mock_item_ops(
    list(success = TRUE, id = "c1"),
    item = list(id = "c1", owner = "jdoe")
  )

  res <- copy_item(
    owned_item(),
    title = "Copy",
    include_resources = TRUE,
    token = token
  )

  expect_match(captured$req$url, "/items/i1/copy$")
  form <- form_values(captured$req)
  expect_identical(form$title, "Copy")
  expect_identical(form$includeResources, "true")
  expect_match(captured$reqs[[2]]$url, "/content/items/c1\\?f=json$")
  expect_identical(res$id, "c1")

  captured <- mock_item_ops(
    list(success = TRUE, itemId = "c2"),
    item = list(id = "c2", owner = "jdoe")
  )
  expect_identical(copy_item(owned_item(), token = token)$id, "c2")
})

test_that("reassign_item() sends the target user and folder", {
  captured <- mock_item_ops(list(success = TRUE, itemId = "i1"))

  reassign_item(owned_item(), "asmith", "Handover", token = token)

  expect_match(captured$req$url, "/items/i1/reassign$")
  form <- form_values(captured$req)
  expect_identical(form$targetUsername, "asmith")
  expect_identical(form$targetFolderName, "Handover")
})

test_that("update_thumbnail() uploads the image", {
  captured <- mock_item_ops(list(success = TRUE, id = "i1"))
  path <- tempfile(fileext = ".png")
  writeBin(as.raw(1:10), path)

  update_thumbnail(owned_item(), path, token = token)

  expect_match(captured$req$url, "/items/i1/updateThumbnail$")
  expect_s3_class(form_values(captured$req)$file, "form_file")
  expect_error(update_thumbnail(owned_item(), "nope.png"), "does not exist")
})

test_that("lifecycle functions surface portal errors", {
  mock_item_ops(list(error = list(code = 403, message = "Item is protected")))

  expect_error(
    delete_item(owned_item(), token = token),
    "Item is protected"
  )
})
