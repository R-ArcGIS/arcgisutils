host <- "https://www.arcgis.com"
token <- httr2::oauth_token("1234", arcgis_host = host, username = "jdoe")

# routes each request by its final path segment
mock_upload <- function(fail_on = NULL, env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  captured$reqs <- list()
  captured$part_sizes <- integer()
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs[[length(captured$reqs) + 1]] <- req
      op <- sub("\\?.*$", "", basename(req$url))

      if (identical(op, fail_on)) {
        return(httr2::response_json(
          body = list(error = list(code = 500, message = "Part rejected"))
        ))
      }

      if (op == "addPart") {
        captured$part_sizes <- c(
          captured$part_sizes,
          file.size(req$body$data$file$path)
        )
      }

      body <- switch(
        op,
        addItem = list(success = TRUE, id = "big1", folder = ""),
        status = list(status = "completed", itemId = "big1"),
        big1 = list(id = "big1", owner = "jdoe", title = "Big"),
        list(success = TRUE, id = "big1")
      )
      httr2::response_json(body = body)
    },
    env = env
  )
  captured
}

ops <- function(captured) {
  vapply(
    captured$reqs,
    function(r) sub("\\?.*$", "", basename(r$url)),
    character(1)
  )
}

big_file <- function(mb) {
  path <- tempfile(fileext = ".csv")
  writeBin(raw(floor(mb * 1024^2)), path)
  path
}

test_that("upload_large_file() adds, uploads parts, commits, waits, and updates", {
  captured <- mock_upload()
  path <- big_file(11)

  res <- upload_large_file(
    path,
    "Big",
    tags = c("a", "b"),
    part_size = 5,
    .progress = FALSE,
    token = token
  )

  expect_identical(
    ops(captured),
    c("addItem", "addPart", "addPart", "addPart", "commit", "status", "update", "big1")
  )
  expect_equal(captured$part_sizes, c(5, 5, 1) * 1024^2)

  add <- captured$reqs[[1]]
  expect_match(add$url, "/content/users/jdoe/addItem$")
  expect_identical(add$body$data$multipart, "true")
  expect_identical(add$body$data$filename, basename(path))

  part_nums <- vapply(
    captured$reqs[2:4],
    function(r) r$body$data$partNum,
    character(1)
  )
  expect_identical(part_nums, c("1", "2", "3"))
  expect_match(captured$reqs[[2]]$url, "/content/users/jdoe/items/big1/addPart$")

  commit <- captured$reqs[[5]]$body$data
  expect_identical(utils::URLdecode(commit$type), "CSV")
  expect_identical(utils::URLdecode(commit$tags), "a,b")

  expect_s3_class(res, "PortalItem")
  expect_identical(res$id, "big1")
})

test_that("upload_large_file() uploads a small file as one part into a folder", {
  captured <- mock_upload()
  path <- big_file(0.01)

  upload_large_file(path, "Small", folder = "f1", .progress = FALSE, token = token)

  expect_match(captured$reqs[[1]]$url, "/content/users/jdoe/f1/addItem$")
  expect_identical(sum(ops(captured) == "addPart"), 1L)
})

test_that("upload_large_file() deletes the partial item when a step fails", {
  captured <- mock_upload(fail_on = "addPart")
  path <- big_file(0.01)

  expect_error(
    upload_large_file(path, "Big", .progress = FALSE, token = token),
    "Deleted the partial item"
  )
  expect_identical(utils::tail(ops(captured), 1), "delete")
  expect_match(
    utils::tail(captured$reqs, 1)[[1]]$url,
    "/content/users/jdoe/items/big1/delete$"
  )
})

test_that("upload_large_file() validates its arguments", {
  path <- big_file(0.01)
  expect_error(
    upload_large_file(path, "Big", part_size = 1, token = token),
    "part_size"
  )
  expect_error(upload_large_file("nope.csv", "Big", token = token), "does not exist")
  unknown <- tempfile(fileext = ".xyz")
  writeLines("x", unknown)
  expect_error(upload_large_file(unknown, "Big", token = token), "inferred")
  expect_error(upload_large_file(path, "Big", token = NULL), "token")
})
