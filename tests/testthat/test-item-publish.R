host <- "https://www.arcgis.com"
token <- httr2::oauth_token("1234", arcgis_host = host, username = "jdoe")

mock_sequence <- function(..., env = parent.frame()) {
  bodies <- list(...)
  captured <- new.env(parent = emptyenv())
  captured$reqs <- list()
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs[[length(captured$reqs) + 1]] <- req
      i <- min(length(captured$reqs), length(bodies))
      httr2::response_json(body = bodies[[i]])
    },
    env = env
  )
  captured
}

csv_item <- structure(
  list(id = "i1", owner = "jdoe", type = "CSV"),
  class = c("PortalItem", "list")
)

analysis <- list(
  publishParameters = list(
    type = "csv",
    layerInfo = list(fields = list(list(name = "lon"), list(name = "lat")))
  ),
  records = list(list(attributes = list(lon = 1, lat = 2)))
)

test_that("analyze_file() analyzes an item and keeps arrays as lists", {
  captured <- mock_sequence(analysis)

  res <- analyze_file(
    csv_item,
    analyze_parameters = list(locationType = "coordinates"),
    token = token
  )

  q <- httr2::url_parse(captured$reqs[[1]]$url)$query
  expect_match(captured$reqs[[1]]$url, "/sharing/rest/content/features/analyze\\?")
  expect_identical(q$itemid, "i1")
  expect_identical(q$filetype, "csv")
  expect_identical(q$analyzeParameters, '{"locationType":"coordinates"}')
  expect_type(res$publishParameters$layerInfo$fields, "list")
  expect_identical(res$publishParameters$layerInfo$fields[[2]]$name, "lat")
})

test_that("analyze_file() uploads a local file", {
  captured <- mock_sequence(analysis)
  path <- tempfile(fileext = ".xlsx")
  writeBin(as.raw(1:10), path)

  analyze_file(path = path, token = token)

  body <- captured$reqs[[1]]$body
  expect_identical(body$type, "multipart")
  expect_s3_class(body$data$file, "form_file")
  expect_identical(body$data$filetype, "excel")
})

test_that("analyze_file() validates its inputs", {
  expect_error(analyze_file(token = token), "exactly one")
  expect_error(analyze_file(csv_item, path = "x.csv", token = token), "exactly one")
  expect_error(analyze_file("i1", token = token), "infer")
  expect_error(analyze_file("i1", file_type = "pdf", token = token), "file_type")
  expect_error(analyze_file(path = "x.txt", token = token), "infer")
  expect_error(
    analyze_file(csv_item, analyze_parameters = list("address"), token = token),
    "named list"
  )
})

test_that("analyze_file() surfaces portal errors", {
  mock_sequence(list(error = list(code = 400, message = "File too large")))
  expect_error(
    analyze_file(csv_item, token = token),
    "File too large"
  )
})

test_that("item_status() defaults to the signed in user for a bare ID", {
  captured <- mock_sequence(list(status = "processing", itemId = "i1"))

  res <- item_status("i1", "job-1", "export", token = token)

  url <- captured$reqs[[1]]$url
  expect_match(url, "/content/users/jdoe/items/i1/status\\?")
  q <- httr2::url_parse(url)$query
  expect_identical(q$jobId, "job-1")
  expect_identical(q$jobType, "export")
  expect_identical(res$status, "processing")
})

test_that("item_status() validates the job arguments", {
  expect_error(item_status("i1", "job-1", token = token), "job_type")
  expect_error(item_status("i1", "job-1", "upload", token = token), "job_type")
  expect_error(item_status("i1", token = NULL), "token")
})

test_that("await_item() polls until the job completes", {
  captured <- mock_sequence(
    list(status = "processing"),
    list(status = "processing"),
    list(status = "completed", itemId = "i1")
  )

  res <- await_item(
    "i1",
    "job-1",
    "publish",
    interval = 0,
    token = token
  )

  expect_length(captured$reqs, 3)
  expect_identical(res$status, "completed")
})

test_that("await_item() errors when the job fails or times out", {
  mock_sequence(list(status = "failed", statusMessage = "Bad geometry"))
  expect_error(
    await_item("i1", interval = 0, token = token),
    "Bad geometry"
  )

  mock_sequence(list(status = "processing"))
  expect_error(
    await_item("i1", interval = 0.01, timeout = 0, token = token),
    "did not finish"
  )
})

test_that("export_item() posts to the signed in user's export", {
  captured <- mock_sequence(list(
    exportItemId = "e1",
    jobId = "job-1",
    exportFormat = "csv"
  ))

  res <- export_item(
    csv_item,
    "csv",
    title = "Export",
    export_parameters = list(layers = list(list(id = 0L, where = "x > 1"))),
    token = token
  )

  req <- captured$reqs[[1]]
  expect_match(req$url, "/sharing/rest/content/users/jdoe/export$")
  form <- lapply(req$body$data, function(x) utils::URLdecode(as.character(x)))
  expect_identical(form$itemId, "i1")
  expect_identical(form$exportFormat, "csv")
  expect_identical(form$title, "Export")
  expect_identical(form$exportParameters, '{"layers":[{"id":0,"where":"x > 1"}]}')
  expect_identical(res$exportItemId, "e1")
})

test_that("export_item() validates the format", {
  expect_error(export_item("i1", "parquet", token = token), "must be one of")
})
