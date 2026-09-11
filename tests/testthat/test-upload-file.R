fake_token <- function() {
  httr2::oauth_token("1234", arcgis_host = arc_host(), username = "jdoe")
}

csv_path <- function() {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(address = "380 New York St"),
    path,
    row.names = FALSE
  )
  path
}

capture_req <- function(env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs <- c(captured$reqs, list(req))
      httr2::response_json(body = list(success = TRUE, id = "abc123"))
    },
    env = env
  )
  captured
}

test_that("upload_file() posts a multipart request to addItem", {
  path <- csv_path()
  captured <- capture_req()

  try(upload_file(path, "Iris", "CSV", token = fake_token()), silent = TRUE)

  body <- captured$reqs[[1]][["body"]][["data"]]
  expect_match(captured$reqs[[1]][["url"]], "/sharing/rest/content/users/jdoe/addItem")
  expect_identical(body[["type"]], "CSV")
  expect_identical(body[["title"]], "Iris")
  expect_s3_class(body[["file"]], "form_file")
})

test_that("upload_file() uploads into a folder when given one", {
  path <- csv_path()
  captured <- capture_req()

  try(
    upload_file(path, "Iris", "CSV", folder = "f01", token = fake_token()),
    silent = TRUE
  )

  expect_match(captured$reqs[[1]][["url"]], "/users/jdoe/f01/addItem")
})

test_that("upload_file() collapses tags and drops empty fields", {
  path <- csv_path()
  captured <- capture_req()

  try(
    upload_file(path, "Iris", "CSV", tags = c("a", "b"), token = fake_token()),
    silent = TRUE
  )

  body <- captured$reqs[[1]][["body"]][["data"]]
  expect_identical(body[["tags"]], "a,b")
  expect_false("description" %in% names(body))
  expect_false("snippet" %in% names(body))
})

test_that("upload_file() rejects an unknown item type", {
  path <- csv_path()
  expect_error(upload_file(path, "Iris", "Not A Type", token = fake_token()))
})

test_that("upload_file() requires the file to exist", {
  expect_error(
    upload_file("nope.csv", "Iris", "CSV", token = fake_token()),
    "does not exist"
  )
})

test_that("upload_file() validates its arguments", {
  path <- csv_path()
  expect_error(upload_file(path, "Iris", "CSV", tags = 1, token = fake_token()))
  expect_error(upload_file(path, "Iris", "CSV", folder = "", token = fake_token()))
  expect_error(upload_file(path, 1, "CSV", token = fake_token()))
})

test_that("upload_file() requires a token with a username", {
  path <- csv_path()
  expect_error(
    upload_file(
      path,
      "Iris",
      "CSV",
      token = httr2::oauth_token("1234", arcgis_host = arc_host())
    ),
    "username"
  )
})

test_that("every mapped extension is a real portal item type", {
  expect_true(all(item_type_extensions %in% portal_item_types()))
})

test_that("item_type is an s7x enum", {
  expect_s3_class(item_type("CSV"), "s7x::Enum")
  expect_identical(item_type("CSV")@value, "CSV")
  expect_identical(as.character(item_type("Shapefile")), "Shapefile")
  expect_error(item_type("Not A Type"))
  expect_error(item_type(NA_character_))
})

test_that("type is inferred from the file extension", {
  expect_identical(infer_item_type("a/b/data.csv"), "CSV")
  expect_identical(infer_item_type("DATA.CSV"), "CSV")
  expect_identical(infer_item_type("map.geojson"), "GeoJson")
  expect_identical(infer_item_type("book.xlsx"), "Microsoft Excel")
  expect_identical(infer_item_type("pic.TIF"), "Image")
})

test_that("ambiguous or unknown extensions are not inferred", {
  expect_error(infer_item_type("shapes.zip"), "cannot be inferred")
  expect_error(infer_item_type("notes.wat"), "cannot be inferred")
  expect_error(infer_item_type("README"), "cannot be inferred")
})

test_that("upload_file() infers the type when it is not given", {
  path <- csv_path()
  captured <- capture_req()

  try(upload_file(path, "Iris", token = fake_token()), silent = TRUE)

  expect_identical(captured$reqs[[1]][["body"]][["data"]][["type"]], "CSV")
})

test_that("upload_file() uploads a parquet file to a portal", {
  skip_on_cran()
  skip_on_ci()
  skip_if(!interactive(), "Uploads to a live portal")
  skip_if(getRversion() < "4.5.0", "`penguins` was added to datasets in R 4.5.0")
  skip_if_not_installed("nanoparquet")

  set_arc_token(auth_user())

  path <- file.path(tempdir(), "penguins.parquet")
  nanoparquet::write_parquet(datasets::penguins, path)

  item <- upload_file(path, "penguins")
  on.exit(delete_items(item), add = TRUE)

  expect_s3_class(item, "PortalItem")
  expect_identical(item[["title"]], "penguins")
  expect_identical(item[["type"]], "Apache Parquet")
})
