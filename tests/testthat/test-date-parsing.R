test_that("date field parsing works", {

  fp <- test_path("testdata", "date-parse-error.json")
  x <- readLines(fp)
  res <- parse_esri_json(x)
  expect_snapshot(res)
})
