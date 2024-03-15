test_that("multiplication works", {

  string <- paste(
    readLines("testdata/null-polyline.json", warn = FALSE),
    collapse = ""
  )

  expect_no_error(parse_esri_json(string))
})
