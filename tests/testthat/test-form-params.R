test_that("as_form_params() does not double-encode string scalars", {
  features <- as_esri_features(penguins[1, ])
  params <- as_form_params(list(f = "json", features = features))

  expect_equal(params@params[["f"]], "json")
  expect_equal(params@params[["features"]], features)
})
