# Regression tests for https://github.com/R-ArcGIS/arcgisutils/issues/83
#
# An empty column read from a CSV comes back as logical NA, not a typed NA.
# Before the fix `vec_mapping` had no "logical" entry, so `as_fields()` produced
# a field with type NA and `data.frame()` aborted with "row names contain
# missing values" -- an error that gave no hint about the real cause.
#
# Esri has no boolean field type, so logical maps to esriFieldTypeSmallInteger.

test_that("logical column maps to esriFieldTypeSmallInteger (#83)", {
  fields <- as_fields(data.frame(x = 1L, y = c(TRUE, FALSE)[1]))
  expect_identical(
    fields$type[fields$name == "y"],
    "esriFieldTypeSmallInteger"
  )
})

test_that("all-NA logical column maps to a valid Esri field type (#83)", {
  fields <- as_fields(data.frame(x = 1L, y = NA))
  y_type <- fields$type[fields$name == "y"]

  expect_false(is.na(y_type))
  expect_identical(y_type, "esriFieldTypeSmallInteger")
})

test_that("all-NA logical column converts to EsriJSON as null (#83)", {
  json <- as_esri_features(data.frame(x = 1L, y = NA))
  expect_true(grepl('"y":null', json, fixed = TRUE))
})
