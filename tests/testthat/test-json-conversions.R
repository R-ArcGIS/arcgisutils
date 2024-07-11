test_that("as_esri_features() respects integers", {
  x <- data.frame(
    objectid = 1:5,
    value = sample(c("y", "n"), 5, TRUE)
  )

  y <- as_features(x)
  expect_false(any(grepl(".0", y)))
})
