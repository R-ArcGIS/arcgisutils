test_that("multiplication works", {
  correct <- r"([{"attributes":{"OBJECTID":100.0,"RelativeDepthTypeCode":null}}])"

  created <- as_esri_features(
    data.frame(
      OBJECTID = 100,
      RelativeDepthTypeCode = NA_character_
    )
  )
  expect_identical(created, correct)
})
