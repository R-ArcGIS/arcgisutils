test_that("obj_check_token() points at the auth_ functions (#37)", {
  expect_error(obj_check_token(NULL), "auth_code")
  expect_error(obj_check_token(NULL), "set_arc_token")
})

test_that("obj_check_token() still reports the offending class (#37)", {
  expect_error(obj_check_token(NULL), "httr2_token")
  expect_error(obj_check_token(1L), "integer")
})

test_that("obj_check_token() accepts a well formed token", {
  token <- structure(
    list(access_token = "x", arcgis_host = "https://www.arcgis.com"),
    class = "httr2_token"
  )
  expect_identical(obj_check_token(token), token)
})
