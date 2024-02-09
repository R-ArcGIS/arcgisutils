test_that("tokens set and fetched by name are identical", {
  # create fake tokens
  token_a <- httr2::oauth_token("1234", arcgis_host = arc_host())
  token_b <- httr2::oauth_token("abcd", arcgis_host = arc_host())

  # set token by name
  set_arc_token(org_a = token_a, org_b = token_b)

  # fetch token by name
  expect_identical(arc_token("org_a"), token_a)
  expect_identical(arc_token("org_b"), token_b)
})

test_that("Default token set and fetched", {

  # create fake token
  token <- httr2::oauth_token("1234", arcgis_host = arc_host())

  # set token to the default location
  set_arc_token(token)

  # fetch token from the default location
  expect_identical(arc_token(), token)
})


test_that("Default token and named tokens", {

  # create fake tokens
  token <- httr2::oauth_token("....", arcgis_host = arc_host())
  token_a <- httr2::oauth_token("1234", arcgis_host = arc_host())
  token_b <- httr2::oauth_token("abcd", arcgis_host = arc_host())


  # set token by name and default
  set_arc_token(token, org_a = token_a, org_b = token_b)

  # fetch token by name
  expect_identical(arc_token(), token)
  expect_identical(arc_token("org_a"), token_a)
  expect_identical(arc_token("org_b"), token_b)

})


test_that("Host must be set:", {
  token <- httr2::oauth_token("....")
  expect_error(
    obj_check_token(token),
    "`token` does not have \"arcgis_host\""
  )
})

test_that("Only one host can be set:", {
  token <- httr2::oauth_token("....", arcgis_host = c("a", "b"))
  expect_error(
    obj_check_token(token),
    "`token` has more than one \"arcgis_host\""
  )
})
