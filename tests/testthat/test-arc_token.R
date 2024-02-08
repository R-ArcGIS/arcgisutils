test_that("tokens set and fetched by name are identical", {
  # create fake tokens
  token_a <- httr2::oauth_token("1234")
  token_b <- httr2::oauth_token("abcd")


  # set token by name
  set_arc_token(org_a = token_a, org_b = token_b)

  # fetch token by name
  expect_identical(arc_token("org_a"), token_a)
  expect_identical(arc_token("org_b"), token_b)
})

test_that("Default token set and fetched", {

  # create fake token
  token <- httr2::oauth_token("1234")

  # set token to the default location
  set_arc_token(token)

  # fetch token from the default location
  expect_identical(arc_token(), token)
})


test_that("Default token and named tokens", {

  # create fake tokens
  token <- httr2::oauth_token("....")
  token_a <- httr2::oauth_token("1234")
  token_b <- httr2::oauth_token("abcd")


  # set token by name and default
  set_arc_token(token, org_a = token_a, org_b = token_b)

  # fetch token by name
  expect_identical(arc_token(), token)
  expect_identical(arc_token("org_a"), token_a)
  expect_identical(arc_token("org_b"), token_b)

})
