raw_user <- function() {
  list(
    username = "jdoe",
    created = 1700000000000,
    modified = 1700000001000,
    lastLogin = 1700000002000,
    emailStatusDate = 1700000003000,
    groups = data.frame(
      id = c("a", "b"),
      created = c(1700000004000, 1700000005000),
      modified = c(1700000006000, 1700000007000)
    )
  )
}

test_that("as_portal_user() parses every date field (#75)", {
  user <- as_portal_user(raw_user())

  for (field in c("created", "modified", "lastLogin", "emailStatusDate")) {
    expect_s3_class(user[[field]], "POSIXct")
  }
})

test_that("as_portal_user() makes groups a tbl with parsed dates (#75)", {
  user <- as_portal_user(raw_user())

  expect_s3_class(user[["groups"]], "tbl")
  expect_s3_class(user[["groups"]][["created"]], "POSIXct")
  expect_s3_class(user[["groups"]][["modified"]], "POSIXct")
})

test_that("as_portal_user() tolerates missing fields (#75)", {
  user <- as_portal_user(list(username = "jdoe"))

  expect_s3_class(user, "PortalUser")
  expect_null(user[["lastLogin"]])
  expect_null(user[["groups"]])
})

test_that("as_portal_user() returns a PortalUser (#75)", {
  expect_s3_class(as_portal_user(raw_user()), "PortalUser")
})
