item <- function(...) {
  structure(
    utils::modifyList(list(id = "abc123", owner = "jdoe"), list(...)),
    class = c("PortalItem", "list")
  )
}

test_that("share_access() maps access onto the wire parameters", {
  expect_identical(
    share_access("private"),
    list(everyone = "false", org = "false")
  )
  expect_identical(share_access("org"), list(everyone = "false", org = "true"))
  expect_identical(
    share_access("public"),
    list(everyone = "true", org = "false")
  )
})

test_that("share_access() rejects an unknown level", {
  expect_error(share_access("everyone"), "access")
  expect_error(share_access(c("org", "public")), "access")
})

test_that("share_item() requires an item id and owner", {
  expect_error(share_item(item(id = NULL)), "id")
  expect_error(share_item(item(owner = NULL)), "owner")
})

test_that("share_item() accepts a bare id when user is given", {
  expect_error(share_item("abc123"), "user")
  expect_no_error(share_body("abc123", "org", NULL, FALSE))
})

test_that("share_body() collapses groups", {
  body <- share_body("abc123", "org", c("g1", "g2"), FALSE)

  expect_identical(body$groups, "g1,g2")
  expect_identical(body$org, "true")
  expect_identical(body$confirmItemControl, "false")
})

test_that("share_body() omits groups when absent", {
  expect_false("groups" %in% names(share_body("abc123", "org", NULL, FALSE)))
})

test_that("share_body() accepts PortalGroup objects", {
  grp <- structure(list(id = "g1"), class = c("PortalGroup", "list"))

  expect_identical(share_body("abc123", "org", list(grp), FALSE)$groups, "g1")
})

test_that("warn_not_shared() warns on partial success", {
  expect_warning(warn_not_shared(list(notSharedWith = c("g1"))), "g1")
  expect_no_warning(warn_not_shared(list(notSharedWith = character())))
  expect_no_warning(warn_not_shared(list()))
})

test_that("item_owner_req() builds the documented endpoint", {
  req <- item_owner_req(
    list(id = "abc123", owner = "jdoe"),
    "share",
    "https://www.arcgis.com",
    NULL
  )

  expect_identical(
    req$url,
    "https://www.arcgis.com/sharing/rest/content/users/jdoe/items/abc123/share"
  )
})

test_that("item_owner() prefers an explicit user", {
  expect_identical(item_owner(item(owner = "a"), "b")$owner, "b")
  expect_identical(item_owner(item(owner = "a"), NULL)$owner, "a")
})
