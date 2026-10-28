item <- function(...) {
  structure(
    utils::modifyList(list(id = "abc123", owner = "jdoe"), list(...)),
    class = c("PortalItem", "list")
  )
}

test_that("share_item() rejects an unknown access level", {
  expect_error(share_item(item(), "everyone"), "ItemAccess")
  expect_error(share_item(item(), c("org", "public")), "ItemAccess")
})

test_that("share_item() requires a logical confirm_item_control", {
  expect_error(
    share_item(item(), "org", confirm_item_control = "yes"),
    "confirm_item_control"
  )
})

test_that("share_item() requires an item id and owner", {
  expect_error(share_item(item(id = NULL)), "id")
  expect_error(share_item(item(owner = NULL)), "owner")
})

test_that("share_item() requires user for a bare id", {
  expect_error(share_item("abc123"), "user")
})

test_that("item_owner() prefers an explicit user", {
  expect_identical(item_owner(item(owner = "a"), "b")$owner, "b")
  expect_identical(item_owner(item(owner = "a"), NULL)$owner, "a")
})
