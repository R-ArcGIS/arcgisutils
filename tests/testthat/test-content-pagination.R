test_that("content functions expose pagination args (#77)", {
  for (fn in list(arc_user_content, arc_group_content)) {
    args <- formals(fn)
    expect_true(all(c("page_size", "max_pages", ".progress") %in% names(args)))
    expect_equal(args$page_size, 50)
    expect_equal(args$max_pages, quote(Inf))
    expect_true(args$.progress)
  }
})

test_that("content functions validate pagination args before requesting (#77)", {
  expect_error(arc_user_content("someone", page_size = 0), "page_size")
  expect_error(arc_user_content("someone", page_size = 101), "page_size")
  expect_error(arc_user_content("someone", max_pages = 0), "max_pages")
  expect_error(arc_user_content("someone", .progress = "yes"), "progress")

  expect_error(arc_group_content("abc123", page_size = 0), "page_size")
  expect_error(arc_group_content("abc123", max_pages = 0), "max_pages")
})

test_that("content functions still reject bad identifiers", {
  expect_error(arc_user_content(1L), "must be a string")
  expect_error(arc_group_content(1L), "must be a string")
})
