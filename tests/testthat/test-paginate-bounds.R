req <- function() arc_base_req("https://www.arcgis.com")

test_that("arc_paginate_req() enforces page_size bounds", {
  expect_error(arc_paginate_req(req(), page_size = 0), "page_size")
  expect_error(arc_paginate_req(req(), page_size = 101), "page_size")
  expect_error(arc_paginate_req(req(), page_size = 1.5), "page_size")
})

test_that("arc_paginate_req() enforces max_pages and .progress", {
  expect_error(arc_paginate_req(req(), max_pages = 0), "max_pages")
  expect_error(arc_paginate_req(req(), .progress = "yes"), "progress")
})

test_that("arc_paginate_req() rejects a non-request", {
  expect_error(arc_paginate_req("not a request"), "httr2_request")
})

test_that("is_url() reports errors against its own caller", {
  expect_error(is_url(1L), "character")
})
