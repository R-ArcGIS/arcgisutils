mock_search <- function(results, env = parent.frame()) {
  captured <- new.env(parent = emptyenv())
  captured$reqs <- list()
  httr2::local_mocked_responses(
    function(req) {
      captured$reqs[[length(captured$reqs) + 1]] <- req
      body <- list(total = length(results), start = 1, nextStart = -1)
      body[["results"]] <- results
      httr2::response_json(body = body)
    },
    env = env
  )
  captured
}

query_of <- function(req) httr2::url_parse(req$url)$query

test_that("search_groups() queries community/groups", {
  captured <- mock_search(list(
    list(id = "g1", title = "Parks", created = 1700000000000)
  ))

  res <- search_groups(
    "parks",
    sort_field = "title",
    sort_order = "desc",
    .progress = FALSE,
    host = "https://www.arcgis.com",
    token = NULL
  )

  req <- captured$reqs[[1]]
  expect_match(req$url, "/sharing/rest/community/groups\\?")
  expect_identical(query_of(req)$q, "parks")
  expect_identical(query_of(req)$sortField, "title")
  expect_identical(query_of(req)$sortOrder, "desc")
  expect_null(query_of(req)$searchUserAccess)
  expect_identical(res$id, "g1")
  expect_s3_class(res$created, "POSIXct")
})

test_that("search_groups() limits to the user's groups with member_only", {
  captured <- mock_search(list(list(id = "g1")))

  search_groups(
    filter = 'owner:"jdoe"',
    member_only = TRUE,
    .progress = FALSE,
    host = "https://www.arcgis.com",
    token = NULL
  )

  q <- query_of(captured$reqs[[1]])
  expect_identical(q$searchUserAccess, "groupMember")
  expect_identical(q$filter, 'owner:"jdoe"')
  expect_null(q$q)
})

test_that("search_users() queries community/users", {
  captured <- mock_search(list(list(username = "asmith")))
  res <- search_users(
    "asmith",
    sort_field = "fullname",
    .progress = FALSE,
    host = "https://www.arcgis.com",
    token = NULL
  )
  expect_match(captured$reqs[[1]]$url, "/sharing/rest/community/users\\?")
  expect_identical(res$username, "asmith")
})

test_that("search_group_content() searches inside one group", {
  captured <- mock_search(list(list(id = "i1", title = "Parcels")))
  grp <- structure(list(id = "g1"), class = c("PortalGroup", "list"))

  res <- search_group_content(
    grp,
    "parcels",
    categories = c("/Categories/Water", "/Categories/Forest"),
    sort_field = "added",
    .progress = FALSE,
    host = "https://www.arcgis.com",
    token = NULL
  )

  req <- captured$reqs[[1]]
  expect_match(req$url, "/sharing/rest/content/groups/g1/search\\?")
  expect_identical(
    query_of(req)$categories,
    "/Categories/Water,/Categories/Forest"
  )
  expect_identical(res$id, "i1")
})

test_that("search functions return zero rows when nothing matches", {
  mock_search(list())
  res <- search_groups(
    "nothing",
    .progress = FALSE,
    host = "https://www.arcgis.com",
    token = NULL
  )
  expect_identical(nrow(res), 0L)
})

test_that("search functions validate their arguments", {
  expect_error(search_groups(), "query")
  expect_error(search_groups("x", sort_field = "numviews"), "sort_field")
  expect_error(search_users("x", sort_field = "title"), "sort_field")
  expect_error(search_users("x", sort_order = "up"), "SortOrder")
  expect_error(search_group_content(1, "x"), "group")
  expect_error(
    search_group_content("g1", "x", categories = letters[1:9]),
    "categories"
  )
})
