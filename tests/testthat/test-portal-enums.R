test_that("enums accept their variants", {
  expect_s3_class(ItemAccess("org"), "s7x::Enum")
  expect_identical(as.character(ItemAccess("public")), "public")
  expect_identical(as.character(RelationshipDirection("reverse")), "reverse")
  expect_identical(as.character(RelationshipType("Service2Data")), "Service2Data")
})

test_that("enums reject values outside their variants", {
  expect_error(ItemAccess("everyone"))
  expect_error(RelationshipDirection("backward"))
  expect_error(SortOrder("ascending"))
  expect_error(RelationshipType("Service2Datum"))
})

test_that("enums reject NA", {
  expect_error(ItemAccess(NA_character_))
  expect_error(RelationshipType(NA_character_))
})

test_that("RelationshipType covers every documented value", {
  expect_length(RelationshipType("Map2Service")@variants, 45)
  expect_true(all(c("Map2Service", "WMA2Code", "Notebook2WebTool") %in% RelationshipType("Map2Service")@variants))
})

test_that("ItemSortField matches the search API fields", {
  for (f in c("title", "created", "modified", "numViews")) {
    expect_identical(as.character(ItemSortField(f)), f)
  }
})
