with_list_col <- function() {
  df <- data.frame(x = 1:2)
  df$y <- list(1:2, 3:4)
  df
}

test_that("as_fields() returns NA for unmapped columns (#49)", {
  fields <- as_fields(with_list_col())

  expect_identical(fields$name, c("x", "y"))
  expect_identical(fields$type[fields$name == "x"], "esriFieldTypeInteger")
  expect_true(is.na(fields$type[fields$name == "y"]))
})

test_that("as_fields() row names are never NA (#49)", {
  expect_no_error(as_fields(with_list_col()))
  expect_false(anyNA(row.names(as_fields(with_list_col()))))
})

test_that("as_featureset() names the offending column and type (#49)", {
  expect_error(as_featureset(with_list_col()), "y")
  expect_error(as_featureset(with_list_col()), "EsriJSON")
  expect_error(as_featureset(with_list_col()), "list")
})

test_that("as_featureset() names every offending column (#49)", {
  df <- data.frame(x = 1:2)
  df$y <- list(1:2, 3:4)
  df$z <- complex(real = 1:2, imaginary = 1:2)

  expect_error(as_featureset(df), "y")
  expect_error(as_featureset(df), "z")
})
