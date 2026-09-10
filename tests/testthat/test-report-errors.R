err_response <- list(
  error = list(
    code = 400L,
    message = "Unable to generate token.",
    details = "Invalid username or password."
  )
)

test_that("report_errors() warns rather than aborting (#39)", {
  expect_warning(report_errors(err_response), "Unable to generate token")
  expect_no_error(suppressWarnings(report_errors(err_response)))
})

test_that("report_errors() is silent on a clean response (#39)", {
  expect_no_warning(report_errors(list(results = list())))
  expect_null(suppressWarnings(report_errors(list(results = list()))))
})

test_that("detect_errors() still aborts on the same response", {
  expect_error(detect_errors(err_response), "Unable to generate token")
})

test_that("report_errors() is exported (#39)", {
  expect_true("report_errors" %in% getNamespaceExports("arcgisutils"))
})
