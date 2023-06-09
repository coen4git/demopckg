#context("report_p")

testthat::test_that("errors", {
  testthat::expect_error(
    report_p(-1),
    "p cannot be less than 0"
  )

  testthat::expect_error(
    report_p(2),
    "p cannot be greater than 1"
  )
})

testthat::test_that("default_values", {
  testthat::expect_equal(
    report_p(p = 0.0222),
    "p = .022"
  )
})
