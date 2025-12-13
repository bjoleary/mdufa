# Test that all bundled datasets have expected columns in expected order

test_that("mdufa2 has expected columns", {
  expect_equal(names(mdufa2), mdufa_cols)
})

test_that("mdufa3 has expected columns", {
  expect_equal(names(mdufa3), mdufa_cols)
})

test_that("mdufa4 has expected columns", {
  expect_equal(names(mdufa4), mdufa_cols)
})

test_that("mdufa5 has expected columns", {
  expect_equal(names(mdufa5), mdufa_cols)
})

test_that("mdufa_combined has expected columns", {
  expect_equal(names(mdufa_combined), mdufa_combined_cols)
})

test_that("cohort_status has expected columns", {
  expect_equal(names(cohort_status), cohort_status_cols)
})

test_that("report_dates has expected columns", {
  expect_equal(names(report_dates), report_dates_cols)
})

test_that("validate_columns catches missing columns", {
  test_data <- tibble::tibble(a = 1, b = 2)
  expect_error(
    validate_columns(test_data, c("a", "b", "c"), "test"),
    "missing columns: c"
  )
})

test_that("validate_columns catches extra columns", {
  test_data <- tibble::tibble(a = 1, b = 2, c = 3)
  expect_error(
    validate_columns(test_data, c("a", "b"), "test"),
    "unexpected columns: c"
  )
})

test_that("validate_columns catches wrong order", {
  test_data <- tibble::tibble(b = 2, a = 1)
  expect_error(
    validate_columns(test_data, c("a", "b"), "test"),
    "out of order"
  )
})

test_that("validate_columns passes for correct structure", {
  test_data <- tibble::tibble(a = 1, b = 2)
  expect_true(validate_columns(test_data, c("a", "b"), "test"))
})

test_that("standardize_columns reorders correctly", {
  test_data <- tibble::tibble(b = 2, a = 1, c = 3)
  result <- standardize_columns(test_data, c("a", "b", "c"))
  expect_equal(names(result), c("a", "b", "c"))
})

test_that("standardize_columns errors on missing columns", {
  test_data <- tibble::tibble(a = 1, b = 2)
  expect_error(
    standardize_columns(test_data, c("a", "b", "c")),
    "missing columns: c"
  )
})
