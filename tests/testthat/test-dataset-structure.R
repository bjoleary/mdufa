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

# Test uniqueness of metrics in bundled datasets

test_that("mdufa2 has unique metrics", {
  expect_true(validate_unique_metrics(mdufa2, "mdufa2"))
})

test_that("mdufa3 has unique metrics", {
  expect_true(validate_unique_metrics(mdufa3, "mdufa3"))
})

test_that("mdufa4 has unique metrics", {
  expect_true(validate_unique_metrics(mdufa4, "mdufa4"))
})

test_that("mdufa5 has unique metrics", {
  expect_true(validate_unique_metrics(mdufa5, "mdufa5"))
})

test_that("mdufa_combined has unique metrics", {
  # mdufa_combined includes derived rows, so key includes derived flag
  key_cols <- c(
    "table_number", "organization", "program",
    "performance_metric", "fy", "derived"
  )
  expect_true(validate_unique_metrics(mdufa_combined, "mdufa_combined",
    key_cols = key_cols
  ))
})

test_that("validate_unique_metrics catches duplicates", {
  test_data <- tibble::tibble(
    table_number = c("1.1", "1.1"),
    organization = c("CDRH", "CDRH"),
    program = c("510(k)", "510(k)"),
    performance_metric = c("Count", "Count"),
    fy = c("2023", "2023"),
    value = c("100", "100")
  )
  expect_error(
    validate_unique_metrics(test_data, "test"),
    "duplicate key combinations"
  )
})

test_that("validate_unique_metrics passes for unique data", {
  test_data <- tibble::tibble(
    table_number = c("1.1", "1.1"),
    organization = c("CDRH", "CDRH"),
    program = c("510(k)", "510(k)"),
    performance_metric = c("Count", "Count"),
    fy = c("2023", "2024"), # Different FY makes it unique
    value = c("100", "110")
  )
  expect_true(validate_unique_metrics(test_data, "test"))
})
