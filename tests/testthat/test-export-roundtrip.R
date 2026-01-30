# Column names expected in all mdufa datasets
mdufa_cols <- c(
  "report_description", "report_link", "report_date", "report_mdufa_period",
  "source", "page", "table_number", "organization", "program", "table_title",
  "metric_type", "performance_metric", "fy", "value"
)

test_that("Excel round-trip preserves mdufa3 data", {
  skip_if_not_installed("readxl")

  filepath <- tempfile(fileext = ".xlsx")
  suppressWarnings(export_excel(mdufa::mdufa3, filepath))
  imported <- readxl::read_excel(filepath, sheet = "data")

  expect_equal(nrow(imported), nrow(mdufa::mdufa3))
  expect_true(all(mdufa_cols %in% names(imported)))
  expect_equal(imported$value, mdufa::mdufa3$value)

  # Names attribute on performance_metric is lost during Excel roundtrip

  expect_equal(
    imported$performance_metric,
    mdufa::mdufa3$performance_metric,
    ignore_attr = TRUE
  )
})

test_that("Excel round-trip preserves mdufa4 data", {
  skip_if_not_installed("readxl")

  filepath <- tempfile(fileext = ".xlsx")
  suppressWarnings(export_excel(mdufa::mdufa4, filepath))
  imported <- readxl::read_excel(filepath, sheet = "data")

  # export_excel() filters out rows with NA metric_type; after re-extraction
  # with updated metric_types(), only rows with NA performance_metric remain
  expected <- mdufa::mdufa4[!is.na(mdufa::mdufa4$performance_metric), ]

  expect_equal(nrow(imported), nrow(expected))
  expect_true(all(mdufa_cols %in% names(imported)))
  expect_equal(imported$value, expected$value)
  expect_equal(imported$performance_metric, expected$performance_metric)
})

test_that("Excel round-trip preserves mdufa5 data", {
  skip_if_not_installed("readxl")

  filepath <- tempfile(fileext = ".xlsx")
  suppressWarnings(export_excel(mdufa::mdufa5, filepath))
  imported <- readxl::read_excel(filepath, sheet = "data")

  expect_equal(nrow(imported), nrow(mdufa::mdufa5))
  expect_true(all(mdufa_cols %in% names(imported)))
  expect_equal(imported$value, mdufa::mdufa5$value)
  expect_equal(imported$performance_metric, mdufa::mdufa5$performance_metric)
})
