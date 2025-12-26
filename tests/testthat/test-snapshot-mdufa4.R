# Snapshot Test for MDUFA IV 2023-11-16 Report
#
# Uses testthat's built-in snapshot testing via expect_snapshot_value().
# Snapshots are stored in tests/testthat/_snaps/snapshot-mdufa4/
#
# To update snapshots after intentional changes:
#   testthat::snapshot_accept("snapshot-mdufa4")

test_that("MDUFA IV 2023-11-16 extraction matches snapshot", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  # Extract from PDF
  extracted <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA IV"
    )
  ))

  # Select columns for snapshot comparison
  snapshot_cols <- c(
    "source", "page", "table_number", "organization", "program",
    "table_title", "metric_type", "performance_metric", "fy", "value"
  )

  actual <- extracted[, snapshot_cols] |>
    dplyr::arrange(table_number, page, performance_metric, fy)

  # Use testthat's native snapshot testing
  expect_snapshot_value(actual, style = "json2")
})

test_that("MDUFA IV 2023-03-01 extraction matches snapshot", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-03-01")
  skip_if(is.null(pdf_path), "MDUFA IV 2023-03-01 PDF not available locally")

  # Extract from PDF
  extracted <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA IV"
    )
  ))

  # Select columns for snapshot comparison
  snapshot_cols <- c(
    "source", "page", "table_number", "organization", "program",
    "table_title", "metric_type", "performance_metric", "fy", "value"
  )

  actual <- extracted[, snapshot_cols] |>
    dplyr::arrange(table_number, page, performance_metric, fy)

  # Use testthat's native snapshot testing
  expect_snapshot_value(actual, style = "json2")
})
