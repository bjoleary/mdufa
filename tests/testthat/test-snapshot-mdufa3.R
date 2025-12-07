# Snapshot Test for MDUFA III 2018-12-10 Report
#
# Uses testthat's built-in snapshot testing via expect_snapshot_value().
# Snapshots are stored in tests/testthat/_snaps/snapshot-mdufa3/
#
# To update snapshots after intentional changes:
#   testthat::snapshot_accept("snapshot-mdufa3")

# Helper to find local PDF reports
find_local_pdf <- function(pattern) {
  pkg_root <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL
  )

  possible_dirs <- c(
    if (!is.null(pkg_root)) file.path(pkg_root, "data-raw", "pdf_reports"),
    "data-raw/pdf_reports",
    "../data-raw/pdf_reports",
    "../../data-raw/pdf_reports"
  )

  for (pdf_dir in possible_dirs) {
    if (!is.null(pdf_dir) && dir.exists(pdf_dir)) {
      files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
      if (length(files) > 0) {
        return(files[1])
      }
    }
  }
  NULL
}

test_that("MDUFA III 2018-12-10 extraction matches snapshot", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  # Extract from PDF
  extracted <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA III"
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
