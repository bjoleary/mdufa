# Snapshot Test for MDUFA V 2025-02-27 Report
#
# Uses testthat's built-in snapshot testing via expect_snapshot_value().
# Snapshots are stored in tests/testthat/_snaps/snapshot-mdufa5/
#
# To update snapshots after intentional changes:
#   testthat::snapshot_accept("snapshot-mdufa5")

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

# Helper to remove 508 compliance coversheet if present
# FDA added these coversheets after some reports were originally downloaded
remove_coversheet_if_present <- function(pdf_path) {
  # Check if first page contains 508 compliance text
  first_page <- pdftools::pdf_text(pdf_path)[1]

  if (grepl("508", first_page, ignore.case = TRUE) ||
    grepl("accessibility", first_page, ignore.case = TRUE) ||
    grepl("Section 508", first_page, ignore.case = TRUE)) {
    # Remove first page using qpdf
    if (!requireNamespace("qpdf", quietly = TRUE)) {
      skip("qpdf package required to handle 508 coversheet")
    }

    n_pages <- pdftools::pdf_info(pdf_path)$pages
    temp_pdf <- tempfile(fileext = ".pdf")
    qpdf::pdf_subset(pdf_path, pages = 2:n_pages, output = temp_pdf)
    return(temp_pdf)
  }

  pdf_path
}

test_that("MDUFA V 2025-02-27 extraction matches snapshot", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2025-02-27")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  # Handle 508 coversheet if present
  pdf_path <- remove_coversheet_if_present(pdf_path)

  # Extract from PDF
  extracted <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA V"
    )
  ))

  # Ensure page is integer for consistent comparison
  extracted$page <- as.integer(extracted$page)

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

test_that("MDUFA V 2023-03-01 extraction matches snapshot", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-03-01")
  skip_if(is.null(pdf_path), "MDUFA V 2023-03-01 PDF not available locally")

  # Handle 508 coversheet if present
  pdf_path <- remove_coversheet_if_present(pdf_path)

  # Extract from PDF
  extracted <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA V"
    )
  ))

  # Ensure page is integer for consistent comparison
  extracted$page <- as.integer(extracted$page)

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
