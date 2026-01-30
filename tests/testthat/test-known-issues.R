# Known extraction issues documented as skipped tests
#
# This file documents known issues with MDUFA PDF extraction.
# Each test contains real assertions that will pass once the issue is fixed.
# When an issue is fixed, remove the skip() to enable the test.

test_that("MDUFA V page footers are not extracted as data", {
  # KNOWN ISSUE: PDF page footers containing footnote text are sometimes
  # parsed as data rows. This occurs when footnote text at the bottom of
  # a PDF page (explaining goals for future FYs) is captured as a metric.
  #
  # Example from MDUFA V 2025-08-27, page 215, table 9.2:
  #   Metric: "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal..."
  #   Value: NA
  #   FY: 2023
  #
  # See: local/known_issues_mdufa5_2025-08-27.md

  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2025-08-27")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- suppressMessages(suppressWarnings(
    extract_report(pdf_path, mdufa_period = "MDUFA V")
  ))

  # Test: No metrics should contain "FYs 2025, 2026, and 2027"
  footer_rows <- data |>
    dplyr::filter(grepl("FYs 2025, 2026, and 2027", performance_metric))

  expect_equal(
    nrow(footer_rows), 0,
    label = "Rows with page footer text"
  )

  # Test: No metrics should contain "maximum number of submissions"
  footer_rows2 <- data |>
    dplyr::filter(grepl("maximum number of submissions", performance_metric))

  expect_equal(
    nrow(footer_rows2), 0,
    label = "Rows with footnote continuation text"
  )
})

test_that("MDUFA III Table 5.3 header rows are not extracted as data", {
  # KNOWN ISSUE: Table 5.3 "Performance Metric" header rows are parsed as data
  # rows with garbage values like "cohort" and "cohort cohort 385".
  #
  # These appear to be column header text being captured as metric values.
  # Example from MDUFA III 2018-12-10, Table 5.3 CDRH:
  #   performance_metric: "Performance Metric"
  #   fy: 2013, 2014, 2015, 2016
  #   value: "cohort", "cohort", "cohort", "cohort cohort 385"
  #
  # Introduced by str_squish() fix in extract_report.R (2026-01)

  skip("Known issue: Table 5.3 header rows extracted as garbage data")

  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  data <- suppressMessages(suppressWarnings(
    extract_report(pdf_path, mdufa_period = "MDUFA III")
  ))

  # Test: No rows should have "cohort" as a value in Table 5.3
  garbage_rows <- data |>
    dplyr::filter(
      table_number == "5.3",
      performance_metric == "Performance Metric",
      grepl("cohort", value, ignore.case = TRUE)
    )

  expect_equal(
    nrow(garbage_rows), 0,
    label = "Rows with garbage 'cohort' values in Table 5.3"
  )
})

test_that("All MDUFA III reports can be verified", {
  # Several MDUFA III quarterly reports are no longer available from FDA:
  # - mdufa-3_2017-03-06_quarterly-report.pdf
  # - mdufa-3_2016-09-07_quarterly-report.pdf
  # - mdufa-3_2016-05-02_quarterly-report.pdf
  # - mdufa-3_2016-02-18_quarterly-report.pdf
  # - mdufa-3_2015-11-09_quarterly-report.pdf
  # - mdufa-3_2015-07-30_quarterly-report.pdf
  # - mdufa-3_2015-02-02_quarterly-report.pdf
  #
  # UPDATE: These files are now available after renaming mdufa-2i to mdufa-3
  # Skip on CI to avoid downloading 7 PDFs just to verify they exist
  skip_on_ci()

  previously_missing_reports <- c(
    "2017-03-06",
    "2016-09-07",
    "2016-05-02",
    "2016-02-18",
    "2015-11-09",
    "2015-07-30",
    "2015-02-02"
  )

  # Check each previously missing report (now available via S3 or locally)
  for (date in previously_missing_reports) {
    pattern <- paste0("mdufa-3_", date)
    pdf_path <- find_local_pdf(pattern)

    expect_false(
      is.null(pdf_path),
      label = paste("MDUFA III", date, "PDF exists")
    )
  }
})
