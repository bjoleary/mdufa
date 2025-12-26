# End-to-End Regression Tests for MDUFA Data Extraction
#
# These tests verify that the PDF extraction pipeline produces values that
# match manually verified data from: https://www.boleary.com/blog/mdufa/202401/
#
# The verified data CSVs in fixtures/ were manually checked against the
# original FDA PDF reports and serve as the ground truth.
#
# Test approach:
# 1. Extract data from local PDF reports using extract_report()
# 2. Compare extracted values against verified CSV fixtures
# 3. Any mismatch indicates a regression in the extraction pipeline

# =============================================================================
# LOAD FIXTURES INTO MEMORY ONCE
# =============================================================================

# Cache for fixtures - loaded once at file load time
fixture_cache <- new.env(parent = emptyenv())

# Load all fixture files at startup
local({
  fixture_dir <- testthat::test_path("fixtures")
  if (dir.exists(fixture_dir)) {
    fixture_files <- c(
      "fdadays-oht.csv",
      "industrydays-oht.csv",
      "totaldays-oht.csv",
      "volume-oht.csv",
      "cycles-oht.csv"
    )
    for (f in fixture_files) {
      fpath <- file.path(fixture_dir, f)
      if (file.exists(fpath)) {
        fixture_cache[[f]] <- utils::read.csv(fpath, stringsAsFactors = FALSE)
      }
    }
  }
})

# Helper to get cached fixture
load_fixture <- function(filename) {
  if (exists(filename, envir = fixture_cache)) {
    return(fixture_cache[[filename]])
  }
  fixture_path <- testthat::test_path("fixtures", filename)
  if (!file.exists(fixture_path)) {
    testthat::skip(paste("Fixture not found:", filename))
  }
  utils::read.csv(fixture_path, stringsAsFactors = FALSE)
}

# =============================================================================
# CACHE FOR EXTRACTED PDF DATA
# =============================================================================

# Cache for extracted PDF data - avoid re-extracting the same PDF
extraction_cache <- new.env(parent = emptyenv())

# Helper to extract from PDF with caching and warning suppression
extract_with_cache <- function(pdf_path, mdufa_period) {
  cache_key <- paste0(pdf_path, "_", mdufa_period)
  if (exists(cache_key, envir = extraction_cache)) {
    return(extraction_cache[[cache_key]])
  }
  # Suppress readr/vroom warnings during extraction
  result <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = mdufa_period
    )
  ))
  extraction_cache[[cache_key]] <- result
  result
}

# =============================================================================
# MDUFA III END-TO-END EXTRACTION TESTS
# Source PDF: December 10, 2018 MDUFA III Performance Report
# Ground truth: Blog CSV fixtures
# =============================================================================

test_that("MDUFA III extraction: FDA days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  # Load ground truth from fixture
  verified <- load_fixture("fdadays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 3",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  # Extract from PDF (cached)
  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  # Filter to FDA days Average values
  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average FDA days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  # Verify extraction produces data for all expected organizations
  exp_orgs <- unique(expected$organization)
  act_orgs <- unique(actual$organization)
  common_orgs <- intersect(exp_orgs, act_orgs)
  expect_equal(
    sort(common_orgs),
    sort(exp_orgs),
    label = "Extraction must produce Avg FDA days for all orgs"
  )

  # Compare each organization/FY combination
  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA III", org, "FY", exp_fy, "FDA days")
      )
    }
  }
})

test_that("MDUFA III extraction: Industry days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  verified <- load_fixture("industrydays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 3",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average Industry days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA III", org, "FY", exp_fy, "Industry days")
      )
    }
  }
})

test_that("MDUFA III extraction: Total days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  verified <- load_fixture("totaldays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 3",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average Total days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA III", org, "FY", exp_fy, "Total days")
      )
    }
  }
})

test_that("MDUFA III extraction: Volume matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  verified <- load_fixture("volume-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 3",
      performance_metric == "Number Received"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.integer(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.1",
      performance_metric == "Number Received"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.integer(gsub(",", "", value)))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        label = paste("MDUFA III", org, "FY", exp_fy, "Number Received")
      )
    }
  }
})

test_that("MDUFA III extraction: Review cycles matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  verified <- load_fixture("cycles-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 3",
      grepl("Average", performance_metric, ignore.case = TRUE),
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average review cycles", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA III", org, "FY", exp_fy, "Review cycles")
      )
    }
  }
})

# =============================================================================
# MDUFA IV END-TO-END EXTRACTION TESTS
# Source PDF: November 16, 2023 MDUFA IV Performance Report
# Ground truth: Blog CSV fixtures
# =============================================================================

test_that("MDUFA IV extraction: FDA days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("fdadays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 4",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average.*FDA.*days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA IV", org, "FY", exp_fy, "FDA days")
      )
    }
  }
})

test_that("MDUFA IV extraction: Industry days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("industrydays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 4",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average.*Industry.*days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA IV", org, "FY", exp_fy, "Industry days")
      )
    }
  }
})

test_that("MDUFA IV extraction: Total days matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("totaldays-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 4",
      performance_metric == "Average",
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average.*Total.*days", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA IV", org, "FY", exp_fy, "Total days")
      )
    }
  }
})

test_that("MDUFA IV extraction: Volume matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("volume-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 4",
      performance_metric == "Number Received"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.integer(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.1",
      performance_metric == "Number Received"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.integer(gsub(",", "", value)))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        label = paste("MDUFA IV", org, "FY", exp_fy, "Number Received")
      )
    }
  }
})

test_that("MDUFA IV extraction: Review cycles matches verified CSV", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("cycles-oht.csv")
  expected <- verified |>
    dplyr::filter(
      report_mdufa_period == "MDUFA 4",
      grepl("Average", performance_metric, ignore.case = TRUE),
      metric_type == "double"
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(fy = as.character(fy), value = as.numeric(value))

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual <- extracted |>
    dplyr::filter(
      table_number == "6.5",
      grepl("Average Review Cycles", performance_metric, ignore.case = TRUE)
    ) |>
    dplyr::select(organization, fy, value) |>
    dplyr::mutate(value = as.numeric(value))

  for (i in seq_len(nrow(expected))) {
    org <- expected$organization[i]
    exp_fy <- expected$fy[i]
    exp_val <- expected$value[i]

    actual_val <- actual |>
      dplyr::filter(organization == org, fy == exp_fy) |>
      dplyr::pull(value)

    if (length(actual_val) > 0) {
      expect_equal(
        actual_val[1], exp_val,
        tolerance = 0.01,
        label = paste("MDUFA IV", org, "FY", exp_fy, "Review cycles")
      )
    }
  }
})

# =============================================================================
# MDUFA V END-TO-END EXTRACTION TESTS
# Source PDF: November 16, 2023 MDUFA V Performance Report
# =============================================================================

test_that("MDUFA V extraction: produces valid data", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  extracted <- extract_with_cache(pdf_path, "MDUFA V")

  # Basic structure checks

  expect_true(nrow(extracted) > 0, label = "MDUFA V extraction produces rows")
  expect_true("organization" %in% names(extracted))
  expect_true("table_number" %in% names(extracted))
  expect_true("fy" %in% names(extracted))
  expect_true("value" %in% names(extracted))
})

# =============================================================================
# EXTRACTION FUNCTION UNIT TESTS
# =============================================================================

test_that("extract_report returns tibble with expected columns", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  result <- extract_with_cache(pdf_path, "MDUFA IV")

  expect_s3_class(result, "tbl_df")
  expect_true("source" %in% names(result))
  expect_true("page" %in% names(result))
  expect_true("table_number" %in% names(result))
  expect_true("organization" %in% names(result))
  expect_true("performance_metric" %in% names(result))
  expect_true("fy" %in% names(result))
  expect_true("value" %in% names(result))
})

test_that("extract_report adds report metadata when provided", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  result <- suppressMessages(suppressWarnings(
    mdufa::extract_report(
      pdf_path = pdf_path,
      mdufa_period = "MDUFA IV",
      report_date = as.Date("2023-11-16"),
      report_description = "Test report"
    )
  ))

  expect_true("report_date" %in% names(result))
  expect_true("report_description" %in% names(result))
  expect_equal(unique(result$report_date), as.Date("2023-11-16"))
  expect_equal(unique(result$report_description), "Test report")
})

# =============================================================================
# COVERAGE VERIFICATION TESTS
# Ensure all organizations in fixtures are extracted
# =============================================================================

test_that("MDUFA III extraction: all fixture organizations are found", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  verified <- load_fixture("fdadays-oht.csv")
  expected_orgs <- verified |>
    dplyr::filter(report_mdufa_period == "MDUFA 3") |>
    dplyr::pull(organization) |>
    unique()

  extracted <- extract_with_cache(pdf_path, "MDUFA III")

  actual_orgs <- unique(extracted$organization)

  for (org in expected_orgs) {
    expect_true(
      org %in% actual_orgs,
      label = paste("MDUFA III extraction contains organization", org)
    )
  }
})

test_that("MDUFA IV extraction: all fixture organizations are found", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  verified <- load_fixture("fdadays-oht.csv")
  expected_orgs <- verified |>
    dplyr::filter(report_mdufa_period == "MDUFA 4") |>
    dplyr::pull(organization) |>
    unique()

  extracted <- extract_with_cache(pdf_path, "MDUFA IV")

  actual_orgs <- unique(extracted$organization)

  for (org in expected_orgs) {
    expect_true(
      org %in% actual_orgs,
      label = paste("MDUFA IV extraction contains organization", org)
    )
  }
})

# Note: Comprehensive fixture diff tests are in test-regression-quick.R
# Run quick tests with: devtools::test(filter = "quick")
# Run full detailed tests with: devtools::test(filter = "regression$")
