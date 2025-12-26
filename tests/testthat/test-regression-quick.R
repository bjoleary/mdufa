# Quick Regression Tests - Comprehensive Fixture Diffs
#
# These tests run fast and provide broad coverage by comparing extracted data
# against reference fixtures. Run these regularly during development.
#
# Usage:
#   devtools::test(filter = "quick")
#
# For detailed per-value tests, run the full regression suite:
#   devtools::test(filter = "regression$")

# =============================================================================
# Shared Utilities
# =============================================================================

# Cache for fixture data (loaded once per test run)
if (!exists("fixture_cache", envir = globalenv())) {
  assign("fixture_cache", new.env(parent = emptyenv()), envir = globalenv())
}

#' Load fixture with caching
load_fixture <- function(filename) {
  cache <- get("fixture_cache", envir = globalenv())
  if (!exists(filename, envir = cache)) {
    filepath <- testthat::test_path("fixtures", filename)
    data <- suppressMessages(suppressWarnings(
      readr
      ::read_csv(filepath, show_col_types = FALSE)
    ))
    assign(filename, data, envir = cache)
  }
  get(filename, envir = cache)
}

# Cache for PDF extractions
if (!exists("extraction_cache", envir = globalenv())) {
  assign("extraction_cache", new.env(parent = emptyenv()), envir = globalenv())
}

#' Extract with caching and warning suppression
extract_with_cache <- function(pdf_path, mdufa_period) {
  cache <- get("extraction_cache", envir = globalenv())
  cache_key <- paste0(basename(pdf_path), "_", mdufa_period)
  if (!exists(cache_key, envir = cache)) {
    data <- suppressMessages(suppressWarnings(
      mdufa::extract_report(pdf_path, mdufa_period)
    ))
    assign(cache_key, data, envir = cache)
  }
  get(cache_key, envir = cache)
}

#' Map fixture metric to extraction metric pattern
map_metric_pattern <- function(fixture_name, fixture_metric, mdufa_period) {
  period_suffix <- switch(mdufa_period,
    "MDUFA III" = "MDUFA III",
    "MDUFA IV" = "MDUFA IV",
    "MDUFA V" = "MDUFA V",
    mdufa_period
  )

  if (grepl("fdadays", fixture_name)) {
    if (fixture_metric == "Average") {
      return(paste0("Average FDA days to ", period_suffix))
    } else {
      return(paste0(fixture_metric, " FDA days to ", period_suffix))
    }
  } else if (grepl("industrydays", fixture_name)) {
    if (fixture_metric == "Average") {
      return(paste0("Average Industry days to ", period_suffix))
    } else {
      return(paste0(fixture_metric, " Industry days to ", period_suffix))
    }
  } else if (grepl("totaldays", fixture_name)) {
    if (fixture_metric == "Average") {
      return(paste0("Average Total days to ", period_suffix))
    } else {
      return(paste0(fixture_metric, " Total days to ", period_suffix))
    }
  } else if (grepl("cycles", fixture_name)) {
    return("Average review cycles")
  } else if (grepl("volume", fixture_name)) {
    return("Number Received")
  }

  fixture_metric
}

#' Run comprehensive diff test for a fixture
#' @param fixture_name Name of fixture file
#' @param mdufa_period MDUFA period for extraction (e.g., "MDUFA III")
#' @param fixture_period Period string in fixture (e.g., "MDUFA 3")
#' @param table_num Table number to filter
#' @param metric_filter Optional filter function for metrics
#' @return Number of mismatches found
run_fixture_diff <- function(fixture_name, mdufa_period, fixture_period,
                             table_num, metric_filter = NULL) {
  pdf_pattern <- switch(mdufa_period,
    "MDUFA III" = "mdufa-3_2018-12-10",
    "MDUFA IV" = "mdufa-4_2023-11-16",
    "MDUFA V" = "mdufa-5_2023-11-16",
    NULL
  )

  pdf_path <- find_local_pdf(pdf_pattern)
  if (is.null(pdf_path)) {
    return(NA)
  }

  fixture <- load_fixture(fixture_name)
  expected <- fixture |>
    dplyr::filter(report_mdufa_period == fixture_period)

  if (!is.null(metric_filter)) {
    expected <- metric_filter(expected)
  }

  select_cols <- c("organization", "performance_metric", "fy", "value")
  expected <- expected |>
    dplyr::select(dplyr::any_of(select_cols)) |>
    dplyr::mutate(
      fy = as.character(fy),
      value = suppressWarnings(as.numeric(value))
    )

  extracted <- extract_with_cache(pdf_path, mdufa_period)

  mismatches <- 0
  for (i in seq_len(nrow(expected))) {
    exp_metric <- if ("performance_metric" %in% names(expected)) {
      expected$performance_metric[i]
    } else {
      "Average"
    }
    extraction_pattern <- map_metric_pattern(
      fixture_name, exp_metric, mdufa_period
    )

    actual <- extracted |>
      dplyr::filter(
        table_number == table_num,
        grepl(extraction_pattern, performance_metric, fixed = TRUE) |
          grepl(extraction_pattern, performance_metric, ignore.case = TRUE),
        organization == expected$organization[i],
        fy == expected$fy[i]
      )

    if (nrow(actual) > 0) {
      actual_val <- suppressWarnings(as.numeric(actual$value[1]))
      exp_val <- expected$value[i]
      if (!is.na(exp_val) && !is.na(actual_val)) {
        if (abs(actual_val - exp_val) > 0.5) {
          mismatches <- mismatches + 1
        }
      }
    }
  }

  mismatches
}

# =============================================================================
# MDUFA III Quick Diff Tests
# =============================================================================

test_that("MDUFA III quick diff: fdadays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  mismatches <- run_fixture_diff(
    "fdadays-oht.csv", "MDUFA III", "MDUFA 3", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA III fdadays value mismatches")
})

test_that("MDUFA III quick diff: industrydays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  mismatches <- run_fixture_diff(
    "industrydays-oht.csv", "MDUFA III", "MDUFA 3", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA III industrydays value mismatches")
})

test_that("MDUFA III quick diff: totaldays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  mismatches <- run_fixture_diff(
    "totaldays-oht.csv", "MDUFA III", "MDUFA 3", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA III totaldays value mismatches")
})

test_that("MDUFA III quick diff: cycles", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  mismatches <- run_fixture_diff(
    "cycles-oht.csv", "MDUFA III", "MDUFA 3", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA III cycles value mismatches")
})

test_that("MDUFA III quick diff: volume", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  mismatches <- run_fixture_diff(
    "volume-oht.csv", "MDUFA III", "MDUFA 3", "6.1",
    function(df) dplyr::filter(df, performance_metric == "Number Received")
  )

  expect_equal(mismatches, 0, label = "MDUFA III volume value mismatches")
})

# =============================================================================
# MDUFA IV Quick Diff Tests
# =============================================================================

test_that("MDUFA IV quick diff: fdadays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  mismatches <- run_fixture_diff(
    "fdadays-oht.csv", "MDUFA IV", "MDUFA 4", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA IV fdadays value mismatches")
})

test_that("MDUFA IV quick diff: industrydays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  mismatches <- run_fixture_diff(
    "industrydays-oht.csv", "MDUFA IV", "MDUFA 4", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA IV industrydays value mismatches")
})

test_that("MDUFA IV quick diff: totaldays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  mismatches <- run_fixture_diff(
    "totaldays-oht.csv", "MDUFA IV", "MDUFA 4", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA IV totaldays value mismatches")
})

test_that("MDUFA IV quick diff: cycles", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  mismatches <- run_fixture_diff(
    "cycles-oht.csv", "MDUFA IV", "MDUFA 4", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA IV cycles value mismatches")
})

test_that("MDUFA IV quick diff: volume", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  mismatches <- run_fixture_diff(
    "volume-oht.csv", "MDUFA IV", "MDUFA 4", "6.1",
    function(df) dplyr::filter(df, performance_metric == "Number Received")
  )

  expect_equal(mismatches, 0, label = "MDUFA IV volume value mismatches")
})

# =============================================================================
# MDUFA V Quick Diff Tests
# =============================================================================

test_that("MDUFA V quick diff: fdadays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  mismatches <- run_fixture_diff(
    "fdadays-oht.csv", "MDUFA V", "MDUFA 5", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA V fdadays value mismatches")
})

test_that("MDUFA V quick diff: industrydays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  mismatches <- run_fixture_diff(
    "industrydays-oht.csv", "MDUFA V", "MDUFA 5", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA V industrydays value mismatches")
})

test_that("MDUFA V quick diff: totaldays", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  mismatches <- run_fixture_diff(
    "totaldays-oht.csv", "MDUFA V", "MDUFA 5", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA V totaldays value mismatches")
})

test_that("MDUFA V quick diff: cycles", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  mismatches <- run_fixture_diff(
    "cycles-oht.csv", "MDUFA V", "MDUFA 5", "6.5",
    function(df) dplyr::filter(df, metric_type == "double")
  )

  expect_equal(mismatches, 0, label = "MDUFA V cycles value mismatches")
})

test_that("MDUFA V quick diff: volume", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  mismatches <- run_fixture_diff(
    "volume-oht.csv", "MDUFA V", "MDUFA 5", "6.1",
    function(df) dplyr::filter(df, performance_metric == "Number Received")
  )

  expect_equal(mismatches, 0, label = "MDUFA V volume value mismatches")
})
