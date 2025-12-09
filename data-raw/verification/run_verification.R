# run_verification.R
# Entry point for MDUFA extraction verification system
# NOT part of the installed package - development use only
#
# Usage:
#   source("data-raw/verification/run_verification.R")
#   verify_report("data-raw/pdf_reports/mdufa-4_2023-11-16.pdf", "MDUFA IV")

# Get the directory of this script
verification_dir <- dirname(sys.frame(1)$ofile)
if (is.null(verification_dir) || verification_dir == "") {
  verification_dir <- "data-raw/verification"
}

# Source all verification modules
message("Loading verification modules...")
source(file.path(verification_dir, "verification_diff.R"))
source(file.path(verification_dir, "verification_sample.R"))
source(file.path(verification_dir, "verification_helpers.R"))
source(file.path(verification_dir, "verification_app.R"))

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(pdftools)
  library(magick)
  library(stringr)
})

# Load the mdufa package
devtools::load_all()

message("Verification system loaded!")
message("")
message("Quick start:")
message("  verify_report('path/to/pdf', 'MDUFA IV')")
message("")
message("From CSV (regression test):")
message("  verify_from_csv('tests/testthat/fixtures/verified_data.csv')")
message("")
message("Or step-by-step:")
message("  diff_result <- generate_diff_for_report('path/to/pdf', 'MDUFA IV')")
message("  sample <- generate_verification_sample(diff_result)")
message("  launch_verification_app(sample, 'path/to/pdf', 'MDUFA IV')")

#' Generate Diff for Report
#'
#' High-level function to generate diff and open in KSDiff.
#'
#' @param pdf_path Path to PDF
#' @param mdufa_period MDUFA period string
#' @param baseline Optional baseline data (defaults to bundled)
#' @return Diff result list
generate_diff_for_report <- function(pdf_path, mdufa_period, baseline = NULL) {
  # Extract fresh data
  message("Extracting data from PDF...")
  extracted <- suppressMessages(suppressWarnings(
    extract_report(pdf_path, mdufa_period = mdufa_period)
  ))

  # Get baseline if not provided
  if (is.null(baseline)) {
    baseline <- switch(mdufa_period,
      "MDUFA III" = mdufa::mdufa3,
      "MDUFA IV" = mdufa::mdufa4,
      "MDUFA V" = mdufa::mdufa5
    )
  }

  # Generate diff
  diff_result <- generate_diff(baseline, extracted, scenario = "extraction")
  message(diff_result$summary)

  # Open in KSDiff for review
  message("\nOpening diff in KSDiff...")
  message("Review removed rows manually before proceeding.")
  open_diff(diff_result)

  diff_result
}

#' Verify Report (Complete Workflow)
#'
#' Runs the complete verification workflow for a report:
#' 1. Generate diff and open in KSDiff
#' 2. Wait for user confirmation
#' 3. Generate sample
#' 4. Launch verification app
#' 5. Generate test file if verification passes
#'
#' @param pdf_path Path to PDF
#' @param mdufa_period MDUFA period string
#' @param n Sample size (default 35 for 90% LB)
#' @param seed Random seed for reproducibility
#' @return Path to results CSV
verify_report <- function(pdf_path, mdufa_period, n = 35, seed = NULL) {
  # Step 1: Generate diff
  message("\n=== Step 1: Generating diff ===")
  diff_result <- generate_diff_for_report(pdf_path, mdufa_period)

  # Wait for user to review KSDiff
  message("\n=== Step 2: Review removed rows ===")
  message("Review removed rows in KSDiff.")
  message("Press Enter when ready to continue...")
  readline()

  # Step 3: Generate sample
  message("\n=== Step 3: Generating verification sample ===")
  sample <- generate_verification_sample(diff_result, n = n, seed = seed)

  # Step 4: Launch verification app
  message("\n=== Step 4: Launching verification app ===")
  message("Complete verification in the Shiny app.")
  message("Press Q or 'Finish & Save' when done.")

  results_path <- launch_verification_app(
    sample = sample,
    pdf_path = pdf_path,
    mdufa_period = mdufa_period,
    full_data = diff_result$comparison
  )

  # Step 5: Check results and generate test file
  message("\n=== Step 5: Processing results ===")
  results <- read.csv(results_path, stringsAsFactors = FALSE)
  passed <- results |> filter(status == "pass")
  failed <- results |> filter(status == "fail")

  status <- check_verification_status(nrow(passed), nrow(failed))
  message(status$message)

  if (status$status == "FAILED") {
    message("\nReview failures in: ", results_path)
    message("Fix extraction code and re-run verification.")
  } else if (status$status == "COMPLETE") {
    # Extract date from pdf_path
    report_date <- stringr::str_extract(basename(pdf_path), "\\d{4}-\\d{2}-\\d{2}")
    mdufa_num <- switch(mdufa_period,
      "MDUFA III" = "3",
      "MDUFA IV" = "4",
      "MDUFA V" = "5"
    )

    test_file <- paste0(
      "tests/testthat/test-verified-mdufa",
      mdufa_num, "-", report_date, ".R"
    )

    message("\nGenerating test file: ", test_file)
    generate_test_file(
      results = passed,
      pdf_path = pdf_path,
      mdufa_period = mdufa_period,
      output_file = test_file
    )

    message("\nVerification complete!")
    message("Next steps:")
    message("  1. Review test file: ", test_file)
    message("  2. Run tests: devtools::test(filter = 'verified')")
    message("  3. Commit: git add ", test_file)
  } else {
    message("\nNeed ", status$samples_needed, " more samples for 90% LB.")
    message("Resume with:")
    message("  launch_verification_app(")
    message("    resume_from = '", results_path, "',")
    message("    pdf_path = '", pdf_path, "',")
    message("    mdufa_period = '", mdufa_period, "'")
    message("  )")
  }

  results_path
}

#' Quick Verification Check
#'
#' Quickly check a few rows without running full verification.
#'
#' @param pdf_path Path to PDF
#' @param mdufa_period MDUFA period string
#' @param n Number of rows to check (default 5)
#' @param seed Random seed
quick_check <- function(pdf_path, mdufa_period, n = 5, seed = NULL) {
  message("Quick check: ", n, " random rows")

  # Extract data
  extracted <- suppressMessages(suppressWarnings(
    extract_report(pdf_path, mdufa_period = mdufa_period)
  ))

  # Sample rows
  if (!is.null(seed)) set.seed(seed)
  sample <- extracted |>
    filter(!is.na(page), !is.na(value)) |>
    slice_sample(n = n)

  # Verify each row
  for (i in seq_len(nrow(sample))) {
    row <- sample[i, ]
    message("\n--- Row ", i, " of ", n, " ---")
    message("Table: ", row$table_number, " | Org: ", row$organization)
    message("Metric: ", row$performance_metric)
    message("FY: ", row$fy, " | Value: ", row$value)

    verify_row_v2(row, pdf_path = pdf_path)

    response <- readline("Correct? (y/n/q to quit): ")
    if (tolower(response) == "q") break
    if (tolower(response) == "n") {
      message("Note: Row failed verification")
    }
  }

  message("\nQuick check complete.")
}

#' Verify From CSV
#'
#' Load a CSV file with verified data and launch verification app for each
#' report. Supports two CSV formats:
#'
#' Format 1 (simple): Columns: dataset, report_date, table_number, organization,
#' expected_metric (or performance_metric), fy, value. Page numbers are looked
#' up from bundled datasets.
#'
#' Format 2 (full): Columns: report_date, report_mdufa_period, page,
#' table_number, organization, performance_metric, fy, value. Already has page
#' numbers.
#'
#' PDFs are auto-discovered from data-raw/pdf_reports/.
#'
#' @param csv_path Path to CSV file with verified data
#' @param pdf_dir Directory containing PDF reports (default: data-raw/pdf_reports)
#' @return Invisible NULL (launches interactive app for each report)
verify_from_csv <- function(csv_path,
                            pdf_dir = "data-raw/pdf_reports") {
  message("Loading CSV: ", csv_path)
  csv_data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  message("Loaded ", nrow(csv_data), " rows")

  # Detect CSV format
  has_dataset <- "dataset" %in% names(csv_data)
  has_mdufa_period <- "report_mdufa_period" %in% names(csv_data)
  has_page <- "page" %in% names(csv_data)

  if (!has_dataset && has_mdufa_period && has_page) {
    # Format 2: Full format with page numbers already included
    message("Detected full format CSV (has page numbers)")
    verify_from_csv_full(csv_data, pdf_dir)
  } else if (has_dataset) {
    # Format 1: Simple format requiring page lookup
    message("Detected simple format CSV (needs page lookup)")
    verify_from_csv_simple(csv_data, pdf_dir)
  } else {
    stop(
      "CSV format not recognized. Need either 'dataset' column or ",
      "'report_mdufa_period' + 'page' columns."
    )
  }

  invisible(NULL)
}

#' Verify From CSV - Full Format
#'
#' Internal function for CSVs that already have page numbers.
#'
#' @param csv_data Data frame loaded from CSV
#' @param pdf_dir Directory containing PDF reports
#' @keywords internal
verify_from_csv_full <- function(csv_data, pdf_dir) {
  # Check required columns
  required_cols <- c(
    "report_date", "report_mdufa_period", "page",
    "table_number", "organization", "performance_metric"
  )

  missing_cols <- setdiff(required_cols, names(csv_data))
  if (length(missing_cols) > 0) {
    stop("CSV missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure consistent types
  csv_data$table_number <- as.character(csv_data$table_number)
  csv_data$fy <- as.character(csv_data$fy)
  csv_data$page <- as.integer(csv_data$page)

  # Derive dataset from report_mdufa_period (e.g., "MDUFA 3" -> "mdufa3")
  csv_data$dataset <- gsub("MDUFA ", "mdufa", csv_data$report_mdufa_period)

  # Find unique reports in the CSV
  reports <- csv_data |>
    dplyr::distinct(dataset, report_date, report_mdufa_period) |>
    dplyr::arrange(dataset, report_date)

  message("Found ", nrow(reports), " unique reports to verify:")
  for (i in seq_len(nrow(reports))) {
    message("  ", reports$dataset[i], " (", reports$report_date[i], ")")
  }

  # Process each report
  for (i in seq_len(nrow(reports))) {
    dataset <- reports$dataset[i]
    report_date <- reports$report_date[i]
    mdufa_period_raw <- reports$report_mdufa_period[i]

    message("\n=== Verifying ", dataset, " (", report_date, ") ===")

    # Get rows for this report
    report_rows <- csv_data |>
      dplyr::filter(
        .data$dataset == !!dataset,
        .data$report_date == !!report_date
      )

    if (nrow(report_rows) == 0) {
      message("No rows to verify for this report, skipping...")
      next
    }

    # Find PDF
    mdufa_num <- gsub("mdufa", "", dataset)
    pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)
    pdf_files <- list.files(pdf_dir, pattern = pdf_pattern, full.names = TRUE)

    if (length(pdf_files) == 0) {
      warning("PDF not found for ", dataset, " ", report_date, ", skipping...")
      next
    }
    pdf_path <- pdf_files[1]

    # Convert MDUFA period format (e.g., "MDUFA 3" -> "MDUFA III")
    mdufa_period <- switch(dataset,
      "mdufa3" = "MDUFA III",
      "mdufa4" = "MDUFA IV",
      "mdufa5" = "MDUFA V",
      mdufa_period_raw # fallback to raw
    )

    message("PDF: ", basename(pdf_path))

    # Collapse to unique metrics (one row per table/org/metric/page)
    full_data <- report_rows
    sample_collapsed <- report_rows |>
      dplyr::distinct(table_number, organization, performance_metric, page)

    message("Unique metrics to verify: ", nrow(sample_collapsed))

    # Launch verification app
    launch_verification_app(
      sample = sample_collapsed,
      pdf_path = pdf_path,
      mdufa_period = mdufa_period,
      output_path = tempfile(
        pattern = paste0(dataset, "_", report_date, "_"),
        fileext = ".csv"
      ),
      full_data = full_data
    )
  }
}

#' Verify From CSV - Simple Format
#'
#' Internal function for CSVs that need page number lookup.
#'
#' @param csv_data Data frame loaded from CSV
#' @param pdf_dir Directory containing PDF reports
#' @keywords internal
verify_from_csv_simple <- function(csv_data, pdf_dir) {
  # Check required columns
  required_cols <- c("dataset", "report_date", "table_number", "organization")
  missing_cols <- setdiff(required_cols, names(csv_data))
  if (length(missing_cols) > 0) {
    stop("CSV missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Map column names if needed
  if ("expected_metric" %in% names(csv_data) &&
    !"performance_metric" %in% names(csv_data)) {
    csv_data$performance_metric <- csv_data$expected_metric
  }

  # Ensure consistent types
  csv_data$table_number <- as.character(csv_data$table_number)
  csv_data$fy <- as.character(csv_data$fy)

  # Find unique reports in the CSV
  reports <- csv_data |>
    dplyr::distinct(dataset, report_date) |>
    dplyr::arrange(dataset, report_date)

  message("Found ", nrow(reports), " unique reports to verify:")
  for (i in seq_len(nrow(reports))) {
    message("  ", reports$dataset[i], " (", reports$report_date[i], ")")
  }

  # Process each report
  for (i in seq_len(nrow(reports))) {
    dataset <- reports$dataset[i]
    report_date <- reports$report_date[i]

    message("\n=== Verifying ", dataset, " (", report_date, ") ===")

    # Get rows for this report
    report_rows <- csv_data |>
      dplyr::filter(
        .data$dataset == !!dataset,
        .data$report_date == !!report_date
      )

    # Get bundled data for page lookup
    bundled <- switch(dataset,
      "mdufa3" = mdufa::mdufa3,
      "mdufa4" = mdufa::mdufa4,
      "mdufa5" = mdufa::mdufa5,
      stop("Unknown dataset: ", dataset)
    )
    bundled$table_number <- as.character(bundled$table_number)
    bundled$fy <- as.character(bundled$fy)

    # Normalize metric names for fuzzy matching (remove MDUFA period)
    normalize_metric <- function(x) {
      gsub(" (III|IV|V) ", " ", x)
    }

    # Look up page numbers from bundled data using normalized metrics
    page_lookup <- bundled |>
      dplyr::mutate(
        metric_normalized = normalize_metric(.data$performance_metric)
      ) |>
      dplyr::distinct(
        table_number, organization, metric_normalized,
        performance_metric, page
      )

    # Add normalized metric to report_rows for joining
    report_rows$metric_normalized <- normalize_metric(
      report_rows$performance_metric
    )

    sample <- report_rows |>
      dplyr::left_join(
        page_lookup,
        by = c("table_number", "organization", "metric_normalized"),
        suffix = c("", "_bundled")
      ) |>
      dplyr::mutate(
        performance_metric = dplyr::coalesce(
          .data$performance_metric_bundled,
          .data$performance_metric
        )
      ) |>
      dplyr::select(-"metric_normalized", -"performance_metric_bundled")

    # For rows with missing pages, fall back to table_number-only lookup
    missing_pages <- sum(is.na(sample$page))
    if (missing_pages > 0) {
      message(missing_pages, " rows could not match metric - using table page")

      table_pages <- bundled |>
        dplyr::group_by(table_number, organization) |>
        dplyr::summarise(
          fallback_page = min(page, na.rm = TRUE),
          .groups = "drop"
        )

      sample <- sample |>
        dplyr::left_join(table_pages, by = c("table_number", "organization")) |>
        dplyr::mutate(
          page = dplyr::coalesce(.data$page, .data$fallback_page)
        ) |>
        dplyr::select(-"fallback_page")
    }

    # Final check for still-missing pages
    still_missing <- sum(is.na(sample$page))
    if (still_missing > 0) {
      warning(still_missing, " rows still have no page after fallback")
      sample <- sample |> dplyr::filter(!is.na(.data$page))
    }

    if (nrow(sample) == 0) {
      message("No rows to verify for this report, skipping...")
      next
    }

    # Find PDF
    mdufa_num <- gsub("mdufa", "", dataset)
    pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)
    pdf_files <- list.files(pdf_dir, pattern = pdf_pattern, full.names = TRUE)

    if (length(pdf_files) == 0) {
      warning("PDF not found for ", dataset, " ", report_date, ", skipping...")
      next
    }
    pdf_path <- pdf_files[1]

    # Determine MDUFA period
    mdufa_period <- switch(dataset,
      "mdufa3" = "MDUFA III",
      "mdufa4" = "MDUFA IV",
      "mdufa5" = "MDUFA V"
    )

    message("PDF: ", basename(pdf_path))

    # Collapse to unique metrics (one row per table/org/metric)
    full_data <- sample
    sample_collapsed <- sample |>
      dplyr::distinct(table_number, organization, performance_metric, page)

    message("Unique metrics to verify: ", nrow(sample_collapsed))

    # Launch verification app
    launch_verification_app(
      sample = sample_collapsed,
      pdf_path = pdf_path,
      mdufa_period = mdufa_period,
      output_path = tempfile(
        pattern = paste0(dataset, "_", report_date, "_"),
        fileext = ".csv"
      ),
      full_data = full_data
    )
  }
}
