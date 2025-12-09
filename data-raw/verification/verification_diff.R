# verification_diff.R
# Diff generation functions for MDUFA extraction verification
# NOT part of the installed package - development use only

#' Open Files in KSDiff
#'
#' Opens two files in Kaleidoscope for comparison.
#'
#' @param left_file Path to left (baseline) file
#' @param right_file Path to right (new) file
#' @return Invisibly returns TRUE
ksdiff_open <- function(left_file, right_file) {
  cmd <- paste("ksdiff", shQuote(left_file), shQuote(right_file))
  system(cmd, wait = FALSE)
  invisible(TRUE)
}

#' Generate Verification Diff
#'
#' Creates a structured diff between two datasets with appropriate
#' column selection and sorting based on the verification scenario.
#'
#' @param baseline The baseline dataset (left side of diff)
#' @param comparison The comparison dataset (right side of diff)
#' @param scenario One of "extraction", "report", or "full"
#' @param output_dir Directory to write CSV files (default /tmp)
#' @param prefix Filename prefix for output CSVs
#'
#' @return A list with components:
#'   - new_rows: Rows in comparison but not baseline (tibble)
#'   - removed_rows: Rows in baseline but not comparison (tibble)
#'   - updated_rows: Rows with same keys but different metadata (tibble)
#'   - baseline_csv: Path to sorted baseline CSV
#'   - comparison_csv: Path to sorted comparison CSV
#'   - baseline: The baseline data (for downstream use)
#'   - comparison: The comparison data (for downstream use)
#'   - summary: Character string summarizing differences
generate_diff <- function(baseline,
                          comparison,
                          scenario = c("extraction", "report", "full"),
                          output_dir = "/tmp",
                          prefix = "mdufa_diff") {
  scenario <- match.arg(scenario)

  # Select columns based on scenario
  diff_cols <- switch(scenario,
    "extraction" = c(
      "table_number", "organization", "program",
      "table_title", "performance_metric", "fy", "value"
    ),
    "report" = c(
      "table_number", "organization", "performance_metric",
      "fy", "value", "metric_type"
    ),
    "full" = c(
      "source", "page", "table_number", "organization", "program",
      "table_title", "metric_type", "performance_metric", "fy", "value"
    )
  )

  # Key columns for matching (always the same)
  key_cols <- c("table_number", "performance_metric", "fy", "value")

  # Categorize differences
  new_rows <- dplyr::anti_join(comparison, baseline, by = key_cols)
  removed_rows <- dplyr::anti_join(baseline, comparison, by = key_cols)

  common_cols <- intersect(names(baseline), names(comparison))
  all_diff <- dplyr::anti_join(
    comparison[, common_cols],
    baseline[, common_cols]
  )
  updated_rows <- dplyr::semi_join(all_diff, baseline, by = key_cols)

  # Sort consistently
  sort_cols <- c("table_number", "page", "performance_metric", "fy")
  sort_cols <- intersect(sort_cols, diff_cols)

  baseline_sorted <- baseline |>
    dplyr::select(dplyr::any_of(diff_cols)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  comparison_sorted <- comparison |>
    dplyr::select(dplyr::any_of(diff_cols)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Write CSVs
  baseline_csv <- file.path(output_dir, paste0(prefix, "_baseline.csv"))
  comparison_csv <- file.path(output_dir, paste0(prefix, "_comparison.csv"))

  utils::write.csv(baseline_sorted, baseline_csv, row.names = FALSE)
  utils::write.csv(comparison_sorted, comparison_csv, row.names = FALSE)

  # Generate summary
  summary <- paste0(
    "Diff Summary (", scenario, " scenario):\n",
    "  NEW rows: ", nrow(new_rows), "\n",
    "  REMOVED rows: ", nrow(removed_rows), "\n",
    "  UPDATED rows: ", nrow(updated_rows), "\n",
    "  Baseline CSV: ", baseline_csv, "\n",
    "  Comparison CSV: ", comparison_csv
  )

  list(
    new_rows = new_rows,
    removed_rows = removed_rows,
    updated_rows = updated_rows,
    baseline_csv = baseline_csv,
    comparison_csv = comparison_csv,
    baseline = baseline,
    comparison = comparison,
    summary = summary
  )
}

#' Open Diff in KSDiff
#'
#' Opens the baseline and comparison CSVs from generate_diff() in Kaleidoscope.
#'
#' @param diff_result Result from generate_diff()
#' @return Invisibly returns TRUE
open_diff <- function(diff_result) {
  ksdiff_open(diff_result$baseline_csv, diff_result$comparison_csv)
}

#' Create Verification Diff
#'
#' Extracts data from a PDF and generates a diff against bundled data.
#'
#' @param pdf_path Path to the PDF to extract
#' @param mdufa_period MDUFA period string
#' @param bundled Optional bundled dataset; if NULL, uses package data
#' @param open_ksdiff If TRUE, opens diff in Kaleidoscope
#'
#' @return Result from generate_diff()
diff_for_verification <- function(pdf_path,
                                  mdufa_period,
                                  bundled = NULL,
                                  open_ksdiff = TRUE) {
  # Extract fresh data
  extracted <- extract_report(pdf_path, mdufa_period = mdufa_period)

  # Get bundled data if not provided
  if (is.null(bundled)) {
    bundled <- switch(mdufa_period,
      "MDUFA III" = mdufa::mdufa3,
      "MDUFA IV" = mdufa::mdufa4,
      "MDUFA V" = mdufa::mdufa5
    )
  }

  # Generate diff
  diff_result <- generate_diff(
    baseline = bundled,
    comparison = extracted,
    scenario = "extraction"
  )

  message(diff_result$summary)

  if (open_ksdiff) {
    open_diff(diff_result)
  }

  diff_result
}
