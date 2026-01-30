#' Validate Dataset Columns Match Expected Structure
#'
#' Checks that a dataset has exactly the expected columns in the expected order.
#' Useful for ensuring data pipeline outputs conform to package specifications.
#'
#' @param data A data frame or tibble to validate
#' @param expected_cols Character vector of expected column names in order
#' @param dataset_name Name of dataset for error messages (default: "dataset")
#'
#' @return TRUE invisibly if validation passes
#' @export
#'
#' @examples
#' # Validate mdufa4 has expected columns
#' validate_columns(mdufa4, mdufa_cols, "mdufa4")
#'
#' # Validate mdufa_combined
#' validate_columns(mdufa_combined, mdufa_combined_cols, "mdufa_combined")
validate_columns <- function(data, expected_cols, dataset_name = "dataset") {
  actual <- names(data)

  # Check for missing columns
  missing <- setdiff(expected_cols, actual)
  if (length(missing) > 0) {
    stop(
      dataset_name, " missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  # Check for extra columns
  extra <- setdiff(actual, expected_cols)
  if (length(extra) > 0) {
    stop(
      dataset_name, " has unexpected columns: ",
      paste(extra, collapse = ", "),
      call. = FALSE
    )
  }

  # Check column order
  if (!identical(actual, expected_cols)) {
    stop(
      dataset_name, " columns are out of order.\n",
      "Expected: ", paste(expected_cols, collapse = ", "), "\n",
      "Actual:   ", paste(actual, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Standardize Column Order for a MDUFA Dataset
#'
#' Reorders columns in a dataset to match the expected column order.
#' This is useful when extraction functions return columns in a different
#' order than the package standard.
#'
#' @param data A data frame or tibble with MDUFA data
#' @param cols Expected column vector (default: [mdufa_cols])
#'
#' @return Tibble with columns in standard order
#' @export
#'
#' @examples
#' # Reorder columns to standard order
#' standardized <- standardize_columns(mdufa4, mdufa_cols)
standardize_columns <- function(data, cols = mdufa_cols) {
  # Verify all expected columns exist
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      "Cannot standardize: missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  data[, cols]
}

#' Validate Unique Metrics in MDUFA Dataset
#'
#' Checks that a dataset has no duplicate rows for the key columns that should
#' uniquely identify each metric. This validation ensures data integrity and
#' prevents issues with downstream analysis like pivot_wider creating list cols.
#'
#' @param data A data frame or tibble with MDUFA data
#' @param dataset_name Name of dataset for error messages (default: "dataset")
#' @param key_cols Character vector of columns that should be unique together.
#'   Default is the standard MDUFA metric key columns.
#'
#' @return TRUE invisibly if validation passes
#' @export
#'
#' @examples
#' # Validate mdufa5 has unique metrics
#' validate_unique_metrics(mdufa5, "mdufa5")
validate_unique_metrics <- function(data,
                                    dataset_name = "dataset",
                                    key_cols = c(
                                      "table_number",
                                      "organization",
                                      "program",
                                      "performance_metric",
                                      "fy"
                                    )) {
  # Find duplicate key combinations
  dups <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(dups) > 0) {
    n_combos <- nrow(dups) / 2 # Each duplicate appears twice
    example <- dups[1, key_cols]
    stop(
      dataset_name, " has ", n_combos, " duplicate key combinations.\n",
      "First duplicate: ",
      paste(names(example), "=", example, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
