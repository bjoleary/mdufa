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
#' standardized <- standardize_columns(extracted_data, mdufa_cols)
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
