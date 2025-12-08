#' Make a Tibble with Correct Names
#'
#' @param data A tibble
#' @param skip_empty_row_fix Logical. If TRUE, skips the fix_empty_rows step.
#'   Use when metric names have already been fixed by fix_wrapped_metric_names()
#'   and we want to preserve rows with all-NA data values (e.g., legitimate
#'   N/A values in MDUFA V table 1.12).
#'
#' @return A tibble with names that are harmonized with other tables from the
#'   report.
#' @export
#'
make_clean_tibble <- function(data, skip_empty_row_fix = FALSE) {
  name_replacer <- function(x) {
    stringr::str_replace(
      string = x,
      pattern = stringr::regex("performance_metrics|pma_submissions_received"),
      replacement = "performance_metric"
    )
  }
  result <- data |>
    janitor::clean_names() |>
    dplyr::rename_with(name_replacer)

  if (!skip_empty_row_fix) {
    result <- fix_empty_rows(result, name_column = colnames(result)[[1]])
  }

  result
}
