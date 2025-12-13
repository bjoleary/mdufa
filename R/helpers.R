#' Find Metrics by Pattern
#'
#' Find unique performance metric names that match a regular expression pattern.
#' Useful for exploring available metrics in a MDUFA dataset.
#'
#' @param data A MDUFA dataset with a performance_metric column
#' @param pattern A regular expression pattern to search for (case-insensitive)
#'
#' @return Character vector of unique performance metrics that match the pattern
#' @export
#'
#' @examples
#' # Find all metrics related to FDA days
#' find_metrics(mdufa4, "FDA days")
#'
#' # Find all percentage/rate metrics
#' find_metrics(mdufa4, "^(Rate|Percent)")
find_metrics <- function(data, pattern) {
  metrics <- unique(data$performance_metric)
  matches <- stringr::str_subset(
    metrics,
    stringr::regex(pattern, ignore_case = TRUE)
  )

  matches
}

#' Convert Integer Metrics
#'
#' Filters a MDUFA dataset to integer metrics and converts the value column
#' to integer type. Also converts fy to integer.
#'
#' @param data A MDUFA dataset, typically filtered to metric_type == "integer"
#'
#' @return Dataset with value as integer and fy as integer
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa4 |>
#'   filter(metric_type == "integer") |>
#'   convert_integers() |>
#'   head()
convert_integers <- function(data) {
  data |>
    dplyr::mutate(
      value = stringr::str_remove_all(.data$value, "[^0-9.-]") |>
        as.integer(),
      fy = as.integer(.data$fy)
    )
}

#' Convert Double Metrics
#'
#' Filters a MDUFA dataset to double metrics and converts the value column
#' to double/numeric type. Also converts fy to integer.
#'
#' @param data A MDUFA dataset, typically filtered to metric_type == "double"
#'
#' @return Dataset with value as double and fy as integer
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa4 |>
#'   filter(metric_type == "double") |>
#'   convert_doubles() |>
#'   head()
convert_doubles <- function(data) {
  data |>
    dplyr::mutate(
      value = stringr::str_remove_all(.data$value, "[^0-9.-]") |>
        as.double(),
      fy = as.integer(.data$fy)
    )
}

#' Convert Percent Metrics
#'
#' Filters a MDUFA dataset to percent metrics and converts the value column
#' to a proportion (0-1 scale). Also converts fy to integer.
#'
#' @param data A MDUFA dataset, typically filtered to metric_type == "percent"
#'
#' @return Dataset with value as proportion (0-1) and fy as integer
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa4 |>
#'   filter(metric_type == "percent") |>
#'   convert_percents() |>
#'   head()
convert_percents <- function(data) {
  data |>
    dplyr::mutate(
      value = stringr::str_remove_all(.data$value, "%") |>
        as.double() / 100,
      fy = as.integer(.data$fy)
    )
}

#' Get Cohort Status
#'
#' Determines whether a fiscal year cohort is "open" (submissions still pending)
#' or "closed" (all submissions have received decisions). A cohort is considered
#' closed when the "Number Pending" metric equals 0.
#'
#' @param data A MDUFA dataset with performance_metric, fy, and value columns
#' @param pending_pattern Regex pattern to identify the "pending" metric.
#'   Defaults to "Pending.*Decision$" which matches MDUFA III/IV/V formats.
#'
#' @return Tibble with columns: fy (character), cohort_status ("open" or
#'   "closed"), n_pending (integer count of pending submissions)
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa4 |>
#'   filter(
#'     organization == "OHT7",
#'     program == "510(k)"
#'   ) |>
#'   get_cohort_status()
get_cohort_status <- function(data, pending_pattern = "Pending.*Decision$") {
  # Find the pending metric
  pending_metrics <- data |>
    dplyr::filter(
      grepl(pending_pattern, .data$performance_metric, ignore.case = TRUE)
    )

  if (nrow(pending_metrics) == 0) {
    return(tibble::tibble(
      fy = character(),
      cohort_status = character(),
      n_pending = integer()
    ))
  }

  pending_metrics |>
    dplyr::mutate(
      n_pending = suppressWarnings(as.integer(.data$value))
    ) |>
    dplyr::group_by(.data$fy) |>
    dplyr::summarise(
      n_pending = sum(.data$n_pending, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      cohort_status = dplyr::if_else(
        .data$n_pending == 0,
        "closed",
        "open"
      )
    ) |>
    dplyr::select("fy", "cohort_status", "n_pending")
}

#' MDUFA III OHT7 Cohort Status (Manual)
#'
#' Returns manually verified cohort status for MDUFA III OHT7 (IVD)
#' fiscal years. This data was verified against the final MDUFA III
#' quarterly report (December 10, 2018).
#'
#' Note: FY2017 cohort remained open in the final MDUFA III report because
#' submissions were still pending decision when the MDUFA III period ended.
#'
#' @return Tibble with fy, organization, and cohort_status columns
#' @export
#'
#' @examples
#' mdufa3_oht7_cohort_status()
mdufa3_oht7_cohort_status <- function() {
  tibble::tibble(
    fy = c("2013", "2014", "2015", "2016", "2017"),
    organization = "OHT7",
    cohort_status = c("closed", "closed", "closed", "closed", "open")
  )
}

#' Check if Row is Likely a Footnote
#'
#' Identifies rows that appear to be footnote text rather than actual
#' performance metrics. Footnotes are typically long explanatory text
#' that got parsed as metric names during PDF extraction.
#'
#' Common footnote patterns include:
#' - Starts with digit + space (e.g., "1 If FDA's...")
#' - Starts with "FYs", "In FY", "Subs in FYs"
#' - Contains "goal are met for FY" or "cohort excludes"
#'
#' @param performance_metric Character vector of metric names to check
#' @param value Character vector of values (footnotes typically have NA)
#'
#' @return Logical vector, TRUE if row appears to be footnote text
#' @keywords internal
is_footnote_row <- function(performance_metric, value) {
  is.na(value) & (
    # Starts with digit + space (footnote numbers like "1 If FDA's...")
    grepl("^[0-9]+\\s+", performance_metric) |
      # Starts with fiscal year scope explanations
      grepl("^(FYs|In FY|Subs in FYs)", performance_metric) |
      # Goal explanation text
      grepl("goal are met (for|in) FY", performance_metric) |
      # Cohort exclusion explanations
      grepl("cohort excludes", performance_metric)
  )
}

#' Remove Footnote Rows from MDUFA Data
#'
#' Filters out rows that appear to be footnote text rather than actual
#' performance metrics. These are typically long explanatory text strings
#' that were incorrectly parsed as metric names during PDF extraction.
#'
#' @param data A MDUFA dataset with performance_metric and value columns
#'
#' @return Dataset with footnote rows removed
#' @export
#'
#' @examples
#' # Remove footnotes from MDUFA data
#' clean_data <- remove_footnote_rows(mdufa5)
#' nrow(mdufa5) - nrow(clean_data)  # Number of footnote rows removed
remove_footnote_rows <- function(data) {
  data |>
    dplyr::filter(!is_footnote_row(.data$performance_metric, .data$value))
}
