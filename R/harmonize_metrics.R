#' Harmonize Metric Names Across MDUFA Periods
#'
#' Standardizes performance_metric values across MDUFA periods to enable
#' aggregation and plotting over time. Handles differences such as:
#' - Asterisks and footnote markers (e.g., "Number Received*")
#' - Period-specific wording (e.g., "MDUFA III decision")
#' - Case variations (e.g., "FDA Days" vs "FDA days")
#' - Minor variations in spacing, punctuation
#'
#' @param data MDUFA dataset with a performance_metric column
#' @return Dataset with added `metric_harmonized` column containing
#'   the standardized metric name
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa3 |>
#'   harmonize_metric_names() |>
#'   select(performance_metric, metric_harmonized) |>
#'   distinct() |>
#'   head(10)
harmonize_metric_names <- function(data) {
  # Acronyms that should remain uppercase after title case conversion

  acronyms <- c(
    "FDA", "MDUFA", "PMA", "PMAs", "SI", "RTA", "RTF", "NSE", "FY",
    "CDRH", "OHT", "IVD", "DCD", "CLIA", "TS", "BLA", "NDA"
  )

  data |>
    dplyr::mutate(
      metric_harmonized = .data$performance_metric |>
        # Remove asterisks and footnote markers
        stringr::str_remove_all("\\*+") |>
        # Normalize MDUFA period references (MDUFA II, III, IV, V -> MDUFA)
        stringr::str_replace("MDUFA\\s+(II|III|IV|V)\\b", "MDUFA") |>
        # Normalize "Panel Track" vs "Panel-Track"
        stringr::str_replace("Panel Track", "Panel-Track") |>
        # Normalize spacing around dashes
        stringr::str_replace_all("\\s*-\\s*", " - ") |>
        stringr::str_replace_all("\\s+-\\s+", " - ") |>
        # Normalize case: convert to title case
        stringr::str_to_title() |>
        # Restore acronyms to uppercase
        (\(x) {
          result <- x
          for (acronym in acronyms) {
            # Match the title-cased version and replace with uppercase
            pattern <- paste0("\\b", stringr::str_to_title(acronym), "\\b")
            result <- stringr::str_replace_all(result, pattern, acronym)
          }
          result
        })() |>
        # Fix 510(k) - the (k) should be lowercase per FDA convention
        stringr::str_replace_all("510\\(K\\)S?", function(m) {
          dplyr::if_else(nchar(m) == 6, "510(k)", "510(k)s")
        }) |>
        # Trim whitespace and normalize multiple spaces
        stringr::str_squish()
    )
}

#' Get Harmonized Metric Mapping Table
#'
#' Returns a reference table showing how original metric names map to
#' their harmonized versions. Useful for understanding and validating
#' the harmonization process.
#'
#' @param data MDUFA dataset(s) to analyze
#' @return A tibble with columns: performance_metric (original),
#'   metric_harmonized (standardized), report_mdufa_period (source period)
#' @export
#'
#' @examples
#' # See how MDUFA III metrics are harmonized
#' get_harmonized_mapping(mdufa3) |>
#'   dplyr::filter(performance_metric != metric_harmonized)
get_harmonized_mapping <- function(data) {
  data |>
    harmonize_metric_names() |>
    dplyr::select(
      "performance_metric",
      "metric_harmonized",
      "report_mdufa_period"
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$metric_harmonized, .data$report_mdufa_period)
}
