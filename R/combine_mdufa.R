#' Combine MDUFA Datasets at OHT Level
#'
#' Combines mdufa3_oht, mdufa4, and mdufa5 into a single dataset
#' with consistent OHT-level organizations. Adds a `derived` column
#' to indicate which rows were calculated (aggregated) vs. extracted
#' directly from PDF reports.
#'
#' The function also applies [harmonize_metric_names()] to standardize
#' metric names across MDUFA periods.
#'
#' @param mdufa3_oht_data Optional MDUFA III OHT data. If NULL, uses
#'   the bundled mdufa3_oht dataset.
#' @param mdufa4_data Optional MDUFA IV data. If NULL, uses the
#'   bundled mdufa4 dataset.
#' @param mdufa5_data Optional MDUFA V data. If NULL, uses the
#'   bundled mdufa5 dataset.
#' @param filter_to_ohts If TRUE (default), filters to only OHT
#'   organizations. Set to FALSE to include CDRH/CBER/ODE/OIR.
#'
#' @return Combined tibble with columns:
#'   - All standard MDUFA data columns
#'   - `derived`: logical, TRUE for MDUFA III OHT7 aggregated rows
#'   - `metric_harmonized`: standardized metric name for cross-period analysis
#'
#' @export
#'
#' @examples
#' combined <- combine_mdufa_oht()
#' unique(combined$organization)
#' table(combined$report_mdufa_period, combined$derived)
combine_mdufa_oht <- function(mdufa3_oht_data = NULL,
                              mdufa4_data = NULL,
                              mdufa5_data = NULL,
                              filter_to_ohts = TRUE) {
  # Load datasets if not provided
  if (is.null(mdufa3_oht_data)) {
    mdufa3_oht_data <- mdufa3_oht
  }
  if (is.null(mdufa4_data)) {
    mdufa4_data <- mdufa4
  }
  if (is.null(mdufa5_data)) {
    mdufa5_data <- mdufa5
  }


  # mdufa3_oht already has derived column (TRUE for OHT7, FALSE for others)
  # Add derived = FALSE to MDUFA IV and V (all directly extracted)
  m3 <- mdufa3_oht_data

  m4 <- mdufa4_data |>
    dplyr::mutate(derived = FALSE)

  m5 <- mdufa5_data |>
    dplyr::mutate(derived = FALSE)

  # Filter to OHTs if requested
  if (filter_to_ohts) {
    m4 <- m4 |>
      dplyr::filter(stringr::str_detect(.data$organization, "^OHT\\d"))
    m5 <- m5 |>
      dplyr::filter(stringr::str_detect(.data$organization, "^OHT\\d"))
  }

  # Combine and harmonize metric names
  dplyr::bind_rows(m3, m4, m5) |>
    harmonize_metric_names()
}

#' Filter to Comparable OHT Organizations
#'
#' Filters a combined MDUFA dataset to only include OHT organizations
#' that can be compared across all three MDUFA periods (III, IV, V).
#' This excludes OHT1, OHT3, and OHT4 which don't have clean mappings
#' from MDUFA III divisions.
#'
#' Comparable OHTs:
#' - OHT2 (Cardiovascular) <- DCD
#' - OHT5 (Neurological) <- DNPMD
#' - OHT6 (Orthopedic) <- DOD
#' - OHT7 (IVD) <- DCTD + DIHD + DMD + DMGP (aggregated)
#' - OHT8 (Radiological) <- DRH
#'
#' @param data Combined MDUFA dataset from [combine_mdufa_oht()]
#' @return Filtered dataset with only comparable OHTs
#' @export
#'
#' @examples
#' combined <- combine_mdufa_oht()
#' comparable <- filter_comparable_ohts(combined)
#' unique(comparable$organization)
filter_comparable_ohts <- function(data) {
  comparable_ohts <- c("OHT2", "OHT5", "OHT6", "OHT7", "OHT8")

  data |>
    dplyr::filter(.data$organization %in% comparable_ohts)
}

#' Get Available Fiscal Years by Period
#'
#' Returns the fiscal years available in each MDUFA period. Useful for
#' understanding the time coverage when combining datasets.
#'
#' @param data Combined MDUFA dataset from [combine_mdufa_oht()]
#' @return A tibble with report_mdufa_period and available fiscal years
#' @export
#'
#' @examples
#' combined <- combine_mdufa_oht()
#' get_available_fys(combined)
get_available_fys <- function(data) {
  data |>
    dplyr::filter(!is.na(.data$fy)) |>
    dplyr::group_by(.data$report_mdufa_period) |>
    dplyr::summarise(
      min_fy = min(as.integer(.data$fy), na.rm = TRUE),
      max_fy = max(as.integer(.data$fy), na.rm = TRUE),
      fiscal_years = paste(sort(unique(.data$fy)), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$min_fy)
}
