# Do not hand edit this file. Edit data-raw/mdufa_combined.R instead.
#' Combined MDUFA Dataset
#'
#' A unified dataset combining FDA quarterly Medical Device User Fee
#' performance metrics from MDUFA II, III, IV, and V. Includes harmonized
#' organization names and metric names for cross-period comparison.
#'
#' @format A tibble with 55713 rows and 17 fields:
#'
#' \describe{
#' \item{report_description}{chr "January 29, 2014 FDA MDUFA II Performance
#' Rep…}
#' \item{report_link}{chr "https://www.fda.gov/media/88098/download", "h…}
#' \item{report_date}{date 2014-01-29, 2014-01-29, 2014-01-29, 2014-01-2…}
#' \item{report_mdufa_period}{chr "MDUFA II", "MDUFA II", "MDUFA II", "MDUFA
#' II"…}
#' \item{source}{chr "MDUFA II Quarterly (Non expedited PMA Orginal…}
#' \item{page}{chr "1", "1", "1", "1", "1", "1", "1", "1", "1", "…}
#' \item{table_number}{chr "1", "1", "1", "1", "1", "1", "1", "1", "1", "…}
#' \item{organization}{chr "CDRH", "CDRH", "CDRH", "CDRH", "CDRH", "CDRH"…}
#' \item{org}{chr "CDRH: Center for Devices and Radiological Hea…}
#' \item{program}{chr "PMA Original and Panel-track Supplements", "P…}
#' \item{table_title}{chr "Non expedited PMA Orginal and Panel-track Sup…}
#' \item{metric_type}{chr "integer", "integer", "percent", "text", "perc…}
#' \item{performance_metric}{chr "Workload (Filed to Date)", "Total FDA
#' Decisio…}
#' \item{metric_harmonized}{chr "Workload (Filed To Date)", "Total FDA
#' Decisio…}
#' \item{fy}{chr "2008", "2008", "2008", "2008", "2008", "2008"…}
#' \item{value}{chr "33", "33", "60%", "yes", "64%", "64%", "90%",…}
#' \item{derived}{lgl FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…}
#' }
#'
#' @source
#' [FDA MDUFA Reports](https://www.fda.gov/industry/medical-device-user-fee-amendments-mdufa-fees/mdufa-reports) # nolint: line_length_linter.
#' accessed 2025-12-13.
"mdufa_combined"
