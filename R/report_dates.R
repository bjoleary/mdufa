# Do not hand edit this file. Edit data-raw/report_dates.R instead.
#' Report Dates
#'
#' Mapping of MDUFA quarterly report publication dates to their data cutoff
#' dates. Each report contains performance data through a specific cutoff
#' date (typically the end of a fiscal quarter).
#'
#' @format A tibble with 80 rows and 5 fields:
#'
#' \describe{
#' \item{report_date}{date 2009-01-28, 2009-05-04, 2009-08-04, 2009-11-1…}
#' \item{report_cutoff_date}{date 2008-12-31, 2009-03-31, 2009-06-30,
#' 2009-09-3…}
#' \item{report_mdufa_period}{chr "MDUFA II", "MDUFA II", "MDUFA II", "MDUFA
#' II"…}
#' \item{report_description}{chr "January 28, 2009 FDA Performance Report
#' (Arch…}
#' \item{report_link}{chr "https://wayback.archive-it.org/7993/201701120…}
#' }
#'
#' @source
#' [FDA MDUFA Reports](https://www.fda.gov/industry/medical-device-user-fee-amendments-mdufa-fees/mdufa-reports) # nolint: line_length_linter.
#' accessed 2025-12-13.
"report_dates"
