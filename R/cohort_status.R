# Do not hand edit this file. Edit data-raw/cohort_status.R instead.
#' Cohort Status
#'
#' Cohort closure status calculated from submissions pending and number
#' received metrics. Used with plot_quintile_days() to indicate whether
#' a fiscal year cohort has all decisions or is still open.
#'
#' @format A tibble with 626 rows and 8 fields:
#'
#' \describe{
#' \item{report_date}{date 2018-12-10, 2018-12-10, 2018-12-10, 2018-12-1…}
#' \item{report_mdufa_period}{chr "MDUFA III", "MDUFA III", "MDUFA III", "MDUFA
#' …}
#' \item{organization}{chr "CBER", "CBER", "CBER", "CBER", "CBER", "CBER"…}
#' \item{org}{chr "CBER: Center for Biologics Evaluation and Res…}
#' \item{program}{chr "510(k)", "510(k)", "510(k)", "510(k)", "510(k…}
#' \item{fy}{int 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020…}
#' \item{percent_closed}{dbl 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.…}
#' \item{status}{chr "Cohort Closed", "Cohort Closed", "Cohort Clos…}
#' }
#'
#' @source Derived from mdufa_combined dataset.
#' Generated 2025-12-13.
"cohort_status"
