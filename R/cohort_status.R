#' Cohort Status for All Organizations, Programs, and Fiscal Years
#'
#' A dataset containing cohort closure status calculated from submissions
#' pending and number received metrics. This dataset is used with
#' [plot_quintile_days()] to style average points by cohort status.
#'
#' Cohort status indicates whether all submissions from a fiscal year cohort
#' have received MDUFA decisions. Open cohorts may have metrics that change
#' in future reports as remaining submissions receive decisions.
#'
#' @format A tibble with the following columns:
#' \describe{
#'   \item{fy}{Fiscal year}
#'   \item{organization}{Short organization code (e.g., "OHT7", "CDRH")}
#'   \item{org}{Full organization name (e.g., "OHT7: In Vitro Diagnostics")}
#'   \item{program}{Submission program (e.g., "510(k)", "PMA", "De Novo")}
#'   \item{percent_closed}{Proportion of cohort with decisions (0-1). Calculated
#'     as (number_received - submissions_pending) / number_received.}
#'   \item{status}{Character: "Cohort Closed" if percent_closed >= 1.0,
#'     otherwise "Cohort Open"}
#'   \item{report_date}{Date of the source report. Used for matching cohort
#'     status to performance metrics from the same report.}
#'   \item{report_mdufa_period}{MDUFA period of the source report (e.g.,
#'     "MDUFA III", "MDUFA IV", "MDUFA V"). Used for coloring in plots.}
#' }
#'
#' @details
#' The dataset is derived from the most recent report for each fy/org/program
#' combination in [mdufa_combined]. It uses:
#' \itemize{
#'   \item "Submissions Pending MDUFA Decision" metrics for pending count

#'   \item "Number Received" metrics for total submissions
#' }
#'
#' When joining to metrics data for plotting, use
#' `by = c("fy", "organization", "program")`
#' or `by = c("fy", "organization")` if program is already filtered.
#'
#' @seealso
#' \itemize{
#'   \item [plot_quintile_days()] for creating quintile plots with cohort status
#'   \item [mdufa_combined] for the source data
#' }
#'
#' @examples
#' # View 510(k) cohort status
#' library(dplyr)
#' cohort_status |>
#'   filter(program == "510(k)") |>
#'   head(20)
#'
#' # Check which fiscal years are still open for OHT7
#' cohort_status |>
#'   filter(
#'     program == "510(k)",
#'     stringr::str_detect(org, "OHT7"),
#'     status == "Cohort Open"
#'   )
"cohort_status"
