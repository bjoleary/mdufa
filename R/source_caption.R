#' Generate Source Caption with Cutoff Dates
#'
#' Creates a caption string listing source reports with their data cutoff
#' dates. Useful for plot captions citing the MDUFA performance reports used.
#'
#' @param .data A data frame containing a `report_description` column.
#'   This column should contain the descriptions of MDUFA performance reports
#'   used as data sources.
#'
#' @return Character string with formatted caption. Each report is listed on
#'   its own line with the format:
#'   "Report Description (data through Month DD, YYYY)"
#'
#' @details
#' The function looks up cutoff dates from the \code{\link{report_dates}}
#' dataset in this package. If a report description is not found or has no
#' cutoff date, it will be listed without the date annotation.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate caption from filtered data
#' data_volume |>
#'   dplyr::filter(org == "CDRH") |>
#'   source_caption_with_dates()
#'
#' # Use in a plot
#' plot_volume(
#'   data = my_data,
#'   caption = source_caption_with_dates(my_data)
#' )
#' }
source_caption_with_dates <- function(.data) {
  # Validate input
  if (!is.data.frame(.data)) {
    chk::abort_chk(".data must be a data frame")
  }

  if (!"report_description" %in% names(.data)) {
    chk::abort_chk(".data must contain a 'report_description' column")
  }

  # Get unique report descriptions from the data
  reports <- .data |>
    dplyr::distinct(.data$report_description) |>
    dplyr::left_join(
      mdufa::report_dates |>
        dplyr::select("report_description", "report_cutoff_date"),
      by = "report_description"
    ) |>
    dplyr::arrange(.data$report_description) |>
    dplyr::mutate(
      caption_line = dplyr::if_else(
        is.na(.data$report_cutoff_date),
        .data$report_description,
        paste0(
          .data$report_description,
          " (data through ",
          format(.data$report_cutoff_date, "%B %d, %Y"),
          ")"
        )
      )
    )

  paste0(
    "Sources: ",
    paste0(reports$caption_line, collapse = "\n")
  )
}
