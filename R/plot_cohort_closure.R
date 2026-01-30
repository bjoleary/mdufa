#' Plot Cohort Closure Percentage
#'
#' Creates a visualization of cohort closure percentages over time.
#' Shows the percentage of submissions in each fiscal year cohort that
#' have received a final decision.
#'
#' @param data A data frame containing cohort closure metrics with columns:
#'   `fy`, `org`, `percent_closed`, `status`.
#' @param show_phe Logical, whether to show COVID-19 Public Health Emergency
#'   highlighting (default: TRUE).
#' @param phe_label_y Numeric, y-position for PHE label text. If NA (default),
#'   position is calculated automatically from data.
#' @param color_by_period Logical, whether to color points by MDUFA period
#'   (default: TRUE). Requires `report_mdufa_period` column in data.
#' @param facet_ncol Number of columns in facet grid (default: 4).
#' @param title Optional plot title. If NULL, a default title is generated.
#' @param subtitle Optional plot subtitle. If NULL, a default subtitle is used.
#' @param caption Plot caption. If NULL (default), auto-generates a caption
#'   listing source reports with cutoff dates using
#'   \code{\link{source_caption_with_dates}}. If FALSE or NA, no caption is
#'   shown. If a character string, uses that value.
#'
#' @return A ggplot2 object that can be further customized with `+`.
#' @export
#'
#' @details
#' The plot displays cohort closure percentages using:
#' - Points for each fiscal year/organization, with shape indicating status
#' - Text labels showing the percentage value (100% labels are hidden)
#' - Colors by MDUFA reporting period when enabled
#'
#' Unlike other plot functions, this one does not have a `fade_open_cohorts`
#' parameter because the cohort closure percentage IS the metric being
#' displayed.
#'
#' @examples
#' \dontrun{
#' # Basic cohort closure plot
#' cohort_status |>
#'   filter(program == "510(k)") |>
#'   plot_cohort_closure()
#'
#' # With custom title
#' cohort_status |>
#'   filter(program == "510(k)") |>
#'   plot_cohort_closure(
#'     title = "510(k) Cohort Closure Status"
#'   )
#' }
plot_cohort_closure <- function(data,
                                show_phe = TRUE,
                                phe_label_y = NA,
                                color_by_period = TRUE,
                                facet_ncol = 4L,
                                title = NULL,
                                subtitle = NULL,
                                caption = NULL) {
  # Validate inputs
  chk::chk_data(data)
  chk::chk_flag(show_phe)
  chk::chk_flag(color_by_period)
  chk::chk_whole_number(facet_ncol)

  # Check required columns in data
  required_cols <- c("fy", "org", "percent_closed", "status")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    chk::abort_chk(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # Default title
  if (is.null(title)) {
    title <- paste0(
      "Percent of submissions closed as of ",
      "MDUFA performance report cutoff dates"
    )
  }

  # Default subtitle
  if (is.null(subtitle)) {
    subtitle <- "By review group and fiscal year received"
  }

  # Handle caption: auto-generate if NULL, suppress if FALSE/NA
  if (is.null(caption)) {
    if ("report_description" %in% names(data)) {
      caption <- source_caption_with_dates(data)
    }
  } else if (isFALSE(caption) || (length(caption) == 1 && is.na(caption))) {
    caption <- NULL
  }

  # Determine y position for PHE label (near bottom of plot)
  y_phe_label <- if (is.na(phe_label_y)) {
    min(data$percent_closed, na.rm = TRUE) * 0.8
  } else {
    phe_label_y
  }

  # Build base plot
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$fy,
      y = .data$percent_closed
    )
  )

  # Add faceting
  p <- p + facet_by_org(ncol = facet_ncol)

  # Add PHE highlighting if requested
  if (show_phe) {
    p <- p +
      geom_phe_rect() +
      geom_phe_label(y_text = y_phe_label)
  }

  # Add points with status styling
  if (color_by_period) {
    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          shape = .data$status,
          color = .data$report_mdufa_period
        ),
        na.rm = TRUE
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          shape = .data$status
        ),
        na.rm = TRUE
      )
  }

  # Add text labels (hide 100% labels to reduce clutter)
  # Format percent and remove "100%" entries
  data <- data |>
    dplyr::mutate(
      label_text = dplyr::if_else(
        .data$percent_closed == 1,
        "",
        scales::percent(.data$percent_closed, accuracy = 1)
      )
    )

  if (color_by_period) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          label = .data$label_text,
          color = .data$report_mdufa_period
        ),
        size = 2,
        vjust = 1,
        nudge_y = -0.02,
        na.rm = TRUE,
        check_overlap = TRUE,
        show.legend = FALSE
      )
  } else {
    p <- p +
      ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          label = .data$label_text
        ),
        size = 2,
        vjust = 1,
        nudge_y = -0.02,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  # Add scales
  p <- p +
    scale_x_fy() +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(NA, 1)
    )

  # Add shape scale with both options always shown
  p <- p + ggplot2::scale_shape_manual(
    values = c(
      "Cohort Closed" = 16,
      "Cohort Open" = 1
    ),
    limits = c("Cohort Closed", "Cohort Open"),
    drop = FALSE,
    name = NULL
  )

  # Add theme
  p <- p + theme_mdufa()

  # Set legend order: shape before color
  p <- p + ggplot2::guides(
    shape = ggplot2::guide_legend(order = 1),
    color = ggplot2::guide_legend(order = 2)
  )

  # Add labels
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Fiscal Year Received",
      y = "Percent Closed",
      caption = caption,
      color = NULL
    )

  p
}
