#' Plot Quintile Days Metrics
#'
#' Creates a visualization of FDA Days, Industry Days, or Total Days
#' performance metrics using a quintile plot format. Shows percentile
#' lines (20th, 40th, 60th, 80th) as horizontal segments and average
#' values as points.
#'
#' @param data A data frame containing days metrics with columns:
#'   `fy`, `org`, `value`, `performance_metric`. Should contain percentile
#'   metrics (20th, 40th, 60th, 80th) and optionally "Average" metrics.
#' @param day_type Type of days metric: "fda", "industry", or "total".
#'   Determines y-axis scale breaks.
#' @param cohort_status A data frame with cohort closure status (required).
#'   Must contain `fy`, `org`, `percent_closed`, and `status` columns.
#'   Average points are styled by status; opacity reflects closure percentage.
#' @param show_phe Logical, whether to show COVID-19 Public Health Emergency
#'   highlighting (default: TRUE).
#' @param show_shutdowns Logical, whether to show government shutdown markers
#'   (default: FALSE). Shows gray square markers at affected fiscal years.
#' @param scale_shutdowns_by_duration Logical, whether to scale shutdown marker
#'   size by duration (default: FALSE). When TRUE, longer shutdowns have larger
#'   markers.
#' @param fade_open_cohorts Logical, whether to reduce opacity for open cohorts
#'   (default: TRUE). When TRUE, alpha is scaled based on `percent_closed`
#'   (range 0.3-1.0).
#' @param y_limits Optional y-axis limits as a length-2 numeric vector.
#'   Use to create clipped versions of plots (e.g., `c(0, 90)` for FDA days).
#' @param show_labels Logical, whether to show numeric labels on average
#'   points (default: TRUE).
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
#' The plot displays percentile distributions of review times using:
#' - Horizontal line segments at each percentile level (20th, 40th, 60th, 80th)
#' - Points for average values, with shape indicating cohort status
#' - Optional opacity based on cohort closure percentage
#'
#' When `fade_open_cohorts = TRUE`, the alpha (opacity) of average points
#' is scaled based on `percent_closed` (range 0.3-1.0). Fully closed cohorts
#' are fully opaque, while open cohorts are semi-transparent, visually
#' indicating that their metrics may change in future reports.
#'
#' @examples
#' \dontrun{
#' # Basic FDA days plot
#' data_fdadays |>
#'   plot_quintile_days(
#'     day_type = "fda",
#'     cohort_status = data_cohort_status
#'   )
#'
#' # With clipped y-axis
#' data_fdadays |>
#'   plot_quintile_days(
#'     day_type = "fda",
#'     cohort_status = data_cohort_status,
#'     y_limits = c(0, 90)
#'   )
#'
#' # Total days with shutdown markers
#' data_totaldays |>
#'   plot_quintile_days(
#'     day_type = "total",
#'     cohort_status = data_cohort_status,
#'     show_shutdowns = TRUE
#'   )
#' }
plot_quintile_days <- function(data,
                               day_type = c("fda", "industry", "total"),
                               cohort_status,
                               show_phe = TRUE,
                               show_shutdowns = FALSE,
                               scale_shutdowns_by_duration = FALSE,
                               fade_open_cohorts = TRUE,
                               y_limits = NULL,
                               show_labels = TRUE,
                               facet_ncol = 4L,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL) {
  # Validate inputs
  day_type <- match.arg(day_type)
  chk::chk_data(data)
  chk::chk_flag(show_phe)
  chk::chk_flag(show_shutdowns)
  chk::chk_flag(scale_shutdowns_by_duration)
  chk::chk_flag(fade_open_cohorts)
  chk::chk_flag(show_labels)
  chk::chk_whole_number(facet_ncol)

  # Check required columns
  required_cols <- c("fy", "org", "value", "performance_metric")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    chk::abort_chk(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # Extract percentile labels if not already done
  if (!any(grepl("Percentile|Average", data$performance_metric))) {
    data <- extract_percentiles(data)
  }

  # Validate and join cohort status (required)
  chk::chk_data(cohort_status)
  if (!"percent_closed" %in% names(cohort_status)) {
    chk::abort_chk("cohort_status must contain 'percent_closed' column")
  }
  if (!"status" %in% names(cohort_status)) {
    chk::abort_chk("cohort_status must contain 'status' column")
  }

  # Validate that cohort_status comes from the same reports as the data

  validate_cohort_status_dates(data, cohort_status)

  # Remove any existing status/percent_closed columns from data to avoid
  # suffix conflicts during join
  data <- data |>
    dplyr::select(-dplyr::any_of(c("percent_closed", "status")))

  data <- dplyr::left_join(
    data,
    cohort_status |>
      dplyr::select(
        dplyr::any_of(c("fy", "org", "percent_closed", "status"))
      ),
    by = c("fy", "org")
  )

  # Prepare percentile data (exclude Average)
  data_percentiles <- data |>
    dplyr::filter(.data$performance_metric != "Average") |>
    dplyr::arrange(.data$performance_metric) |>
    dplyr::mutate(
      performance_metric = .data$performance_metric |>
        forcats::as_factor() |>
        forcats::fct_rev()
    )

  # Prepare average data
  data_averages <- data |>
    dplyr::filter(.data$performance_metric == "Average")

  # Determine y-axis label
  y_label <- switch(day_type,
    fda = "FDA Days",
    industry = "Industry Days",
    total = "Total Days"
  )

  # Default title
  if (is.null(title)) {
    title <- paste0(y_label, " performance metrics")
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

  # Build base plot
  p <- ggplot2::ggplot(
    data = data_percentiles,
    mapping = ggplot2::aes(
      x = .data$fy,
      y = .data$value,
      color = .data$performance_metric
    )
  )

  # Add faceting
  p <- p + facet_by_org(ncol = facet_ncol)

  # Add PHE highlighting if requested
  if (show_phe) {
    p <- p +
      geom_phe_rect() +
      geom_phe_label(y_text = 0)
  }

  # Add shutdown markers if requested
  if (show_shutdowns) {
    p <- p + annotate_shutdowns(
      y_position = 0,
      scale_by_duration = scale_shutdowns_by_duration
    )
  }

  # Add percentile segments
  if (fade_open_cohorts && "percent_closed" %in% names(data_percentiles)) {
    p <- p +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = .data$fy - 0.25,
          y = .data$value,
          xend = .data$fy + 0.25,
          yend = .data$value,
          alpha = .data$percent_closed
        ),
        na.rm = TRUE
      )
  } else {
    p <- p +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = .data$fy - 0.25,
          y = .data$value,
          xend = .data$fy + 0.25,
          yend = .data$value
        ),
        na.rm = TRUE
      )
  }

  # Add average points
  # Shape by cohort status: hollow circle (1) for open, filled (16) for closed
  # Create shape labels that include "Average"
  data_averages <- data_averages |>
    dplyr::mutate(
      shape_label = dplyr::if_else(
        .data$status == "Cohort Closed",
        "Average (Cohort Closed)",
        "Average (Cohort Open)"
      )
    )

  # Shape by status, alpha for percent_closed if fade_open_cohorts
  if (fade_open_cohorts) {
    p <- p +
      ggplot2::geom_point(
        data = data_averages,
        mapping = ggplot2::aes(
          x = .data$fy,
          y = .data$value,
          shape = .data$shape_label,
          alpha = .data$percent_closed
        ),
        color = "black",
        na.rm = TRUE
      ) +
      ggplot2::scale_alpha_continuous(
        range = c(0.3, 1),
        limits = c(0.6, 1),
        oob = scales::squish,
        guide = "none"
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = data_averages,
        mapping = ggplot2::aes(
          x = .data$fy,
          y = .data$value,
          shape = .data$shape_label
        ),
        color = "black",
        na.rm = TRUE
      )
  }

  # Add labels if requested
  if (show_labels && nrow(data_averages) > 0) {
    if (fade_open_cohorts) {
      p <- p +
        ggplot2::geom_text(
          data = data_averages,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$value,
            label = format(.data$value, digits = 4),
            alpha = .data$percent_closed
          ),
          color = "black",
          size = 2,
          vjust = 1,
          nudge_y = -1.5,
          na.rm = TRUE,
          check_overlap = TRUE
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = data_averages,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$value,
            label = format(.data$value, digits = 4)
          ),
          color = "black",
          size = 2,
          vjust = 1,
          nudge_y = -1.5,
          na.rm = TRUE,
          check_overlap = TRUE
        )
    }
  }

  # Add scales
  p <- p +
    scale_x_fy() +
    scale_y_days(day_type, limits = y_limits)

  # Add shape scale with both options always shown
  # Include note about fading for open cohorts
  cohort_note <- if (fade_open_cohorts) {
    "Fainter values are from cohorts with\nmore open submissions and may change." # nolint
  } else {
    NULL
  }

  # Cohort status: hollow circle (1) for open, filled circle (16) for closed
  # Government Shutdown: diamond (18)
  # Use drop = FALSE to show both cohort status options in legend
  if (show_shutdowns) {
    p <- p + ggplot2::scale_shape_manual(
      values = c(
        "Average (Cohort Closed)" = 16,
        "Average (Cohort Open)" = 1,
        "Government Shutdown" = 18
      ),
      limits = c(
        "Average (Cohort Closed)",
        "Average (Cohort Open)",
        "Government Shutdown"
      ),
      drop = FALSE,
      name = cohort_note
    )
  } else {
    p <- p + ggplot2::scale_shape_manual(
      values = c(
        "Average (Cohort Closed)" = 16,
        "Average (Cohort Open)" = 1
      ),
      limits = c("Average (Cohort Closed)", "Average (Cohort Open)"),
      drop = FALSE,
      name = cohort_note
    )
  }

  # Add theme
  p <- p + theme_mdufa()

  # Override theme to show shape legend title (cohort note)
  # Set order so shape legend comes before color legend
  if (!is.null(cohort_note)) {
    p <- p + ggplot2::guides(
      shape = ggplot2::guide_legend(
        title = cohort_note,
        title.theme = ggplot2::element_text(size = 8),
        order = 1
      ),
      color = ggplot2::guide_legend(order = 2)
    )
  } else {
    p <- p + ggplot2::guides(
      shape = ggplot2::guide_legend(order = 1),
      color = ggplot2::guide_legend(order = 2)
    )
  }

  # Add labels
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Fiscal Year Received",
      y = y_label,
      caption = caption
    )

  p
}
