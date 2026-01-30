#' Plot Review Cycles Metrics
#'
#' Creates a visualization of average review cycles over time.
#' Points are styled by cohort status (open vs closed).
#'
#' @param data A data frame containing review cycle metrics with columns:
#'   `fy`, `org`, `value` (average cycles as numeric).
#' @param cohort_status A data frame with cohort closure status (required).
#'   Must contain `fy`, `org`, `percent_closed`, and `status` columns.
#' @param show_phe Logical, whether to show COVID-19 Public Health Emergency
#'   highlighting (default: TRUE).
#' @param phe_label_y Numeric, y-axis position for the PHE label.
#'   If NA (default), uses the lower y-axis limit.
#' @param fade_open_cohorts Logical, whether to reduce opacity for open cohorts
#'   (default: TRUE). When TRUE, alpha is scaled based on `percent_closed`
#'   (range 0.3-1.0).
#' @param color_by_period Logical, whether to color points by MDUFA period
#'   (default: TRUE). Requires `report_mdufa_period` column in data.
#' @param y_limits Numeric vector of length 2, y-axis limits
#'   (default: c(1, NA)). Use c(1, NA) for cycles, c(0, 1) for AINN rate.
#' @param label_format Character, format for text labels: "numeric" (default)
#'   shows values as decimal numbers (e.g., "1.23"), "percent" shows values
#'   as percentages (e.g., "23%").
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
#' The plot displays review cycle metrics using:
#' - Points for each fiscal year/organization, with shape indicating status
#' - Text labels showing the numeric value (rounded to 2 decimal places)
#' - Optional opacity based on cohort closure percentage
#'
#' This function can also be used for AINN Rate (Additional Information
#' Needed rate) by transforming the data before calling:
#' `data |> mutate(value = value - 1)` and then overriding the y-axis
#' scale to show percentages.
#'
#' @examples
#' \dontrun{
#' # Basic cycles plot
#' data_cycles |>
#'   plot_cycles(
#'     cohort_status = cohort_status |> filter(program == "510(k)")
#'   )
#'
#' # AINN Rate (transformed cycles, formatted as percent)
#' data_cycles |>
#'   mutate(value = value - 1) |>
#'   plot_cycles(
#'     cohort_status = cohort_status |> filter(program == "510(k)"),
#'     y_limits = c(0, 1),
#'     label_format = "percent",
#'     title = "Estimated rate at which 510(k)s are placed on hold"
#'   ) +
#'   labs(y = "Estimated Percent Placed On Hold")
#' }
plot_cycles <- function(data,
                        cohort_status,
                        show_phe = TRUE,
                        phe_label_y = NA,
                        fade_open_cohorts = TRUE,
                        color_by_period = TRUE,
                        y_limits = c(1, NA),
                        label_format = c("numeric", "percent"),
                        facet_ncol = 4L,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL) {
  # Validate inputs
  label_format <- match.arg(label_format)
  chk::chk_data(data)
  chk::chk_flag(show_phe)
  chk::chk_flag(fade_open_cohorts)
  chk::chk_flag(color_by_period)
  chk::chk_whole_number(facet_ncol)

  # Check required columns in data
  required_cols <- c("fy", "org", "value")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    chk::abort_chk(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
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

  # Select the columns we need from cohort_status
  cohort_cols <- cohort_status |>
    dplyr::select(
      dplyr::any_of(c("fy", "org", "percent_closed", "status"))
    )

  data <- dplyr::left_join(
    data,
    cohort_cols,
    by = c("fy", "org")
  )

  # Default title
  if (is.null(title)) {
    title <- "Average number of review cycles"
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
  y_phe_label <- if (is.na(phe_label_y)) y_limits[1] else phe_label_y

  # Build base plot
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = .data$fy,
      y = .data$value
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

  # Create shape labels
  data <- data |>
    dplyr::mutate(
      shape_label = dplyr::if_else(
        .data$status == "Cohort Closed",
        "Cohort Closed",
        "Cohort Open"
      )
    )

  # Add points with cohort status styling
  if (fade_open_cohorts && color_by_period) {
    p <- p +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          shape = .data$shape_label,
          alpha = .data$percent_closed,
          color = .data$report_mdufa_period
        ),
        na.rm = TRUE
      ) +
      ggplot2::scale_alpha_continuous(
        range = c(0.3, 1),
        limits = c(0.6, 1),
        oob = scales::squish,
        guide = "none"
      )
  } else if (fade_open_cohorts) {
    p <- p +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          shape = .data$shape_label,
          alpha = .data$percent_closed
        ),
        na.rm = TRUE
      ) +
      ggplot2::scale_alpha_continuous(
        range = c(0.3, 1),
        limits = c(0.6, 1),
        oob = scales::squish,
        guide = "none"
      )
  } else if (color_by_period) {
    p <- p +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          shape = .data$shape_label,
          color = .data$report_mdufa_period
        ),
        na.rm = TRUE
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          shape = .data$shape_label
        ),
        na.rm = TRUE
      )
  }

  # Add text labels with appropriate formatting
  # Calculate nudge based on data range
  nudge_amount <- if (label_format == "percent") -0.03 else -0.02

  # Create label column based on format
  if (label_format == "percent") {
    data <- data |>
      dplyr::mutate(
        label_text = scales::percent(.data$value, accuracy = 1)
      )
  } else {
    data <- data |>
      dplyr::mutate(
        label_text = sprintf("%.2f", .data$value)
      )
  }

  if (fade_open_cohorts && color_by_period) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          label = .data$label_text,
          alpha = .data$percent_closed,
          color = .data$report_mdufa_period
        ),
        size = 2,
        vjust = 1,
        nudge_y = nudge_amount,
        na.rm = TRUE,
        check_overlap = TRUE,
        show.legend = FALSE
      )
  } else if (fade_open_cohorts) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          label = .data$label_text,
          alpha = .data$percent_closed
        ),
        size = 2,
        vjust = 1,
        nudge_y = nudge_amount,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  } else if (color_by_period) {
    p <- p +
      ggplot2::geom_text(
        data = data,
        mapping = ggplot2::aes(
          label = .data$label_text,
          color = .data$report_mdufa_period
        ),
        size = 2,
        vjust = 1,
        nudge_y = nudge_amount,
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
        nudge_y = nudge_amount,
        na.rm = TRUE,
        check_overlap = TRUE
      )
  }

  # Add scales
  p <- p + scale_x_fy()

  if (label_format == "percent") {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = scales::percent,
        limits = y_limits
      )
  } else {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = y_limits
      )
  }

  # Add shape scale with both options always shown
  # Include note about fading for open cohorts
  cohort_note <- if (fade_open_cohorts) {
    "Fainter values are from cohorts with\nmore open submissions and may change." # nolint
  } else {
    NULL
  }

  p <- p + ggplot2::scale_shape_manual(
    values = c(
      "Cohort Closed" = 16,
      "Cohort Open" = 1
    ),
    limits = c("Cohort Closed", "Cohort Open"),
    drop = FALSE,
    name = cohort_note
  )

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
      y = "Average Number of Review Cycles",
      caption = caption,
      color = NULL
    )

  p
}
