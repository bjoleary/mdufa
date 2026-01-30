#' Plot Submission Volume with Predictions
#'
#' Creates a visualization of submission volume over time with optional
#' linear model predictions and confidence intervals.
#'
#' @param data A data frame containing volume metrics with columns:
#'   `fy`, `org`, `value` (count of submissions). If predictions are included,
#'   also needs `prediction`, `low`, `high`, and `type` columns.
#' @param show_predictions Logical, whether to show prediction ribbon and line
#'   (default: TRUE). Requires `prediction`, `low`, `high`, `type` columns.
#' @param show_phe Logical, whether to show COVID-19 Public Health Emergency
#'   highlighting (default: TRUE).
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
#' The plot displays submission volume using:
#' - Points for each fiscal year/organization
#' - Text labels showing the count value
#' - Optional prediction ribbon (confidence interval) and trend line
#' - Shape indicates Actual vs Predicted values when predictions are shown
#'
#' To include predictions, prepare your data with a linear model:
#' ```
#' model <- lm(value ~ fy * org, data = data)
#' preds <- predict(model, newdata = data, se.fit = TRUE)
#' data <- data |>
#'   mutate(
#'     prediction = preds$fit,
#'     low = prediction - 1.96 * preds$se.fit,
#'     high = prediction + 1.96 * preds$se.fit,
#'     type = if_else(is.na(value), "Predicted", "Actual"),
#'     value = coalesce(value, prediction)
#'   )
#' ```
#'
#' @examples
#' \dontrun{
#' # Basic volume plot without predictions
#' data_volume |>
#'   plot_volume(show_predictions = FALSE)
#'
#' # Volume plot with predictions
#' data_volume_with_preds |>
#'   plot_volume(
#'     title = "510(k) Submission Volume",
#'     caption = "Source: MDUFA Performance Reports"
#'   )
#' }
plot_volume <- function(data,
                        show_predictions = TRUE,
                        show_phe = TRUE,
                        color_by_period = TRUE,
                        facet_ncol = 4L,
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL) {
  # Validate inputs
  chk::chk_data(data)
  chk::chk_flag(show_predictions)
  chk::chk_flag(show_phe)
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

  # Check prediction columns if needed
  if (show_predictions) {
    pred_cols <- c("prediction", "low", "high", "type")
    missing_pred_cols <- setdiff(pred_cols, names(data))
    if (length(missing_pred_cols) > 0) {
      chk::abort_chk(
        paste0(
          "show_predictions = TRUE but missing columns: ",
          paste(missing_pred_cols, collapse = ", "),
          ". Set show_predictions = FALSE or add prediction columns."
        )
      )
    }
  }

  # Default title
  if (is.null(title)) {
    title <- "Submissions received"
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
  y_phe_label <- min(data$value, na.rm = TRUE) * 0.1

  # Build base plot
  if (show_predictions && color_by_period) {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$value,
        color = .data$report_mdufa_period,
        shape = .data$type
      )
    )
  } else if (show_predictions) {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$value,
        shape = .data$type
      )
    )
  } else if (color_by_period) {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$value,
        color = .data$report_mdufa_period
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$value
      )
    )
  }

  # Add faceting
  p <- p + facet_by_org(ncol = facet_ncol)

  # Add PHE highlighting if requested
  if (show_phe) {
    p <- p +
      geom_phe_rect() +
      geom_phe_label(y_text = y_phe_label)
  }

  # Add prediction ribbon and line if requested
  # Use group = org to ensure continuous ribbon/line across MDUFA periods
  if (show_predictions) {
    p <- p +
      ggplot2::geom_ribbon(
        mapping = ggplot2::aes(
          ymin = .data$low,
          ymax = .data$high,
          group = .data$org
        ),
        color = NA,
        fill = "lightgray",
        alpha = 0.5,
        show.legend = FALSE,
        na.rm = TRUE
      ) +
      ggplot2::geom_line(
        mapping = ggplot2::aes(
          y = .data$prediction,
          group = .data$org
        ),
        color = "gray50",
        alpha = 0.5,
        show.legend = FALSE,
        na.rm = TRUE
      )
  }

  # Add points
  p <- p + ggplot2::geom_point(na.rm = TRUE)

  # Add text labels
  data <- data |>
    dplyr::mutate(
      label_text = round(.data$value)
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
        nudge_y = -20,
        na.rm = TRUE,
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
        nudge_y = -20,
        na.rm = TRUE
      )
  }

  # Add scales
  p <- p + scale_x_fy()

  # Add shape scale if predictions shown
  # Actual = filled circle (16), Predicted = open circle (1)
  if (show_predictions) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = c("Actual" = 16, "Predicted" = 1),
        na.translate = FALSE
      )
  }

  # Add theme
  p <- p + theme_mdufa()

  # Set legend order: shape before color
  if (show_predictions) {
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
      y = "Count",
      caption = caption,
      color = NULL,
      shape = NULL
    )

  p
}
