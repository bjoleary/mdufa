#' Plot Decision Rate (Stacked Bar with Overlaid Points)
#'
#' Creates a stacked bar visualization of decision rates (SE/NSE/Withdrawn)
#' with optional points and labels overlaid on the "cleared" (SE) bars.
#'
#' @param data A data frame containing decision rate metrics with columns:
#'   `fy`, `org`, `percent` (0-1 scale), `decision` (factor with levels for
#'   stacking order).
#' @param data_se A data frame containing just the SE (cleared) data for
#'   overlaying points and labels. Should have: `fy`, `org`, `percent`, and
#'   optionally `status` and `percent_closed` for cohort status styling.
#' @param cohort_status Optional data frame with cohort status for fading bars.
#'   Should have: `fy`, `org`, `percent_closed`, `status`. If NULL and data_se
#'   contains these columns, they will be used.
#' @param fade_open_cohorts Logical, whether to reduce opacity for open cohorts
#'   (default: TRUE). Requires cohort status data.
#' @param show_phe Logical, whether to show COVID-19 Public Health Emergency
#'   highlighting (default: TRUE).
#' @param phe_label_y Numeric, y-axis position for the PHE label
#'   (default: 1.02).
#' @param show_labels Logical, whether to show percentage labels on SE bars
#'   (default: TRUE).
#' @param show_points Logical, whether to show points on SE bars
#'   (default: TRUE).
#' @param fill_colors Named vector of colors for decision categories. Default
#'   uses cornflowerblue for SE, darkred for NSE, darkgray for Withdrawn.
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
#' The plot displays decision rates using:
#' - Stacked bars showing the proportion of each decision type
#' - Points on the SE (cleared) bars indicating cohort status
#' - Text labels showing the SE percentage
#'
#' The `decision` column should be a factor with levels in the desired stacking
#' order (bottom to top). Typically:
#' - "SE (Cleared)" at the bottom
#' - "NSE (Not Cleared)" in the middle
#' - "Withdrawn or Deleted (Not Cleared)" at the top
#'
#' The function uses `fct_rev()` internally to reverse the legend order so it
#' matches the visual stacking order.
#'
#' @examples
#' \dontrun{
#' # Prepare decision data
#' data_decision <- data |>
#'   mutate(
#'     decision = case_when(
#'       str_detect(metric, "SE") ~ "SE (Cleared)",
#'       str_detect(metric, "NSE") ~ "NSE (Not Cleared)",
#'       TRUE ~ "Withdrawn or Deleted (Not Cleared)"
#'     ) |> as_factor() |> fct_rev()
#'   ) |>
#'   group_by(fy, org) |>
#'   mutate(percent = value / sum(value))
#'
#' # Extract SE data with cohort status
#' data_se <- data_decision |>
#'   filter(str_detect(decision, "^SE")) |>
#'   left_join(cohort_status, by = c("fy", "org"))
#'
#' # Create plot
#' plot_decision_rate(
#'   data_decision,
#'   data_se = data_se,
#'   title = "510(k) Decision Rates"
#' )
#' }
plot_decision_rate <- function(data,
                               data_se = NULL,
                               cohort_status = NULL,
                               fade_open_cohorts = TRUE,
                               show_phe = TRUE,
                               phe_label_y = 1.02,
                               show_labels = TRUE,
                               show_points = TRUE,
                               fill_colors = c(
                                 "Withdrawn or Deleted (Not Cleared)" = "darkgray", # nolint
                                 "NSE (Not Cleared)" = "darkred",
                                 "SE (Cleared)" = "cornflowerblue"
                               ),
                               facet_ncol = 4L,
                               title = NULL,
                               subtitle = NULL,
                               caption = NULL) {
  # Validate inputs
  chk::chk_data(data)
  chk::chk_flag(fade_open_cohorts)
  chk::chk_flag(show_phe)
  chk::chk_flag(show_labels)
  chk::chk_flag(show_points)
  chk::chk_whole_number(facet_ncol)

  # Check required columns in data
  required_cols <- c("fy", "org", "percent", "decision")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    chk::abort_chk(
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  }

  # Default title
  if (is.null(title)) {
    title <- "Decision rates"
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

  # Determine if we can apply alpha fading
  has_cohort_data <- FALSE
  if (!is.null(cohort_status) && "percent_closed" %in% names(cohort_status)) {
    has_cohort_data <- TRUE
    # Join cohort status to data for bar alpha
    data <- data |>
      dplyr::select(-dplyr::any_of(c("percent_closed", "status"))) |>
      dplyr::left_join(
        cohort_status |>
          dplyr::select("fy", "org", "percent_closed", "status"),
        by = c("fy", "org")
      )
  } else if (!is.null(data_se) && "percent_closed" %in% names(data_se)) {
    has_cohort_data <- TRUE
    # Extract cohort status from data_se and join to data
    cohort_from_se <- data_se |>
      dplyr::select("fy", "org", "percent_closed", "status") |>
      dplyr::distinct()
    data <- data |>
      dplyr::select(-dplyr::any_of(c("percent_closed", "status"))) |>
      dplyr::left_join(cohort_from_se, by = c("fy", "org"))
  }

  # Build base plot
  if (fade_open_cohorts && has_cohort_data) {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$percent,
        fill = .data$decision,
        alpha = .data$percent_closed
      )
    )
  } else {
    p <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data$fy,
        y = .data$percent,
        fill = .data$decision
      )
    )
  }

  # Add faceting
  p <- p + facet_by_org(ncol = facet_ncol)

  # Add PHE highlighting if requested (before bars so it's behind)
  if (show_phe) {
    p <- p + geom_phe_rect()
  }

  # Add stacked bars
  p <- p + ggplot2::geom_col(na.rm = TRUE)

  # Add points on SE bars if data_se provided and show_points is TRUE
  if (!is.null(data_se) && show_points) {
    has_status <- "status" %in% names(data_se)
    has_pct_closed <- "percent_closed" %in% names(data_se)
    if (has_status && has_pct_closed && fade_open_cohorts) {
      p <- p +
        ggplot2::geom_point(
          data = data_se,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$percent - 0.02,
            shape = .data$status,
            alpha = .data$percent_closed
          ),
          inherit.aes = FALSE,
          na.rm = TRUE
        )
    } else if ("status" %in% names(data_se)) {
      p <- p +
        ggplot2::geom_point(
          data = data_se,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$percent - 0.02,
            shape = .data$status
          ),
          inherit.aes = FALSE,
          na.rm = TRUE
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = data_se,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$percent - 0.02
          ),
          inherit.aes = FALSE,
          na.rm = TRUE
        )
    }
  }

  # Add text labels on SE bars if data_se provided and show_labels is TRUE
  if (!is.null(data_se) && show_labels) {
    # Create label text (whole number percentages)
    data_se <- data_se |>
      dplyr::mutate(
        label_text = scales::percent(.data$percent, accuracy = 1)
      )

    if ("percent_closed" %in% names(data_se) && fade_open_cohorts) {
      p <- p +
        ggplot2::geom_text(
          data = data_se,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$percent - 0.02,
            label = .data$label_text,
            alpha = .data$percent_closed
          ),
          inherit.aes = FALSE,
          size = 1.75,
          nudge_y = -0.02,
          vjust = 1,
          na.rm = TRUE,
          show.legend = FALSE
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = data_se,
          mapping = ggplot2::aes(
            x = .data$fy,
            y = .data$percent - 0.02,
            label = .data$label_text
          ),
          inherit.aes = FALSE,
          size = 1.75,
          nudge_y = -0.02,
          vjust = 1,
          na.rm = TRUE,
          show.legend = FALSE
        )
    }
  }

  # Add PHE label if requested (after bars so it's on top)
  if (show_phe) {
    p <- p + geom_phe_label(y_text = phe_label_y)
  }

  # Add scales
  p <- p +
    scale_x_fy() +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(NA, 1.15),
      breaks = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    ggplot2::scale_fill_manual(values = fill_colors)

  # Add alpha scale if fading open cohorts
  if (fade_open_cohorts && has_cohort_data) {
    p <- p +
      ggplot2::scale_alpha_continuous(
        range = c(0.3, 1),
        limits = c(0.6, 1),
        oob = scales::squish,
        guide = "none"
      )
  }

  # Include note about fading for open cohorts
  cohort_note <- if (fade_open_cohorts && has_cohort_data) {
    "Fainter values are from cohorts with\nmore open submissions and may change." # nolint
  } else {
    NULL
  }

  # Add shape scale if cohort status is present
  if (!is.null(data_se) && "status" %in% names(data_se) && show_points) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = c("Cohort Closed" = 16, "Cohort Open" = 1),
        drop = FALSE,
        na.translate = FALSE,
        name = cohort_note
      )
  }

  # Add theme
  p <- p + theme_mdufa()

  # Set legend order and hide shape from fill legend
  # Override theme to show shape legend title (cohort note)
  if (!is.null(cohort_note)) {
    p <- p +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          order = 1,
          title = cohort_note,
          title.theme = ggplot2::element_text(size = 8)
        ),
        fill = ggplot2::guide_legend(
          order = 2,
          override.aes = list(shape = NA)
        )
      )
  } else {
    p <- p +
      ggplot2::guides(
        shape = ggplot2::guide_legend(order = 1),
        fill = ggplot2::guide_legend(
          order = 2,
          override.aes = list(shape = NA)
        )
      )
  }

  # Add labels
  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Fiscal Year Received",
      y = "Decision Rates (%)",
      caption = caption,
      fill = NULL,
      shape = NULL
    )

  p
}
