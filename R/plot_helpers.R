#' Add Public Health Emergency Background Rectangle
#'
#' Adds a light gray background rectangle to highlight the COVID-19
#' Public Health Emergency period (FY2020-FY2023) on a plot with
#' fiscal year on the x-axis.
#'
#' The PHE period ran from January 27, 2020 to May 11, 2023, which
#' spans fiscal years 2020 through 2023.
#'
#' @return A ggplot2 geom_rect layer
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_phe_rect() +
#'   geom_line()
#' }
geom_phe_rect <- function() {
  ggplot2::geom_rect(
    xmin = 2019.5,
    xmax = 2023.5,
    ymin = -Inf,
    ymax = Inf,
    color = NA,
    fill = "whitesmoke",
    inherit.aes = FALSE,
    show.legend = FALSE
  )
}

#' Add Public Health Emergency Label
#'
#' Adds a text annotation labeling the COVID-19 Public Health Emergency
#' period on a plot. Use with [geom_phe_rect()] to highlight the PHE period.
#'
#' @param y_text The y-coordinate for the label text. Set to a value
#'   appropriate for your plot's y-axis scale.
#'
#' @return A ggplot2 annotate layer
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_phe_rect() +
#'   geom_phe_label(y_text = 50) +
#'   geom_line()
#' }
geom_phe_label <- function(y_text = 0) {
  ggplot2::annotate(
    geom = "text",
    x = 2019.5 + (2023.5 - 2019.5) / 2,
    y = y_text,
    label = "COVID-19\nPublic Health\nEmergency",
    color = "black",
    vjust = 0,
    size = 2.5
  )
}

#' Convert Fiscal Year to Decimal for Plotting
#'
#' Converts a date to its fiscal year representation as a decimal value
#' suitable for plotting. The federal fiscal year runs from October 1
#' to September 30.
#'
#' @param date A Date object or something coercible to Date
#'
#' @return Numeric fiscal year with decimal portion representing
#'   position within the fiscal year
#' @export
#'
#' @examples
#' # January 27, 2020 is in FY2020
#' date_to_fy_decimal(as.Date("2020-01-27"))
#'
#' # October 1, 2019 is the start of FY2020
#' date_to_fy_decimal(as.Date("2019-10-01"))
date_to_fy_decimal <- function(date) {
  date <- as.Date(date)

  # Federal fiscal year starts October 1
  # FY2020 runs from Oct 1, 2019 to Sep 30, 2020
  cal_year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))

  # Fiscal year is calendar year + 1 for Oct-Dec

  fy <- dplyr::if_else(month >= 10, cal_year + 1L, cal_year)

  # Day of fiscal year (Oct 1 = day 1)
  fy_start <- as.Date(paste0(fy - 1, "-10-01"))
  day_of_fy <- as.integer(difftime(date, fy_start, units = "days"))

  # Convert to decimal (365 days per FY, ignoring leap years for simplicity)
  fy + day_of_fy / 365
}

#' MDUFA Plot Theme
#'
#' A clean theme for MDUFA performance metric plots based on
#' [ggplot2::theme_classic()] with legend positioned at bottom and
#' no legend title.
#'
#' @param base_size Base font size (default: 11)
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   theme_mdufa()
#' }
theme_mdufa <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Facet by Organization
#'
#' Creates a facet wrap by organization with wrapped labels for
#' readability. Designed for MDUFA performance metric plots that

#' show data across multiple FDA review organizations (OHTs).
#'
#' @param ncol Number of columns in facet grid (default: 4)
#' @param wrap_width Maximum width of organization labels before
#'   wrapping (default: 40 characters)
#'
#' @return A ggplot2 facet_wrap object
#' @export
#'
#' @details
#' The function expects a column named `org` in the data containing
#' organization names. Long organization names are wrapped at the
#' specified width for better display in facet labels.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   facet_by_org() +
#'   theme_mdufa()
#' }
facet_by_org <- function(ncol = 4L, wrap_width = 40) {
  ggplot2::facet_wrap(
    ~ .data$org |> stringr::str_wrap(width = wrap_width),
    ncol = ncol
  )
}

#' Y-Axis Scale for Days Metrics
#'
#' Creates a y-axis scale appropriate for FDA/Industry/Total days
#' metrics with breaks at regulatory milestones.
#'
#' @param day_type Type of days metric: "fda", "industry", or "total"
#' @param limits Optional y-axis limits as a length-2 numeric vector.
#'   If NULL, uses c(0, NA) for automatic upper limit.
#'
#' @return A ggplot2 scale_y_continuous object
#' @export
#'
#' @details
#' Break points vary by day type:
#' - FDA days: 0, 15, 30, 45, 60, 75, 90, 180, 270
#' - Industry days: 0, 60, 120, 180, 240, 300, 360
#' - Total days: 0, 30, 90, 180, 270, 365
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   scale_y_days("fda") +
#'   theme_mdufa()
#'
#' # With clipped upper limit
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   scale_y_days("fda", limits = c(0, 90)) +
#'   theme_mdufa()
#' }
scale_y_days <- function(day_type = c("fda", "industry", "total"),
                         limits = NULL) {
  day_type <- match.arg(day_type)

  breaks <- switch(day_type,
    fda = c(0, 15, 30, 45, 60, 75, 90, 180, 270),
    industry = c(0, 60, 120, 180, 240, 300, 360),
    total = c(0, 30, 90, 180, 270, 365)
  )

  if (is.null(limits)) {
    limits <- c(0, NA)
  }

  ggplot2::scale_y_continuous(
    breaks = breaks,
    limits = limits
  )
}


#' X-Axis Scale for Fiscal Year with MDUFA Period Breaks
#'
#' Creates an x-axis scale for fiscal year data with breaks at the
#' first year of each MDUFA period for consistent labeling across facets.
#' The lower limit is determined automatically from the data, while the
#' upper limit extends to FY 2028 to include future years of MDUFA V.
#'
#' @return A ggplot2 scale_x_continuous object
#' @export
#'
#' @details
#' Break points are at the start of each MDUFA period:
#' - MDUFA III: FY 2013-2017
#' - MDUFA IV: FY 2018-2022
#' - MDUFA V: FY 2023-2027
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   scale_x_fy() +
#'   theme_mdufa()
#' }
scale_x_fy <- function() {
  ggplot2::scale_x_continuous(
    breaks = c(2013, 2018, 2023),
    limits = c(NA, 2028)
  )
}


#' Extract Percentile Labels from Metric Names
#'
#' Extracts percentile labels (e.g., "20th Percentile", "Average") from
#' performance metric names for use in plotting legends.
#'
#' @param .data A data frame with performance metric names
#' @param col Name of the column containing metric names
#'   (default: "performance_metric")
#'
#' @return Data frame with the specified column modified to contain
#'   only the percentile label
#' @export
#'
#' @details
#' Extracts patterns like "20th", "40th", "60th", "80th" and converts
#' them to "20th Percentile", etc. Also preserves "Average" labels.
#'
#' @examples
#' \dontrun{
#' data |>
#'   dplyr::filter(
#'     stringr::str_detect(performance_metric, "Percentile|Average")
#'   ) |>
#'   extract_percentiles()
#' }
extract_percentiles <- function(.data, col = "performance_metric") {
  dplyr::mutate(
    .data = .data,
    "{col}" :=
      stringr::str_extract(
        string = .data[[col]],
        pattern = stringr::regex("(\\d{2}th)|Average")
      ) |>
      stringr::str_replace_all(
        pattern = "th$",
        replacement = "th Percentile"
      )
  )
}

#' Government Shutdown Data
#'
#' A tibble of U.S. federal government shutdowns that may have impacted
#' FDA device review timelines during MDUFA periods.
#'
#' @format A tibble with 4 rows and 6 columns:
#' \describe{
#'   \item{name}{Short identifier for the shutdown (e.g., "fy2014")}
#'   \item{start_date}{Start date of shutdown}
#'   \item{end_date}{End date of shutdown}
#'   \item{duration_days}{Duration in calendar days}
#'   \item{fy}{Fiscal year affected}
#'   \item{fy_position}{Numeric position on FY x-axis for plotting}
#' }
#'
#' @details
#' Notable shutdowns during MDUFA periods:
#' - FY 2014 (Oct 1-17, 2013): 16 days - affected FY 2014 cohort
#' - FY 2018 (Jan 20-23, 2018): 3 days - minimal impact
#' - FY 2019 (Dec 22, 2018 - Jan 25, 2019): 35 days - border wall dispute
#' - FY 2026 (Oct 1 - Nov 12, 2025): 43 days - longest in US history
#'
#' @export
gov_shutdowns <- tibble::tibble(
  name = c("fy2014", "fy2018_jan", "fy2019", "fy2026"),
  start_date = as.Date(
    c("2013-10-01", "2018-01-20", "2018-12-22", "2025-10-01")
  ),
  end_date = as.Date(c("2013-10-17", "2018-01-23", "2019-01-25", "2025-11-12")),
  duration_days = c(16L, 3L, 35L, 43L),
  fy = c(2014L, 2018L, 2019L, 2026L),
  # Decimal FY position based on shutdown midpoint
  fy_position = c(2014.022, 2018.310, 2019.271, 2026.058)
)

#' Add Government Shutdown Markers
#'
#' Adds square markers on plots to indicate fiscal years affected by
#' government shutdowns that may have impacted FDA review timelines.
#'
#' @param shutdowns Shutdown data tibble (default: [gov_shutdowns]).
#'   Can be filtered to show only significant shutdowns
#'   (e.g., `gov_shutdowns |> dplyr::filter(duration_days >= 10)`).
#' @param y_position Y-axis position for markers
#' @param color Color for shutdown markers (default: "gray40")
#' @param size Size of markers (default: 2). Ignored if `scale_by_duration`
#'   is TRUE.
#' @param scale_by_duration Logical, whether to scale marker size by shutdown
#'   duration (default: FALSE). When TRUE, longer shutdowns have larger markers.
#'
#' @return A list of ggplot2 layers
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Add all shutdown markers at y = 0
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   annotate_shutdowns(y_position = 0)
#'
#' # Scale markers by shutdown duration
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   annotate_shutdowns(y_position = 0, scale_by_duration = TRUE)
#'
#' # Show only significant shutdowns (>= 10 days)
#' ggplot(data, aes(x = fy, y = value)) +
#'   geom_point() +
#'   annotate_shutdowns(
#'     shutdowns = gov_shutdowns |> dplyr::filter(duration_days >= 10),
#'     y_position = 0
#'   )
#' }
annotate_shutdowns <- function(shutdowns = NULL,
                               y_position = 0,
                               color = "gray40",
                               size = 2,
                               scale_by_duration = FALSE) {
  if (is.null(shutdowns)) {
    shutdowns <- gov_shutdowns
  }


  # Use diamond shape (18) for shutdown markers

  # Note: Do not add scale_shape_manual here - it conflicts with
  # cohort status shapes

  # The calling function should handle combining shape scales
  if (scale_by_duration) {
    list(
      ggplot2::geom_point(
        data = shutdowns,
        mapping = ggplot2::aes(
          x = .data$fy_position,
          y = y_position,
          size = .data$duration_days,
          shape = "Government Shutdown"
        ),
        color = color,
        inherit.aes = FALSE
      ),
      ggplot2::scale_size_continuous(
        range = c(2, 4),
        guide = "none"
      )
    )
  } else {
    list(
      ggplot2::geom_point(
        data = shutdowns,
        mapping = ggplot2::aes(
          x = .data$fy_position,
          y = y_position,
          shape = "Government Shutdown"
        ),
        color = color,
        size = size,
        inherit.aes = FALSE
      )
    )
  }
}


#' Validate Cohort Status Matches Data Report Dates
#'
#' Checks that cohort status data comes from the same reports as the
#' performance metrics data. This ensures that cohort closure percentages
#' accurately reflect the state of submissions at the time the performance
#' metrics were measured.
#'
#' @param data Performance metrics data frame. Must contain `report_date`,
#'   `fy`, and `org` columns.
#' @param cohort_status Cohort status data frame. Must contain `report_date`,
#'   `fy`, and `org` columns.
#'
#' @return Invisibly returns TRUE if validation passes. Issues a warning
#'   if mismatches are detected.
#'
#' @details
#' For each unique combination of (fy, org) in the data, this function

#' verifies that the most recent report_date in data matches the report_date
#' in cohort_status. Mismatches indicate that the cohort closure status
#' may not accurately represent the state of submissions when the performance
#' metrics were calculated.
#'
#' @keywords internal
validate_cohort_status_dates <- function(data, cohort_status) {
  # Check required columns
  if (!all(c("report_date", "fy", "org") %in% names(data))) {
    return(invisible(TRUE)) # Can't validate without these columns
  }
  if (!all(c("report_date", "fy", "org") %in% names(cohort_status))) {
    return(invisible(TRUE)) # Can't validate without these columns
  }


  # Get most recent report_date for each fy/org in data
  data_dates <- data |>
    dplyr::group_by(.data$fy, .data$org) |>
    dplyr::summarize(
      data_report_date = max(.data$report_date, na.rm = TRUE),
      .groups = "drop"
    )

  # Join with cohort_status
  comparison <- dplyr::left_join(
    data_dates,
    cohort_status |>
      dplyr::select("fy", "org", cohort_report_date = "report_date"),
    by = c("fy", "org")
  )

  # Find mismatches
  mismatches <- comparison |>
    dplyr::filter(
      !is.na(.data$cohort_report_date),
      .data$data_report_date != .data$cohort_report_date
    )

  if (nrow(mismatches) > 0) {
    warning(
      "Cohort status report dates do not match performance data report dates ",
      "for ", nrow(mismatches), " fy/org combinations. ",
      "This may result in incorrect cohort status styling. ",
      "Consider filtering cohort_status to match your data's report dates.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
