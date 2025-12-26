#' Rename Division to OHT (1:1 Mappings)
#'
#' Renames divisions that have a direct 1:1 mapping to OHT organizations.
#' DCD -> OHT2, DNPMD -> OHT5, DOD -> OHT6, DRH -> OHT8.
#'
#' @param data MDUFA III dataset
#' @return Dataset with renamed organizations
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa3 |>
#'   filter(organization == "DCD") |>
#'   rename_divisions_to_oht() |>
#'   pull(organization) |>
#'   unique()
rename_divisions_to_oht <- function(data) {
  direct_mappings <- c(
    "DCD" = "OHT2",
    "DNPMD" = "OHT5",
    "DOD" = "OHT6",
    "DRH" = "OHT8"
  )

  data |>
    dplyr::mutate(
      organization = dplyr::if_else(
        .data$organization %in% names(direct_mappings),
        direct_mappings[.data$organization],
        .data$organization
      )
    )
}

#' Identify Metric Aggregation Type
#'
#' Determines how a metric should be aggregated based on its name
#' and metric_type. Returns one of: "sum", "max", "weighted_avg",
#' "recalc_percent", or "exclude".
#'
#' @param performance_metric The metric name
#' @param metric_type The metric type (integer, double, percent, text)
#' @return Character string indicating aggregation type
#' @export
#'
#' @examples
#' identify_aggregation_type("Number Received", "integer")
#' identify_aggregation_type("Average FDA days to MDUFA III decision", "double")
#' identify_aggregation_type("Maximum FDA days to decision", "integer")
#' identify_aggregation_type("20th Percentile FDA days", "integer")
identify_aggregation_type <- function(performance_metric, metric_type) {
  # Text metrics are excluded
  if (metric_type == "text") {
    return("exclude")
  }

  # Percentile metrics cannot be aggregated without raw data
  if (grepl("Percentile", performance_metric, ignore.case = TRUE)) {
    return("exclude")
  }

  # Maximum values: take max across orgs

  if (grepl("^Maximum", performance_metric, ignore.case = TRUE)) {
    return("max")
  }

  # Average/Mean metrics need weighted averaging
  if (grepl("^(Average|Mean)", performance_metric, ignore.case = TRUE)) {
    return("weighted_avg")
  }

  # Percent and Rate metrics need recalculation
  rate_pattern <- "^(Rate of|Current.*Percent)"
  if (metric_type == "percent" ||
    grepl(rate_pattern, performance_metric, ignore.case = TRUE)) {
    return("recalc_percent")
  }

  # Integer counts are summed
  if (metric_type == "integer") {
    return("sum")
  }

  # Default to exclude for safety
  "exclude"
}

#' Aggregate IVD Divisions to OHT7
#'
#' Combines DCTD, DIHD, DMD, DMGP division data into OHT7 organization.
#' Different metrics are aggregated differently:
#' - Count metrics: summed
#' - Average metrics: weighted average by decision count
#' - Percent metrics: recalculated from aggregated counts
#' - Percentile metrics: excluded (cannot be aggregated without raw data)
#'
#' @param data MDUFA III dataset (typically mdufa3)
#' @return Dataset with IVD divisions aggregated to OHT7
#' @export
#'
#' @examples
#' library(dplyr)
#' mdufa3 |>
#'   filter(organization %in% c("DCTD", "DIHD", "DMD", "DMGP")) |>
#'   aggregate_ivd_to_oht7() |>
#'   filter(table_number == "2.1") |>
#'   head()
aggregate_ivd_to_oht7 <- function(data) {
  # Filter to IVD divisions only
  ivd_data <- data |>
    dplyr::filter(.data$organization %in% ivd_divisions)

  if (nrow(ivd_data) == 0) {
    return(dplyr::tibble())
  }

  # Determine aggregation type for each metric
  ivd_data <- ivd_data |>
    dplyr::mutate(
      agg_type = purrr::map2_chr(
        .data$performance_metric,
        .data$metric_type,
        identify_aggregation_type
      ),
      numeric_value = suppressWarnings(as.numeric(.data$value))
    )

  # Get metadata from first division (for non-aggregated columns)
  # Use DCTD as the reference since it's alphabetically first
  metadata <- ivd_data |>
    dplyr::filter(.data$organization == "DCTD") |>
    dplyr::select(
      "table_number", "program", "table_title", "metric_type",
      "performance_metric", "fy", "report_date", "report_description",
      "report_link", "report_mdufa_period"
    ) |>
    dplyr::distinct()

  # Aggregate summable metrics (counts)
  summed <- ivd_data |>
    dplyr::filter(.data$agg_type == "sum", !is.na(.data$numeric_value)) |>
    dplyr::group_by(
      .data$table_number, .data$performance_metric, .data$fy
    ) |>
    dplyr::summarise(
      value = as.character(sum(.data$numeric_value, na.rm = TRUE)),
      agg_type = "sum",
      .groups = "drop"
    )

  # Aggregate max metrics (take max across orgs)
  maxed <- ivd_data |>
    dplyr::filter(.data$agg_type == "max", !is.na(.data$numeric_value)) |>
    dplyr::group_by(
      .data$table_number, .data$performance_metric, .data$fy
    ) |>
    dplyr::summarise(
      value = as.character(max(.data$numeric_value, na.rm = TRUE)),
      agg_type = "max",
      .groups = "drop"
    )

  # Aggregate weighted average metrics
  # First, get the weight values (decision counts)
  weight_lookup <- ivd_data |>
    dplyr::filter(.data$agg_type == "sum", !is.na(.data$numeric_value)) |>
    dplyr::select(
      "organization", "table_number", "fy",
      weight_metric = "performance_metric",
      weight_value = "numeric_value"
    )

  avg_data <- ivd_data |>
    dplyr::filter(
      .data$agg_type == "weighted_avg",
      !is.na(.data$numeric_value)
    ) |>
    dplyr::mutate(
      weight_metric = dplyr::case_when(
        grepl("to MDUFA III decision", .data$performance_metric) ~
          "Number with MDUFA III decision",
        grepl("to MDUFA decision", .data$performance_metric) ~
          "Number with MDUFA decision",
        grepl("review cycles", .data$performance_metric, ignore.case = TRUE) ~
          "Number with MDUFA III decision",
        grepl("to Substantive Interaction", .data$performance_metric) ~
          "Number of Substantive Interactions",
        grepl("to meeting", .data$performance_metric) ~
          "Number with meetings or teleconferences held",
        grepl("missed goal", .data$performance_metric, ignore.case = TRUE) ~
          "Number of submissions that missed the goal",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::left_join(
      weight_lookup,
      by = c("organization", "table_number", "fy", "weight_metric"),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(.data$weight_value))

  weighted <- avg_data |>
    dplyr::group_by(
      .data$table_number, .data$performance_metric, .data$fy
    ) |>
    dplyr::summarise(
      total_weight = sum(.data$weight_value, na.rm = TRUE),
      weighted_sum = sum(
        .data$numeric_value * .data$weight_value,
        na.rm = TRUE
      ),
      agg_type = "weighted_avg",
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # When weight is 0, output "0" to match original PDF format
      value = dplyr::if_else(
        .data$total_weight > 0,
        as.character(round(.data$weighted_sum / .data$total_weight, 2)),
        "0"
      )
    ) |>
    dplyr::select(-"total_weight", -"weighted_sum")

  # Recalculate percent metrics from summed counts
  # TODO: This section requires manual review and verification.
  # Is number of MDUFA III decisions the right denominator for all of these, or
  # should it sometimes be number received? Careful review is required.
  pct_definitions <- ivd_data |>
    dplyr::filter(.data$agg_type == "recalc_percent") |>
    dplyr::select("table_number", "performance_metric", "fy") |>
    dplyr::distinct() |>
    dplyr::mutate(
      numerator_metric = dplyr::case_when(
        grepl("Performance Percent.*Goal Met", .data$performance_metric) ~
          "MDUFA III Decisions Goal Met",
        grepl("Performance Percent.*90 FDA Days", .data$performance_metric) ~
          "MDUFA III Decisions within 90 FDA Days",
        grepl("SI Performance Percent.*Goal Met", .data$performance_metric) ~
          "SI Goal Met",
        grepl("SI Performance Percent.*60 FDA days", .data$performance_metric) ~
          "SI within 60 FDA days",
        grepl("Rate of.*SE decisions", .data$performance_metric) ~
          "Number of SE decisions",
        grepl("Rate of.*NSE decisions", .data$performance_metric) ~
          "Number of NSE decisions",
        grepl("Rate of.*Withdrawals", .data$performance_metric) ~
          "Number of Withdrawals",
        grepl("Rate of.*Deleted", .data$performance_metric) ~
          "Number deleted",
        grepl("Rate of.*Not Approvable", .data$performance_metric) ~
          "Number of Not Approvable",
        grepl("Rate of.*Not Filed", .data$performance_metric) ~
          "Number Not Filed",
        grepl("Rate of.*not accepted", .data$performance_metric) ~
          "Number Not Accepted for Filing Review",
        TRUE ~ NA_character_
      ),
      denominator_metric = dplyr::case_when(
        grepl("Performance Percent", .data$performance_metric) ~
          "Number with MDUFA III decision",
        grepl("SI Performance Percent", .data$performance_metric) ~
          "Eligible for SI",
        grepl(
          "Rate of.*(SE|NSE|Withdrawal|Deleted|Not Approvable)",
          .data$performance_metric
        ) ~
          "MDUFA III Decisions",
        grepl("Rate of.*Not Filed", .data$performance_metric) ~
          "Number Filed",
        grepl("Rate of.*not accepted", .data$performance_metric) ~
          "Number Received",
        TRUE ~ NA_character_
      )
    )

  # Join numerator and denominator values from summed data
  summed_lookup <- summed |>
    dplyr::select("table_number", "fy",
      lookup_metric = "performance_metric",
      lookup_value = "value"
    ) |>
    dplyr::mutate(lookup_value = as.numeric(.data$lookup_value))

  percents <- pct_definitions |>
    dplyr::left_join(
      summed_lookup |>
        dplyr::rename(numerator_val = "lookup_value"),
      by = c("table_number", "fy", "numerator_metric" = "lookup_metric")
    ) |>
    dplyr::left_join(
      summed_lookup |>
        dplyr::rename(denominator_val = "lookup_value"),
      by = c("table_number", "fy", "denominator_metric" = "lookup_metric")
    ) |>
    dplyr::filter(
      !is.na(.data$numerator_val),
      !is.na(.data$denominator_val),
      .data$denominator_val > 0
    ) |>
    dplyr::mutate(
      value = as.character(
        round(.data$numerator_val / .data$denominator_val * 100, 1)
      ),
      agg_type = "recalc_percent"
    ) |>
    dplyr::select(
      "table_number", "performance_metric", "fy", "value", "agg_type"
    )

  # Combine all aggregated values
  all_values <- dplyr::bind_rows(summed, maxed, weighted, percents)

  # Join with metadata to get full row structure
  result <- all_values |>
    dplyr::left_join(
      metadata,
      by = c("table_number", "performance_metric", "fy"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      organization = "OHT7",
      # Create source string indicating aggregation
      source = paste0(
        "(Aggregated) Table ", .data$table_number,
        ". OHT7 - In Vitro Diagnostics - ",
        .data$table_title
      ),
      page = NA_character_
    ) |>
    dplyr::select(
      "source", "page", "table_number", "organization", "program",
      "table_title", "metric_type", "performance_metric", "fy", "value",
      "report_date", "report_description", "report_link", "report_mdufa_period"
    )

  result
}

#' Create MDUFA III OHT Dataset
#'
#' Creates a dataset with MDUFA III data reorganized at the OHT level.
#' - Direct 1:1 mapped divisions (DCD, DNPMD, DOD, DRH) are renamed
#' - IVD divisions (DCTD, DIHD, DMD, DMGP) are aggregated to OHT7
#' - Excluded divisions (DAGRID, DOED, DSD, DRGUD) are omitted
#' - Center (CDRH, CBER) and office (ODE, OIR) level data is omitted
#'
#' @param data MDUFA III dataset (defaults to mdufa3)
#' @return Dataset with OHT-level organizations and a `derived` column
#'   indicating whether the row was aggregated (TRUE for OHT7) or
#'   directly renamed (FALSE for OHT2/5/6/8)
#' @export
#'
#' @examples
#' oht_data <- create_mdufa3_oht()
#' unique(oht_data$organization)
#' table(oht_data$organization, oht_data$derived)
create_mdufa3_oht <- function(data = NULL) {
  if (is.null(data)) {
    data <- mdufa3
  }

  # Get 1:1 mapped divisions and rename (not derived)
  direct_mapped <- data |>
    dplyr::filter(.data$organization %in% c("DCD", "DNPMD", "DOD", "DRH")) |>
    rename_divisions_to_oht() |>
    dplyr::mutate(derived = FALSE)

  # Aggregate IVD divisions to OHT7 (derived)
  oht7_aggregated <- aggregate_ivd_to_oht7(data) |>
    dplyr::mutate(derived = TRUE)

  # Combine
  dplyr::bind_rows(direct_mapped, oht7_aggregated) |>
    dplyr::arrange(.data$table_number, .data$organization, .data$fy)
}
