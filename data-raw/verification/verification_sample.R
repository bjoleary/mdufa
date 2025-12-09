# verification_sample.R
# Sampling functions for MDUFA extraction verification
# NOT part of the installed package - development use only

#' Get Unchanged Rows
#'
#' Returns rows that exist in both baseline and comparison with identical values.
#'
#' @param diff_result Diff result from generate_diff()
#' @return Tibble of unchanged rows
get_unchanged_rows <- function(diff_result) {
  key_cols <- c("table_number", "performance_metric", "fy", "value")

  # Unchanged = in comparison but not in new_rows, removed_rows, or updated_rows
  unchanged <- diff_result$comparison |>
    dplyr::anti_join(diff_result$new_rows, by = key_cols) |>
    dplyr::anti_join(diff_result$updated_rows, by = key_cols)

  unchanged
}

#' Stratified Sample
#'
#' Samples rows with stratification by organization and table type.
#'
#' @param data Data to sample from
#' @param n Number of rows to sample
#' @return Sampled tibble
sample_stratified <- function(data, n) {
  if (nrow(data) <= n) {
    return(data)
  }

  # Stratify by organization and table type
  data |>
    dplyr::mutate(
      table_type = substr(.data$table_number, 1, 1),
      stratum = paste(.data$organization, .data$table_type, sep = "_")
    ) |>
    dplyr::group_by(.data$stratum) |>
    dplyr::slice_sample(n = max(1, ceiling(n / dplyr::n_groups(data)))) |>
    dplyr::ungroup() |>
    dplyr::slice_head(n = n) |>
    dplyr::select(-"table_type", -"stratum")
}

#' Generate Verification Sample
#'
#' Creates a sample of unique metrics (not individual FY values).
#' Each sampled metric includes all its FY values for verification.
#' Prioritizes changed rows, filling with unchanged if needed.
#'
#' @param diff_result Result from generate_diff()
#' @param n Target sample size (default 35 unique metrics for 90% LB)
#' @param seed Random seed for reproducibility
#' @return Tibble of sampled rows (one per unique metric) with change_type column
generate_verification_sample <- function(diff_result, n = 35, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Metric key columns (defines a unique metric row in PDF)
  metric_cols <- c("table_number", "organization", "performance_metric")

  # Changed rows first (new + updated), dedupe to unique metrics
  changed <- dplyr::bind_rows(
    diff_result$new_rows |> dplyr::mutate(change_type = "new"),
    diff_result$updated_rows |> dplyr::mutate(change_type = "updated")
  ) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(metric_cols)),
      .keep_all = TRUE
    )

  if (nrow(changed) >= n) {
    message("Sampling ", n, " unique metrics from ", nrow(changed), " changed")
    return(changed |> dplyr::slice_sample(n = n))
  }

  # Need to fill with unchanged rows
  remaining <- n - nrow(changed)
  message(
    "Using all ", nrow(changed), " changed metrics, sampling ",
    remaining, " unchanged metrics"
  )

  # Get unchanged rows, dedupe to unique metrics
  unchanged <- get_unchanged_rows(diff_result) |>
    dplyr::mutate(change_type = "unchanged") |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(metric_cols)),
      .keep_all = TRUE
    ) |>
    # Exclude metrics already in changed
    dplyr::anti_join(changed, by = metric_cols) |>
    sample_stratified(n = remaining)

  result <- dplyr::bind_rows(changed, unchanged)

  message("Sample contains ", nrow(result), " unique metrics")
  result
}

#' Sample for Code Change Verification
#'
#' Creates a prioritized sample for verifying code changes.
#' Includes all removed rows for confirmation (separate from test sample).
#'
#' @param diff_result Result from generate_diff()
#' @param n Target sample size for test cases (default 35 for 90% LB)
#' @param seed Random seed for reproducibility
#' @return List with verification_sample and removed_for_confirmation
sample_for_code_change <- function(diff_result, n = 35, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Priority 1: All removed rows (for confirmation only, no test case generated)
  removed <- diff_result$removed_rows |>
    dplyr::mutate(priority = "removed", generates_test = FALSE)

  # Priority 2: All new rows
  new_rows <- diff_result$new_rows |>
    dplyr::mutate(priority = "new", generates_test = TRUE)

  # Priority 3: All updated rows
  updated_rows <- diff_result$updated_rows |>
    dplyr::mutate(priority = "updated", generates_test = TRUE)

  # Combine changed rows that will generate tests
  changed <- dplyr::bind_rows(new_rows, updated_rows)

  # If changed rows meet sample size, use those
  if (nrow(changed) >= n) {
    sample <- changed |> dplyr::slice_sample(n = n)
  } else {
    # Fill with stratified unchanged rows
    remaining <- n - nrow(changed)
    unchanged <- get_unchanged_rows(diff_result) |>
      dplyr::mutate(priority = "unchanged", generates_test = TRUE) |>
      sample_stratified(n = remaining)
    sample <- dplyr::bind_rows(changed, unchanged)
  }

  message("Sample composition:")
  message("  NEW: ", sum(sample$priority == "new", na.rm = TRUE))
  message("  UPDATED: ", sum(sample$priority == "updated", na.rm = TRUE))
  message("  UNCHANGED: ", sum(sample$priority == "unchanged", na.rm = TRUE))
  message("  REMOVED (for confirmation): ", nrow(removed))

  list(
    verification_sample = sample,
    removed_for_confirmation = removed
  )
}

#' Sample Data for Verification (Stratified)
#'
#' Creates a stratified sample of rows suitable for manual verification.
#' Default n=35 ensures LB of 95% CI > 90% with 100% pass rate.
#'
#' @param data Dataset to sample from
#' @param n Target sample size (default 35 for statistical validity)
#' @param seed Random seed for reproducibility
#' @param include_cdrh If TRUE (default), always include CDRH-level rows
#'
#' @return A tibble of sampled rows with sample_category metadata
sample_for_verification <- function(data,
                                    n = 35,
                                    seed = NULL,
                                    include_cdrh = TRUE) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Allocation strategy (for n=74)
  cdrh_n <- ceiling(n * 0.4) # 40% CDRH (~30)
  oht_n <- ceiling(n * 0.4) # 40% OHT (~30, split between 2)
  edge_n <- n - cdrh_n - oht_n # 20% edge cases (~14)

  samples <- list()

  # 1. CDRH rows (stratified by table type)
  cdrh_data <- data |>
    dplyr::filter(.data$organization == "CDRH" | is.na(.data$organization))

  if (nrow(cdrh_data) > 0 && include_cdrh) {
    # Sample across table types
    cdrh_sample <- cdrh_data |>
      dplyr::mutate(table_type = substr(.data$table_number, 1, 1)) |>
      dplyr::group_by(.data$table_type) |>
      dplyr::slice_sample(n = max(1, ceiling(cdrh_n / 5))) |>
      dplyr::ungroup() |>
      dplyr::slice_head(n = cdrh_n)

    samples$cdrh <- cdrh_sample
  }

  # 2. OHT rows (pick 2 random OHTs)
  oht_orgs <- data |>
    dplyr::filter(stringr::str_detect(.data$organization, "^OHT")) |>
    dplyr::pull(.data$organization) |>
    unique()

  if (length(oht_orgs) >= 2) {
    selected_ohts <- sample(oht_orgs, 2)
    oht_per <- ceiling(oht_n / 2)

    for (oht in selected_ohts) {
      oht_sample <- data |>
        dplyr::filter(.data$organization == oht) |>
        dplyr::slice_sample(n = oht_per)
      samples[[oht]] <- oht_sample
    }
  }

  # 3. Edge cases
  edge_cases <- data |>
    dplyr::filter(
      is.na(.data$value) |
        .data$value == "N/A" |
        stringr::str_detect(.data$value, "^100\\.?0*%?$") |
        (suppressWarnings(as.numeric(gsub("[^0-9.]", "", .data$value))) > 10000)
    ) |>
    dplyr::slice_sample(n = edge_n)

  samples$edge <- edge_cases

  # Combine and add verification metadata
  result <- dplyr::bind_rows(samples) |>
    dplyr::distinct() |>
    dplyr::mutate(
      sample_category = dplyr::case_when(
        .data$organization == "CDRH" | is.na(.data$organization) ~ "cdrh",
        stringr::str_detect(.data$organization, "^OHT") ~ "oht",
        TRUE ~ "edge"
      )
    )

  # Remove temporary columns if they exist
  if ("table_type" %in% names(result)) {
    result <- dplyr::select(result, -"table_type")
  }

  message("Generated sample of ", nrow(result), " rows:")
  message("  CDRH: ", sum(result$sample_category == "cdrh"))
  message("  OHT: ", sum(result$sample_category == "oht"))
  message("  Edge: ", sum(result$sample_category == "edge"))

  result
}
