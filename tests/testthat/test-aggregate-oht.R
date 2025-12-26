# Tests for MDUFA III OHT reorganization functions

test_that("identify_aggregation_type correctly classifies metrics", {
  # Sum (integer counts)
  expect_equal(identify_aggregation_type("Number Received", "integer"), "sum")
  expect_equal(
    identify_aggregation_type("Number with MDUFA III decision", "integer"),
    "sum"
  )

  # Weighted average (double averages)
  expect_equal(
    identify_aggregation_type(
      "Average FDA days to MDUFA III decision", "double"
    ),
    "weighted_avg"
  )
  expect_equal(
    identify_aggregation_type("Average review cycles", "double"),
    "weighted_avg"
  )
  expect_equal(
    identify_aggregation_type(
      "Mean FDA days for submissions that missed goal", "double"
    ),
    "weighted_avg"
  )

  # Recalculated percent
  expect_equal(
    identify_aggregation_type(
      "Current Performance Percent Goal Met", "percent"
    ),
    "recalc_percent"
  )
  expect_equal(
    identify_aggregation_type("Rate of SE decisions", "percent"),
    "recalc_percent"
  )

  # Percentile (excluded - cannot aggregate without raw data)
  expect_equal(
    identify_aggregation_type(
      "20th Percentile FDA days to MDUFA III decision", "integer"
    ),
    "exclude"
  )
  expect_equal(
    identify_aggregation_type("80th Percentile Total days", "integer"),
    "exclude"
  )

  # Maximum (take max across orgs)
  expect_equal(
    identify_aggregation_type(
      "Maximum FDA days to MDUFA III decision", "integer"
    ),
    "max"
  )

  # Text (excluded)
  expect_equal(
    identify_aggregation_type("Performance Metric", "text"),
    "exclude"
  )
})

test_that("rename_divisions_to_oht correctly renames 1:1 divisions", {
  test_data <- tibble::tibble(
    organization = c("DCD", "DNPMD", "DOD", "DRH", "CDRH", "OHT1"),
    value = 1:6
  )

  result <- rename_divisions_to_oht(test_data)

  # Use unname() to remove any names attribute from the result
  expect_equal(unname(result$organization[1]), "OHT2")
  expect_equal(unname(result$organization[2]), "OHT5")
  expect_equal(unname(result$organization[3]), "OHT6")
  expect_equal(unname(result$organization[4]), "OHT8")
  # Non-mapped should remain unchanged
  expect_equal(unname(result$organization[5]), "CDRH")
  expect_equal(unname(result$organization[6]), "OHT1")
})

test_that("create_mdufa3_oht produces expected organizations", {
  oht_data <- create_mdufa3_oht(mdufa3)

  orgs <- unique(oht_data$organization)

  # Should have exactly these 5 OHTs
  expect_true("OHT2" %in% orgs)
  expect_true("OHT5" %in% orgs)
  expect_true("OHT6" %in% orgs)
  expect_true("OHT7" %in% orgs)
  expect_true("OHT8" %in% orgs)

  # Should NOT have original division names
  expect_false("DCD" %in% orgs)
  expect_false("DCTD" %in% orgs)

  # Should NOT have center/office level
  expect_false("CDRH" %in% orgs)
  expect_false("ODE" %in% orgs)
})

test_that("mdufa3_oht bundled data has correct structure", {
  # Check expected columns
  expected_cols <- c(
    "source", "page", "table_number", "organization", "program",
    "table_title", "metric_type", "performance_metric", "fy", "value",
    "report_date", "report_description", "report_link", "report_mdufa_period"
  )
  expect_true(all(expected_cols %in% names(mdufa3_oht)))

  # Check organizations
  orgs <- unique(mdufa3_oht$organization)
  expect_equal(sort(orgs), c("OHT2", "OHT5", "OHT6", "OHT7", "OHT8"))

  # Check that OHT7 has fewer rows than 1:1 mapped OHTs
  # (percentiles are excluded for OHT7)
  oht7_rows <- sum(mdufa3_oht$organization == "OHT7")
  oht2_rows <- sum(mdufa3_oht$organization == "OHT2")
  expect_true(oht7_rows < oht2_rows)
})

test_that("OHT7 aggregation sums counts correctly", {
  # Get IVD divisions from mdufa3 (table 1.1 PMA has Number Received)
  ivd_data <- mdufa3 |>
    dplyr::filter(
      organization %in% c("DCTD", "DIHD", "DMD", "DMGP"),
      table_number == "1.1",
      performance_metric == "Number Received",
      fy == "2013"
    )

  expected_sum <- sum(as.numeric(ivd_data$value), na.rm = TRUE)

  # Get OHT7 value
  oht7_value <- mdufa3_oht |>
    dplyr::filter(
      organization == "OHT7",
      table_number == "1.1",
      performance_metric == "Number Received",
      fy == "2013"
    ) |>
    dplyr::pull(value) |>
    as.numeric()

  expect_equal(oht7_value, expected_sum)
})

test_that("OHT7 weighted averages are calculated correctly", {
  # Test weighted average calculation
  # Get IVD divisions data for Average FDA days (table 1.7)
  ivd_data <- mdufa3 |>
    dplyr::filter(
      organization %in% c("DCTD", "DIHD", "DMD", "DMGP"),
      table_number == "1.7",
      fy == "2013"
    )

  avg_fda_days <- ivd_data |>
    dplyr::filter(
      performance_metric == "Average FDA days to MDUFA III decision"
    )

  n_decisions <- ivd_data |>
    dplyr::filter(performance_metric == "Number with MDUFA III decision")

  # Only run test if we have data
  skip_if(
    nrow(avg_fda_days) == 0 || nrow(n_decisions) == 0,
    "No weighted average test data available"
  )

  # Calculate expected weighted average
  merged <- dplyr::inner_join(
    avg_fda_days |>
      dplyr::select(organization, avg = value),
    n_decisions |>
      dplyr::select(organization, n = value),
    by = "organization"
  ) |>
    dplyr::mutate(
      avg = as.numeric(avg),
      n = as.numeric(n)
    ) |>
    dplyr::filter(!is.na(avg), !is.na(n), n > 0)

  skip_if(nrow(merged) == 0, "No valid merged data for weighted average")

  expected_wavg <- round(
    sum(merged$avg * merged$n) / sum(merged$n),
    2
  )

  oht7_wavg <- mdufa3_oht |>
    dplyr::filter(
      organization == "OHT7",
      table_number == "1.7",
      performance_metric == "Average FDA days to MDUFA III decision",
      fy == "2013"
    ) |>
    dplyr::pull(value) |>
    as.numeric()

  expect_equal(oht7_wavg, expected_wavg)
})

test_that("OHT7 excludes percentile metrics", {
  oht7_metrics <- mdufa3_oht |>
    dplyr::filter(organization == "OHT7") |>
    dplyr::pull(performance_metric) |>
    unique()

  # Should not contain any percentile metrics
  percentile_metrics <- oht7_metrics[grepl("Percentile", oht7_metrics)]
  expect_equal(length(percentile_metrics), 0)
})
