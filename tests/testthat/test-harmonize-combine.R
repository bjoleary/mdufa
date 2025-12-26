# Tests for harmonize_metric_names and combine_mdufa_oht functions

test_that("harmonize_metric_names removes asterisks", {
  test_data <- tibble::tibble(
    performance_metric = c("Number Received*", "Number Received**", "Clean")
  )

  result <- harmonize_metric_names(test_data)

  expect_equal(result$metric_harmonized[1], "Number Received")
  expect_equal(result$metric_harmonized[2], "Number Received")
  expect_equal(result$metric_harmonized[3], "Clean")
})

test_that("harmonize_metric_names normalizes MDUFA periods", {
  test_data <- tibble::tibble(
    performance_metric = c(
      "Number with MDUFA II decision",
      "Number with MDUFA III decision",
      "Number with MDUFA IV decision",
      "Number with MDUFA V decision"
    )
  )

  result <- harmonize_metric_names(test_data)

  # All should normalize to "MDUFA Decision" (title case)
  expect_true(all(grepl("MDUFA Decision", result$metric_harmonized)))
  expect_false(any(grepl("MDUFA (II|III|IV|V)", result$metric_harmonized)))
})

test_that("harmonize_metric_names normalizes Panel Track variations", {
  test_data <- tibble::tibble(
    performance_metric = c(
      "PMA Original and Panel Track Supplements",
      "PMA Original and Panel-Track Supplements"
    )
  )

  result <- harmonize_metric_names(test_data)

  # Both should normalize to same value
  expect_equal(result$metric_harmonized[1], result$metric_harmonized[2])
})

test_that("get_harmonized_mapping returns distinct mappings", {
  mapping <- get_harmonized_mapping(mdufa3)

  # Should have both columns
  expect_true("performance_metric" %in% names(mapping))
  expect_true("metric_harmonized" %in% names(mapping))
  expect_true("report_mdufa_period" %in% names(mapping))

  # Should have unique rows
  expect_equal(nrow(mapping), nrow(dplyr::distinct(mapping)))
})

test_that("combine_mdufa_oht returns expected structure", {
  combined <- combine_mdufa_oht()

  # Should have derived column
  expect_true("derived" %in% names(combined))

  # Should have metric_harmonized column
  expect_true("metric_harmonized" %in% names(combined))

  # Should have data from all three periods
  periods <- unique(combined$report_mdufa_period)
  expect_true("MDUFA III" %in% periods)
  expect_true("MDUFA IV" %in% periods)
  expect_true("MDUFA V" %in% periods)
})

test_that("combine_mdufa_oht marks OHT7 as derived", {
  combined <- combine_mdufa_oht()

  # MDUFA III OHT7 rows should be marked as derived
  m3_oht7 <- combined |>
    dplyr::filter(
      report_mdufa_period == "MDUFA III",
      organization == "OHT7"
    )

  if (nrow(m3_oht7) > 0) {
    expect_true(all(m3_oht7$derived))
  }

  # MDUFA IV rows should NOT be derived
  m4 <- combined |>
    dplyr::filter(report_mdufa_period == "MDUFA IV")

  if (nrow(m4) > 0) {
    expect_true(all(!m4$derived))
  }
})

test_that("combine_mdufa_oht filter_to_ohts works", {
  # With filter (default)
  combined_filtered <- combine_mdufa_oht(filter_to_ohts = TRUE)
  orgs_filtered <- unique(combined_filtered$organization)
  expect_true(all(grepl("^OHT", orgs_filtered)))

  # Without filter
  combined_all <- combine_mdufa_oht(filter_to_ohts = FALSE)
  orgs_all <- unique(combined_all$organization)
  # Should have CDRH from MDUFA IV/V
  expect_true("CDRH" %in% orgs_all)
})

test_that("filter_comparable_ohts returns only cross-period OHTs", {
  combined <- combine_mdufa_oht()
  comparable <- filter_comparable_ohts(combined)

  orgs <- unique(comparable$organization)

  # Should have exactly these 5 OHTs
  expect_equal(sort(orgs), c("OHT2", "OHT5", "OHT6", "OHT7", "OHT8"))

  # Should NOT have OHT1, OHT3, OHT4
  expect_false("OHT1" %in% orgs)
  expect_false("OHT3" %in% orgs)
  expect_false("OHT4" %in% orgs)
})

test_that("get_available_fys returns fiscal year ranges", {
  combined <- combine_mdufa_oht()
  fys <- get_available_fys(combined)

  expect_true("report_mdufa_period" %in% names(fys))
  expect_true("min_fy" %in% names(fys))
  expect_true("max_fy" %in% names(fys))
  expect_true("fiscal_years" %in% names(fys))

  # Should have rows for each period
  expect_equal(nrow(fys), 3)
})
