# Tests for plot_quintile_days function

test_that("plot_quintile_days returns a ggplot object", {
  # Create minimal test data
  test_data <- tibble::tibble(
    fy = rep(2020:2022, each = 5),
    org = "OHT1: Test Organization",
    value = runif(15, 20, 80),
    performance_metric = rep(
      c(
        "20th Percentile", "40th Percentile", "60th Percentile",
        "80th Percentile", "Average"
      ),
      3
    )
  )

  cohort_status <- tibble::tibble(
    fy = 2020:2022,
    org = "OHT1: Test Organization",
    percent_closed = c(1.0, 0.9, 0.5),
    status = c("Cohort Closed", "Cohort Closed", "Cohort Open")
  )

  p <- plot_quintile_days(
    test_data,
    day_type = "fda", cohort_status = cohort_status
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_quintile_days validates day_type", {
  test_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    value = 50,
    performance_metric = "Average"
  )

  expect_error(
    plot_quintile_days(test_data, day_type = "invalid"),
    regexp = "should be one of"
  )
})

test_that("plot_quintile_days requires necessary columns", {
  # Missing 'value' column
  bad_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    performance_metric = "Average"
  )

  expect_error(
    plot_quintile_days(bad_data, day_type = "fda"),
    regexp = "Missing required columns"
  )
})

test_that("plot_quintile_days works with cohort_status", {
  test_data <- tibble::tibble(
    fy = rep(2020:2022, each = 5),
    org = "OHT1: Test Organization",
    value = runif(15, 20, 80),
    performance_metric = rep(
      c(
        "20th Percentile", "40th Percentile", "60th Percentile",
        "80th Percentile", "Average"
      ),
      3
    )
  )

  cohort_status <- tibble::tibble(
    fy = 2020:2022,
    org = "OHT1: Test Organization",
    percent_closed = c(1.0, 0.9, 0.5),
    status = c("Cohort Closed", "Cohort Closed", "Cohort Open")
  )

  p <- plot_quintile_days(
    test_data,
    day_type = "fda",
    cohort_status = cohort_status
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_quintile_days can show PHE highlighting", {
  test_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    value = 50,
    performance_metric = "Average"
  )

  cohort_status <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    percent_closed = 1.0,
    status = "Cohort Closed"
  )

  p_with_phe <- plot_quintile_days(
    test_data,
    day_type = "fda", cohort_status = cohort_status, show_phe = TRUE
  )
  p_without_phe <- plot_quintile_days(
    test_data,
    day_type = "fda", cohort_status = cohort_status, show_phe = FALSE
  )

  # Both should be valid ggplots
  expect_s3_class(p_with_phe, "ggplot")
  expect_s3_class(p_without_phe, "ggplot")

  # The version with PHE should have more layers
  expect_gt(length(p_with_phe$layers), length(p_without_phe$layers))
})

test_that("plot_quintile_days can show shutdown markers", {
  test_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    value = 50,
    performance_metric = "Average"
  )

  cohort_status <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    percent_closed = 1.0,
    status = "Cohort Closed"
  )

  p_with <- plot_quintile_days(
    test_data,
    day_type = "fda",
    cohort_status = cohort_status,
    show_shutdowns = TRUE,
    show_phe = FALSE
  )
  p_without <- plot_quintile_days(
    test_data,
    day_type = "fda",
    cohort_status = cohort_status,
    show_shutdowns = FALSE,
    show_phe = FALSE
  )

  expect_s3_class(p_with, "ggplot")
  expect_s3_class(p_without, "ggplot")
})

test_that("plot_quintile_days respects y_limits", {
  test_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    value = 50,
    performance_metric = "Average"
  )

  cohort_status <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    percent_closed = 1.0,
    status = "Cohort Closed"
  )

  p <- plot_quintile_days(
    test_data,
    day_type = "fda",
    cohort_status = cohort_status,
    y_limits = c(0, 90)
  )

  # Check that the scale has the specified limits
  scale_y <- p$scales$get_scales("y")
  expect_equal(scale_y$limits, c(0, 90))
})

test_that("plot_quintile_days generates appropriate titles", {
  test_data <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    value = 50,
    performance_metric = "Average"
  )

  cohort_status <- tibble::tibble(
    fy = 2020,
    org = "OHT1",
    percent_closed = 1.0,
    status = "Cohort Closed"
  )

  # Default title for FDA days
  p_fda <- plot_quintile_days(
    test_data,
    day_type = "fda", cohort_status = cohort_status
  )
  expect_match(p_fda$labels$title, "FDA Days")

  # Default title for Industry days
  p_industry <- plot_quintile_days(
    test_data,
    day_type = "industry", cohort_status = cohort_status
  )
  expect_match(p_industry$labels$title, "Industry Days")

  # Default title for Total days
  p_total <- plot_quintile_days(
    test_data,
    day_type = "total", cohort_status = cohort_status
  )
  expect_match(p_total$labels$title, "Total Days")

  # Custom title
  p_custom <- plot_quintile_days(
    test_data,
    day_type = "fda",
    cohort_status = cohort_status,
    title = "My Title"
  )
  expect_equal(p_custom$labels$title, "My Title")
})
