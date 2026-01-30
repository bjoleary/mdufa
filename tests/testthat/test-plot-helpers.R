# Tests for plot helper functions

test_that("theme_mdufa returns a ggplot2 theme", {
  theme <- theme_mdufa()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")
})
test_that("theme_mdufa sets legend position to bottom", {
  theme <- theme_mdufa()
  expect_equal(theme$legend.position, "bottom")
})

test_that("facet_by_org returns a facet specification", {
  facet <- facet_by_org()
  expect_s3_class(facet, "FacetWrap")
})

test_that("facet_by_org respects ncol parameter", {
  facet <- facet_by_org(ncol = 2L)
  expect_equal(facet$params$ncol, 2L)
})

test_that("scale_y_days returns a scale object", {
  scale <- scale_y_days("fda")
  expect_s3_class(scale, "Scale")
})

test_that("scale_y_days validates day_type", {
  expect_error(scale_y_days("invalid"))
})

test_that("scale_y_days sets correct breaks for fda", {
  scale <- scale_y_days("fda")
  expect_equal(scale$breaks, c(0, 15, 30, 45, 60, 75, 90, 180, 270))
})

test_that("scale_y_days sets correct breaks for industry", {
  scale <- scale_y_days("industry")
  expect_equal(scale$breaks, c(0, 60, 120, 180, 240, 300, 360))
})

test_that("scale_y_days sets correct breaks for total", {
  scale <- scale_y_days("total")
  expect_equal(scale$breaks, c(0, 30, 90, 180, 270, 365))
})

test_that("scale_y_days respects limits parameter", {
  scale <- scale_y_days("fda", limits = c(0, 90))
  expect_equal(scale$limits, c(0, 90))
})

test_that("extract_percentiles extracts percentile labels", {
  data <- tibble::tibble(
    performance_metric = c(
      "20th Percentile FDA Days to Decision",
      "40th Percentile FDA Days to Decision",
      "Average FDA Days to Decision"
    )
  )
  result <- extract_percentiles(data)
  expect_equal(
    result$performance_metric,
    c("20th Percentile", "40th Percentile", "Average")
  )
})

test_that("gov_shutdowns has expected structure", {
  expect_s3_class(gov_shutdowns, "tbl_df")
  expected_cols <- c(
    "name", "start_date", "end_date", "duration_days", "fy", "fy_position"
  )
  expect_true(all(expected_cols %in% names(gov_shutdowns)))
})

test_that("gov_shutdowns includes expected shutdowns", {
  expect_true("fy2014" %in% gov_shutdowns$name)
  expect_true("fy2019" %in% gov_shutdowns$name)
  expect_true("fy2026" %in% gov_shutdowns$name)
})

test_that("gov_shutdowns has correct durations", {
  fy2014 <- gov_shutdowns |> dplyr::filter(.data$name == "fy2014")
  expect_equal(fy2014$duration_days, 16L)

  fy2019 <- gov_shutdowns |> dplyr::filter(.data$name == "fy2019")
  expect_equal(fy2019$duration_days, 35L)

  fy2026 <- gov_shutdowns |> dplyr::filter(.data$name == "fy2026")
  expect_equal(fy2026$duration_days, 43L)
})

test_that("annotate_shutdowns returns a list of layers", {
  layers <- annotate_shutdowns(y_position = 0)
  expect_type(layers, "list")
  expect_true(length(layers) >= 1)
})

test_that("annotate_shutdowns uses default gov_shutdowns", {
  layers <- annotate_shutdowns(y_position = 0)
  # Check that the layer data has expected number of rows
  expect_equal(nrow(layers[[1]]$data), nrow(gov_shutdowns))
})

test_that("annotate_shutdowns can filter to significant shutdowns", {
  significant <- gov_shutdowns |> dplyr::filter(.data$duration_days >= 10)
  layers <- annotate_shutdowns(shutdowns = significant, y_position = 0)
  expect_equal(nrow(layers[[1]]$data), nrow(significant))
})

test_that("geom_phe_rect returns a ggplot2 layer", {
  layer <- geom_phe_rect()
  expect_s3_class(layer, "LayerInstance")
})

test_that("geom_phe_label returns a ggplot2 layer", {
  layer <- geom_phe_label(y_text = 50)
  expect_s3_class(layer, "LayerInstance")
})
