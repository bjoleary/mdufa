test_that("only valid metric_types are accepted", {
  # Make a random lower-case string that is not a valid metric type
  metric_type <- "integer"
  while (metric_type %in% c("integer", "double", "percent", "text")) {
    metric_type <-
      sample(letters, size = sample(1:20, 1)) |>
      paste(collapse = "")
  }
  expect_error(
    object =
      mdufa::quarterly_performance |>
        filter_metrics(metric_type = metric_type),
    regexp = "^metric_type"
  )
})

test_that("filter_metric works", {
  test_cases <-
    tibble::tribble(
      ~metric_type, ~value,
      "percent", "92.13%",
      "integer", "42",
      "text", "Life, the universe, and everything",
      "double", "98.6"
    )
  expect_equal(
    object =
      test_cases |>
        filter_metrics("percent"),
    expected =
      tibble::tribble(
        ~metric_type, ~value,
        "percent", 0.9213
      )
  )
  expect_equal(
    object =
      test_cases |>
        filter_metrics("integer"),
    expected =
      tibble::tribble(
        ~metric_type, ~value,
        "integer", 42L
      )
  )
  expect_equal(
    object =
      test_cases |>
        filter_metrics("text"),
    expected =
      tibble::tribble(
        ~metric_type, ~value,
        "text", "Life, the universe, and everything"
      )
  )
  expect_equal(
    object =
      test_cases |>
        filter_metrics("double"),
    expected =
      tibble::tribble(
        ~metric_type, ~value,
        "double", 98.6
      )
  )
})
