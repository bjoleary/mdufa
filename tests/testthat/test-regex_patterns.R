library(testthat)

test_that("fy_start_line_pattern() returns correct regular expression pattern", { # nolint: line_length_linter.
  # Test that the function returns a regular expression pattern that matches a
  # line that includes the string "FY" followed by a space and four digits,
  # repeated five times
  expect_true(stringr::str_detect("\n FY 2020 FY 2021 FY 2022 FY 2023 FY 2024 \n", # nolint: line_length_linter.
                                  fy_start_line_pattern()))

  # Test that the function returns a regular expression pattern that does not
  # match a line that includes the string "FY" followed by a space and four
  # digits, repeated four times
  expect_false(stringr::str_detect("\n FY 2020 FY 2021 FY 2022 FY 2023 \n",
                                   fy_start_line_pattern()))

  # Test that the function returns a regular expression pattern that does not
  # match a line that includes the string "FY" followed by a space and four
  # digits, repeated six times
  expect_false(stringr::str_detect("\n FY 2020 FY 2021 FY 2022 FY 2023 FY 2024 FY 2025 \n", # nolint: line_length_linter.
                                   fy_start_line_pattern()))
})

test_that("within_days_line_pattern() returns correct regular expression pattern", { # nolint: line_length_linter.
  # Test that the function returns a regular expression pattern that matches a
  # line that includes white space and five copies of the within days pattern,
  # followed by white space and a new line
  expect_true(stringr::str_detect("\n |Within 0|Within 0|Within 0|Within 0|Within 0\n", # nolint: line_length_linter.
                                  within_days_line_pattern()))

  # Test that the function returns a regular expression pattern that does not
  # match a line that includes white space and four copies of the within days
  # pattern, followed by white space and a new line
  expect_false(stringr::str_detect("\n |Within 0|Within 0|Within 0|Within 0\n",
                                   within_days_line_pattern()))
})

test_that("goal_percent_line_pattern() returns correct regular expression pattern", { # nolint: line_length_linter.
  # Test that the function returns a regular expression pattern that matches a
  # line that includes white space and five copies of the goal percent pattern,
  # followed by white space and a new line
  expect_true(stringr::str_detect("\n |50% Complete|50% Complete|50% Complete|50% Complete|50% Complete\n", # nolint: line_length_linter.
                                  goal_percent_line_pattern()))

  # Test that the function returns a regular expression pattern that does not
  # match a line that includes white space and four copies of the goal percent
  # pattern, followed by white space and a new line
  expect_false(stringr::str_detect("\n |50% Complete|50% Complete|50% Complete|50% Complete\n", # nolint: line_length_linter.
                                   goal_percent_line_pattern()))
})

test_that("goal_percent_pattern() returns correct regular expression pattern", {
  # Test that the function returns a regular expression pattern that matches a
  # string that includes a percentage followed by 1 or more words, followed by 2
  # or more digits, not followed by a percent
  expect_true(stringr::str_detect("50% Complete 25", goal_percent_pattern()))

  # Test that the function returns a regular expression pattern that matches a
  # string that includes a percentage followed by 1 or more words
  expect_true(stringr::str_detect("50% Complete", goal_percent_pattern()))
})

test_that("goal_days_pattern() returns correct regular expression pattern", {
  # Test that the function returns a regular expression pattern that matches a
  # string that includes zero or more digits, followed by the word FDA or the
  # word Industry, followed by the word Days
  expect_true(stringr::str_detect("FDA Days", goal_days_pattern()))
  expect_true(stringr::str_detect("Industry Days", goal_days_pattern()))
  expect_true(stringr::str_detect("123 FDA Days", goal_days_pattern()))
  expect_true(stringr::str_detect("123 Industry Days", goal_days_pattern()))
})

test_that("goal_days_line_pattern() returns correct regular expression pattern", { # nolint: line_length_linter.
  # Test that the function returns a regular expression pattern that matches a
  # line that includes white space and five copies of the goal days pattern,
  # followed by white space and a new line
  expect_true(stringr::str_detect("\n |20 Industry Days|21 Industry Days|22 Industry Days|23 Industry Days|24 Industry Days\n", # nolint: line_length_linter.
                                  goal_days_line_pattern()))
})

test_that("si_metric_pattern() matches correct text", {
  expect_true(
    stringr::str_detect(
      string = "\n40th Percentile FDA days to Substantive Interaction|",
      pattern = si_metric_pattern()
    )
  )

  expect_true(
    stringr::str_detect(
      string = "\nMaximum FDA days to\nSubstantive Interaction|",
      pattern = si_metric_pattern()
    )
  )

  expect_true(
    stringr::str_detect(
      string = "\nAverage number of FDA days to\nSubstantive Interaction|",
      pattern = si_metric_pattern()
    )
  )
})
