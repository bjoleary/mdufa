test_that("is_footnote_row catches Subs in the MDUFA Cohort wrap text", {
  # Wrapped text from Table 9.2 footnotes 1 and 2 that previously slipped
  # through the filter and was captured as a CDRH/CBER text metric.
  expect_true(is_footnote_row(
    "Subs in the MDUFA Cohort if the MDUFA Cohort is 3585 or more.",
    NA_character_
  ))
  expect_true(is_footnote_row(
    "Subs in the MDUFA Cohort if the MDUFA Cohort is 4060 or more.",
    NA_character_
  ))
})

test_that("is_footnote_row respects the is.na(value) guard", {
  # metric_types.R classifies these strings as "text" metrics; if a real value
  # ever shows up, the row is data, not a footnote. Do not drop those rows.
  expect_false(is_footnote_row(
    "Subs in the MDUFA Cohort if the MDUFA Cohort is 3585 or more.",
    "3,200"
  ))
})

test_that("is_footnote_row still catches existing footnote prefixes", {
  expect_true(is_footnote_row("1 If FDA's review clock pauses", NA_character_))
  expect_true(is_footnote_row("FYs 2023 and 2024 cohort", NA_character_))
  expect_true(is_footnote_row("In FY 2024, the MDUFA goal", NA_character_))
  expect_true(is_footnote_row(
    "Subs in FYs 2023 and 2024 reflect", NA_character_
  ))
  expect_true(is_footnote_row(
    "Text where goal are met for FY 2024", NA_character_
  ))
  expect_true(is_footnote_row(
    "Pre-Sub cohort excludes ineligible submissions", NA_character_
  ))
})

test_that("is_footnote_row leaves legitimate metric names alone", {
  expect_false(is_footnote_row("Number Received", NA_character_))
  expect_false(is_footnote_row(
    "Mean FDA Days for Submissions that Missed the Goal", NA_character_
  ))
  expect_false(is_footnote_row("Subs in the MDUFA Cohort", NA_character_))
  expect_false(is_footnote_row(
    "Subs in the MDUFA Cohort that Closed", NA_character_
  ))
})
