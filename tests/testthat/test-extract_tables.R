test_that("remove uneccessary text works", {
  test_cases_remove_all <-
    c(
      paste0(
        "Section 1 PMA Original and Panel-Track Supplements - ",
        "Center Level Metric"
      ),
      paste0(
        "Section 1 PMA Original and Panel-Track Supplements - ",
        "Office Level Metric"
      ),
      "Section 2 PMA 180-Day Supplements - Center Level Metric",
      "Section 3 PMA Real-Time Supplements - Center Level Metric",
      "Section 4 Pre-Market Report Submissions",
      "Section 5 PMA Annual Metrics and Goals",
      "Section 6 510(k) Center Level Metrics (Excludes Third Party Review)",
      paste0(
        "Section 7 510(k) Annual General Metrics - Annual Metrics and ",
        "Goals will be reported in"
      ),
      "Section 8 De Novo Center Level Metrics",
      "Section 9 Pre-Sub Center Level Metrics",
      "Section 9 Pre-Sub Office Level Metrics",
      "Section 10 IDE- Center Level Metric", # Sigh.
      "Section 11 CLIA Waiver Annual Metrics",
      "Section 12 Dual (510(k) and CLIA Waiver) Annual Metrics",
      paste0(
        "* RTA was implemented on November 8, 2019, thus RTA metrics in table ",
        "8.1 include only De Novos received on or after November 8, 2019. All ",
        "other tables include De Novos received on or after October 1, 2017."
      )
    ) |>
    # Add trailing vertical space
    (\(x) paste0(x, "\v"))()
  expect_equal(
    remove_unnecessary_text(test_cases_remove_all),
    rep("", max(seq_along(test_cases_remove_all)))
  )
  test_cases_do_not_remove <-
    c(
      paste0(
        "Table 8.1 OHT1 - Office of Ophthalmic, Anesthesia, Respiratory, ",
        "ENT and Dental Device"
      ),
      "Table 2.1 CDRH - PMA 180-Day Supplements Substantive Interaction Goal"
    ) |>
    # Add trailing vertical space
    (\(x) paste0(x, "\v"))()
  expect_equal(
    remove_unnecessary_text(test_cases_do_not_remove),
    test_cases_do_not_remove
  )
})

test_that("adding missing table headers works", {
  no_additional_text_needed <-
    c(
      "Hello"
    )
  expect_equal(
    add_missing_table_headers(no_additional_text_needed),
    no_additional_text_needed
  )

  fix_needed <-
    tibble::tribble(
      ~wrong, ~right,
      "\v FY 2018   FY 2019  FY 2020  FY 2021  FY 2022\v",
      "\vPerformance Metric    FY 2018   FY 2019  FY 2020  FY 2021  FY 2022\v"
    )
  expect_equal(
    add_missing_table_headers(fix_needed$wrong),
    fix_needed$right
  )
})

test_that("inserting delimiters works", {
  expect_equal(
    insert_delim(
      text_string = "Column 1  Column 2  Column 3  Column 4"
    ),
    "Column 1|Column 2|Column 3|Column 4"
  )
  expect_equal(
    insert_delim(
      text_string = "Column 1  Column 2  Column 3  Column 4",
      whitespace = 2
    ),
    "Column 1|Column 2|Column 3|Column 4"
  )
  expect_equal(
    insert_delim(
      text_string = "Column 1  Column 2  Column 3  Column 4",
      whitespace = 3
    ),
    "Column 1  Column 2  Column 3  Column 4"
  )
  multiple_text_elements <-
    c(
      "Column 1  Column 2  Column 3  Column 4",
      "nothing   should   happen   with   this"
    )
  expect_equal(
    insert_delim(multiple_text_elements),
    "Column 1|Column 2|Column 3|Column 4"
  )
  expect_equal(
    insert_delim(multiple_text_elements[[1]]),
    "Column 1|Column 2|Column 3|Column 4"
  )
})

test_that("collapsing line breaks works", {
  multiple_text_elements <-
    c(
      "Let's\n\n\nCollapse\n\n\n\n\n\n\n\nThis",
      "But not\n\n\n\n\n this"
    )
  expect_equal(
    collapse_line_breaks(multiple_text_elements),
    "Let's\nCollapse\nThis"
  )
  expect_equal(
    collapse_line_breaks(multiple_text_elements[[1]]),
    "Let's\nCollapse\nThis"
  )
  expect_equal(
    collapse_line_breaks(multiple_text_elements, 4),
    "Let's\n\n\nCollapse\nThis"
  )
})

test_that("extracting page numbers works", {
  # Pick random page numbers
  pages <- sample(1:10000, 100)
  # Make a total number of pages that is always bigger than that
  num_pages <- pages + sample(1:100, 5)
  # For each one, make sure we can extract the page
  for (i in seq_along(pages)) {
    text_object <-
      paste0(
        "Table 1.13 OHT39 - Office of This is Fake Data\n",
        "Performance Metric|FY 2018|FY 2019|FY 2020|FY 2021|FY 2022\n",
        "0|0|0|0|0\n",
        "|Page ", pages[[i]], " of ", num_pages[[i]]
      )
    # Get the page number
    this_result <- get_page_number(text_object)
    expect_equal(
      this_result$page,
      pages[[i]] |> as.character()
    )
    expect_equal(
      this_result$trimmed_string,
      paste0(
        "Table 1.13 OHT39 - Office of This is Fake Data\n",
        "Performance Metric|FY 2018|FY 2019|FY 2020|FY 2021|FY 2022\n",
        "0|0|0|0|0\n"
      )
    )
  }
})

test_that("extracting table titles works", {
  result <-
    get_table_title(
      text_string =
        paste0(
          "Table 1.13 OHT39 - Office of This is Fake Data\n",
          "Performance Metric|FY 2018|FY 2019|FY 2020|FY 2021|FY 2022\n",
          "0|0|0|0|0\n"
        )
    )
  expect_equal(
    result$trimmed_string,
    paste0(
      "\nPerformance Metric|FY 2018|FY 2019|FY 2020|FY 2021|FY 2022\n",
      "0|0|0|0|0\n"
    )
  )
  expect_equal(
    result$title,
    "Table 1.13 OHT39 - Office of This is Fake Data"
  )
})

test_that("fix_goal_row() returns correct output", {
  # Test that the function returns the input string as is when it does not
  # contain a goal line or a within days line
  expect_equal(fix_goal_row("abc"), "abc")

  # Test that the function returns the correct output when it contains a within
  # days line but not a goal line
  expect_equal(
    fix_goal_row("\n |50% Complete Within 0|50% Complete Within 0|50% Complete Within 0|50% Complete Within 0|50% Complete Within 0\n"), # nolint: line_length_linter.
    "\n |50% Complete Within 0|50% Complete Within 0|50% Complete Within 0|50% Complete Within 0|50% Complete Within 0\n" # nolint: line_length_linter.
  )

  # Test that the function returns the correct output when it contains a goal
  # line but not a within days line
  expect_equal(
    fix_goal_row("\n | 2020 Industry Days| 2021 Industry Days| 2022 Industry Days| 2023 Industry Days| 2024 Industry Days\n"), # nolint: line_length_linter.
    "\n | 2020 Industry Days| 2021 Industry Days| 2022 Industry Days| 2023 Industry Days| 2024 Industry Days\n" # nolint: line_length_linter.
  )
})

test_that("fix_si_row() works", {
  expect_equal(
    object =
      fix_si_row(
        # This text has extraneous line breaks
        paste0(
          "\nAverage number of FDA days to\nSubstantive Interaction|",
          "\nMaximum FDA days to\nSubstantive Interaction|",
          "\n40th Percentile FDA days to\nSubstantive Interaction|",
          "\n20th Percentile\nFDA\ndays to Substantive Interaction|",
          sep = "\n"
        )
      ),
    expected =
    # This text does not have extraneous line breaks
      paste0(
        "\nAverage number of FDA days to Substantive Interaction|",
        "\nMaximum FDA days to Substantive Interaction|",
        "\n40th Percentile FDA days to Substantive Interaction|",
        "\n20th Percentile FDA days to Substantive Interaction|",
        sep = "\n"
      )
  )
})
