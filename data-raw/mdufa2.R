## code to prepare `mdufa2` dataset goes here
## Build the dataset ---------------------------------------------------------
# Load libraries
library(dplyr)
library(readr)
devtools::load_all()
str_nolint <- " # nolint: line_length_linter."

old_version <- mdufa::mdufa2

# For the MDUFA 2 dataset, there are only 6 pages to the most recent report.
# Rather than spending a ton of time scraping a totally different report
# format, let's just enter the few hundred metrics into a CSV and read that in.

mdufa2 <-
  readr::read_csv(
    file = "data-raw/mdufa2.csv",
    col_types =
      readr::cols(
        report_description = readr::col_character(),
        report_link = readr::col_character(),
        report_date = readr::col_character(),
        report_mdufa_period = readr::col_character(),
        source = readr::col_character(),
        page = readr::col_character(),
        table_number = readr::col_character(),
        organization = readr::col_character(),
        program = readr::col_character(),
        table_title = readr::col_character(),
        metric_type = readr::col_character(),
        performance_metric = readr::col_character(),
        fy = readr::col_character(),
        value = readr::col_character()
      )
  ) |>
  dplyr::mutate(
    report_date = lubridate::mdy(.data$report_date)
  )

# Validate column structure before saving
validate_columns(mdufa2, mdufa_cols, "mdufa2")

# Validate unique metrics (no duplicates)
validate_unique_metrics(mdufa2, "mdufa2")

usethis::use_data(mdufa2, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(mdufa2, width = 76) |>
  utils::capture.output(type = c("output"))
glimpse_output <- glimpse_output[-c(1:2)]

formatted_fields <- glimpse_output |>
  (\(x) {
    stringr::str_replace(
      string = x,
      pattern = "(^\\$\\s\\w*\\s*)",
      replacement = paste0(
        "  \\\\item{",
        stringr::str_extract(string = x, pattern = "(?<=^\\$\\s)\\b\\w*\\b"),
        "}{"
      )
    )
  })() |>
  (\(x) paste0(x, "}"))() |>
  stringr::str_remove_all(pattern = "\\[|\\]|\\<|\\>") |>
  stringr::str_remove_all(pattern = stringr::fixed("\0333m\03338;5;246m")) |>
  stringr::str_remove_all(pattern = stringr::fixed("\03339m\03323m")) |>
  (\(x) stringr::str_wrap(string = x, width = 76))() |>
  (\(x) stringr::str_split(x, pattern = "\\n"))() |>
  unlist()

documentation_text <-
  c(
    "MDUFA II",
    "",
    "FDA's Quarterly Medical Device User Performance Metrics in a tidy data ",
    "format. ",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa2),
      " rows and ",
      length(mdufa2),
      " fields: "
    ),
    "",
    "\\describe{",
    formatted_fields,
    "}",
    "",
    "@source ",
    paste0("[FDA MDUFA Reports](", url_report_page, ")", str_nolint),
    paste0("accessed 2023-01-21.")
  ) |>
  (\(x) paste0("#' ", x))() |>
  (\(x) {
    c(
      paste0(
        "# Do not hand edit this file. Edit data-raw/mdufa2.R ",
        "instead."
      ),
      x,
      "\"mdufa2\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa2.R",
  append = FALSE
)

devtools::document()

testthat::expect_equal(
  object = mdufa2,
  expected = old_version
)
waldo::compare(x = old_version, y = mdufa2, max_diffs = Inf)
