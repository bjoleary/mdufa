## code to prepare `mdufa4` dataset goes here
# Build the dataset ------------------------------------------------------------
library(rvest)
str_nolint <- " # nolint: line_length_linter."
devtools::load_all()

old_version <- mdufa::mdufa4

url_report_page <-
  paste0(
    "https://www.fda.gov/industry/medical-device-user-fee-amendments-mdufa/",
    "mdufa-reports"
  )

report_page_links <-
  rvest::read_html(url_report_page) |>
  rvest::html_nodes("a")
print(report_page_links)

mdufa_reports <-
  tibble::enframe(
    x = report_page_links |> rvest::html_text(),
    name = NULL,
    value = "report_description"
  ) |>
  dplyr::bind_cols(
    tibble::enframe(
      x = report_page_links |> rvest::html_attr("href"),
      name = NULL,
      value = "report_link"
    )
  ) |>
  # Only interested in links ending in ".pdf" or "download"
  dplyr::filter(
    stringr::str_detect(
      string = .data$report_link,
      pattern = stringr::regex(pattern = "(.*\\.pdf$)|(.*download)")
    )
  ) |>
  # If link doesn't start with https, prepend the fda.gov url
  dplyr::mutate(
    report_link =
      dplyr::case_when(
        stringr::str_detect(
          string = .data$report_link,
          pattern = "^https\\:",
          negate = TRUE
        ) ~ paste0("https://www.fda.gov", .data$report_link),
        TRUE ~ .data$report_link
      ),
    report_date =
      stringr::str_extract(
        string = .data$report_description,
        pattern = stringr::regex("^\\w*\\s\\d{1,2},\\s\\d{4}")
      ) |>
        lubridate::as_date(format = "%B %d, %Y"),
    report_mdufa_period =
      stringr::str_extract(
        string = .data$report_description,
        pattern = stringr::regex("\\bMDUFA\\s[IVXL]{1,}\\b")
      ) |>
        tidyr::replace_na("MDUFA II")
  )

# Get the most recent report
current_report <-
  mdufa_reports |>
  dplyr::filter(.data$report_mdufa_period == "MDUFA IV") |>
  dplyr::filter(.data$report_date == max(.data$report_date))

# Extract data using extract_report()
mdufa4 <- extract_report(
  pdf_path = current_report$report_link,
  mdufa_period = "MDUFA IV",
  report_date = current_report$report_date,
  report_description = current_report$report_description,
  report_link = current_report$report_link
)

usethis::use_data(mdufa4, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(mdufa4, width = 76) |>
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
    "MDUFA IV",
    "",
    "FDA's Quarterly Medical Device User Performance Metrics in a tidy data ",
    "format. ",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa4),
      " rows and ",
      length(mdufa4),
      " fields: "
    ),
    "",
    "\\describe{",
    formatted_fields,
    "}",
    "",
    "@source ",
    paste0("[FDA MDUFA Reports](", url_report_page, ")", str_nolint),
    paste0("accessed ", lubridate::today(), ".")
  ) |>
  (\(x) paste0("#' ", x))() |>
  (\(x) {
    c(
      paste0(
        "# Do not hand edit this file. Edit data-raw/mdufa4.R ",
        "instead."
      ),
      x,
      "\"mdufa4\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa4.R",
  append = FALSE
)

devtools::document()

testthat::expect_equal(
  object = mdufa4,
  expected = old_version
)
waldo::compare(x = old_version, y = mdufa4, max_diffs = Inf)
