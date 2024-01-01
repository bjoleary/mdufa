## code to prepare `mdufa4` dataset goes here
# Build the dataset ------------------------------------------------------------
# Load libraries
library(magrittr)
library(pdftools)
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
  rvest::read_html(url_report_page) %>%
  rvest::html_nodes("a") %T>%
  print()

mdufa_reports <-
  tibble::enframe(
    x = report_page_links %>% rvest::html_text(),
    name = NULL,
    value = "report_description"
  ) %>%
  dplyr::bind_cols(
    tibble::enframe(
      x = report_page_links %>% rvest::html_attr("href"),
      name = NULL,
      value = "report_link"
    )
  ) %>%
  # We are only interested in the links that end in ".pdf" or include "download"
  dplyr::filter(
    stringr::str_detect(
      string = .data$report_link,
      pattern =
        stringr::regex(
          pattern = "(.*\\.pdf$)|(.*download)"
        )
    )
  ) %>%
  # If the link doesn't start with https, let's prepend the fda.gov url
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
        pattern =
          stringr::regex(
            "^\\w*\\s\\d{1,2},\\s\\d{4}"
          )
      ) %>%
      lubridate::as_date(format = "%B %d, %Y"),
    report_mdufa_period =
      stringr::str_extract(
        string = .data$report_description,
        pattern =
          stringr::regex(
            "\\bMDUFA\\s[IVXL]{1,}\\b"
          )
      ) %>%
      tidyr::replace_na("MDUFA II")
  )

# Get the most recent report
current_report <-
  mdufa_reports %>%
  dplyr::filter(.data$report_mdufa_period == "MDUFA IV") %>%
  dplyr::filter(.data$report_date == max(.data$report_date))

# Extract the report text
raw_text <- extract_text(current_report$report_link)

# Extract the tables
report_tables <- vector(mode = "list", length = max(seq_along(raw_text)))

for (i in seq_along(raw_text)) {
  report_tables[[i]] <- process_page(raw_text[[i]])
}

# Build regular expressions for each submission type
submission_type <-
  c(
    "510.{0,4}",
    "De Novo",
    "PMA Original and Panel\\-Track Supplements",
    "PMA Originals and Panel Tracked Supplements",
    "PMA 180\\-Day Supplements",
    "PMA Real-Time Supplements",
    "Pre\\-Market Report Submissions",
    "IDE",
    "Pre\\-Sub"
  ) %>%
  # Wrap with word boundaries
  paste0("\\b", ., "\\b") %>%
  # Add all review tracks, which has a paren that should not be wrapped with
  # word boundaries
  c(
    .,
    "PMAs \\(All Review Tracks\\)"
  ) %>%
  # Collapse with "OR":
  paste(collapse = "|") %>%
  stringr::regex()

mdufa4 <-
  report_tables %>%
  dplyr::bind_rows() %>%
  # Table 9.2, which provides center-level metrics on the number of q-subs
  # with written feedback, is a hot mess, and, frankly, not that interesting.
  # Rather than fix it's crazy headers, let's just ignore it. Pull requests
  # welcome though.
  dplyr::filter(
    stringr::str_detect(
      string = .data$source,
      pattern = stringr::fixed("Table 9.2"),
      negate = TRUE
    )
  ) %>%
  # We can remove the empty columns that resulted from Table 9.2
  janitor::remove_empty(which = c("cols")) %>%
  # Let's detect the table numbers
  dplyr::mutate(
    # Let's detect the table numbers, which are the digits right after the
    # word "Table" at the beginning of the string.
    table_number =
      stringr::str_extract(
        string = .data$source,
        pattern = "(?<=^Table\\s)\\d*\\.\\d*\\b"
      ),
    # And the organization, which is the word right before the hyphen
    organization =
      stringr::str_extract(
        string = .data$source,
        pattern = "\\w*(?=\\b\\s(\\-|\\\\032))" # CBER & CDRH use different
                                                # hyphens...
        # TODO: Rather than handle the non-ascii dash character, I should
        # probably strip it from the string at the outset.
      ),
    # Extract submission type text into its own field
    program =
      stringr::str_extract(
        string = .data$source,
        pattern = submission_type
      ),
    table_title =
      stringr::str_extract(
        string = .data$source,
        pattern = paste0("(?=", submission_type, ").*$")
      )
  ) %>%
  dplyr::left_join(
    y = metric_types(),
    by = c("performance_metric" = "performance_metric")
  ) %>%
  dplyr::select(
    .data$source,
    .data$page,
    .data$table_number,
    .data$organization,
    .data$program,
    .data$table_title,
    .data$metric_type,
    .data$performance_metric,
    .data$fy_2018,
    .data$fy_2019,
    .data$fy_2020,
    .data$fy_2021,
    .data$fy_2022,
    dplyr::everything()
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::contains("fy"),
    names_to = "fy",
    names_prefix = "fy\\_",
    values_to = "value"
  ) %>%
  dplyr::bind_cols(
    current_report,
    .
  ) %>%
  # Squish all character fields
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect:::where(is.character),
      ~ stringr::str_squish(.x)
    )
  )

usethis::use_data(mdufa4, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

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
    dplyr::glimpse(mdufa4, width = 76) %>%
      utils::capture.output(type = c("output")) %>%
      magrittr::extract(-c(1:2)) %>%
      stringr::str_replace(
        string = .,
        pattern = "(^\\$\\s\\w*\\s*)", # the column name
        replacement =
          paste0(
            "  \\\\item{",
            stringr::str_extract(
              string = .,
              pattern = "(?<=^\\$\\s)\\b\\w*\\b"
            ),
            "}{"
          )
      ) %>%
      paste0(., "}") %>%
      # Square brackets are a link in Roxygen. Replace:
      stringr::str_remove_all(
        string = .,
        pattern = "\\[|\\]|\\<|\\>"
      ) %>%
      # Remove formatting strings
      stringr::str_remove_all(
        string = .,
        pattern = stringr::fixed("\0333m\03338;5;246m")
      ) %>%
      stringr::str_remove_all(
        string = .,
        pattern = stringr::fixed("\03339m\03323m")
      ) %>%
      stringr::str_wrap(
        string = .,
        width = 76
      ) %>%
      stringr::str_split(pattern = "\\n") %>%
      unlist() ,
    "}",
    "",
    "@source ",
    paste0("[FDA MDUFA Reports](", url_report_page, ")", str_nolint),
    paste0("accessed ", lubridate::today(), ".")
  ) %>%
  paste0("#' ", .) %>%
  c(
    paste0(
      "# Do not hand edit this file. Edit data-raw/mdufa4.R ",
      "instead."
    ),
    .,
    "\"mdufa4\""
  ) %>%
  stringr::str_squish() %T>%
  readr::write_lines(
    x = .,
    file = "R/mdufa4.R",
    append = FALSE
  )

devtools::document()

testthat::expect_equal(
  object = mdufa4,
  expected = old_version
)
waldo::compare(x = old_version, y = mdufa4, max_diffs = Inf)
