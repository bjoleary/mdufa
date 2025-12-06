## code to prepare `mdufa5` dataset goes here
# Build the dataset ------------------------------------------------------------
# Load libraries
library(magrittr)
library(rvest)
str_nolint <- " # nolint: line_length_linter."
devtools::load_all()

old_version <- mdufa::mdufa5

report_page_links <-
  rvest::read_html(url_report_page) %>%
  rvest::html_nodes("a")

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
  dplyr::filter(.data$report_mdufa_period == "MDUFA V") %>%
  dplyr::filter(.data$report_date == max(.data$report_date, na.rm = TRUE)) %>%
  head(1) %>%
  as.list()

mdufa5 <-
  get_m5(
    report_description = current_report$report_description,
    report_link =
      current_report$report_link %>%
        stringr::str_remove("\\?attachment$"),
    report_date = current_report$report_date
  )

usethis::use_data(mdufa5, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

documentation_text <-
  c(
    "MDUFA V",
    "",
    "FDA's Quarterly Medical Device User Performance Metrics in a tidy data ",
    "format. ",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa5),
      " rows and ",
      length(mdufa5),
      " fields: "
    ),
    "",
    "\\describe{",
    dplyr::glimpse(mdufa5, width = 76) %>%
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
      unlist(),
    "}",
    "",
    "@source ",
    paste0("[FDA MDUFA Reports](", url_report_page, ")", str_nolint),
    paste0("accessed ", lubridate::today(), ".")
  ) %>%
  paste0("#' ", .) %>%
  c(
    paste0(
      "# Do not hand edit this file. Edit data-raw/mdufa5.R ",
      "instead."
    ),
    .,
    "\"mdufa5\""
  ) %>%
  stringr::str_squish() %T>%
  readr::write_lines(
    x = .,
    file = "R/mdufa5.R",
    append = FALSE
  )

devtools::document()

testthat::expect_equal(
  object = mdufa5,
  expected = old_version
)
waldo::compare(x = old_version, y = mdufa5, max_diffs = Inf)
