## code to prepare `mdufa3` dataset goes here
# Build the dataset ------------------------------------------------------------
# Load libraries
library(magrittr)
library(pdftools)
library(rvest)
str_nolint <- " # nolint: line_length_linter."
devtools::load_all()

## Make a copy of the last version -----
old_version <- mdufa::mdufa3

## Scrape the FDA.gov page -----
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
  # We are only interested in the links that end in ".pdf" or "download"
  dplyr::filter(
    stringr::str_detect(
      string = .data$report_link,
      pattern =
        stringr::regex(
          pattern = "(.*\\.pdf$)|(.*download$)"
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

## Get the most recent report -----
current_report <-
  mdufa_reports %>%
  dplyr::filter(.data$report_mdufa_period == "MDUFA III") %>%
  dplyr::filter(.data$report_date == max(.data$report_date))

# FDA uses page freezer for old content now. Hard-coding a download link.
current_report$report_link <-
  paste0(
    "https://public4.pagefreezer.com/content/1701303066151/FDA/",
    "20-05-2023T12:18/",
    "https://www.fda.gov/media/120475/download"
  )

## Extract the report text -----
raw_text <-
  pdftools::pdf_text(current_report$report_link) %>%
  # Remove first 10 pages
  magrittr::extract(-seq(1, 10, 1)) %>%
  # Make sure a table title is present
  magrittr::extract(
    stringr::str_detect(
      string = .,
      pattern = table_title_text_pattern()
    )
  ) %>%
  # Remove whitespace before the term "performance"
  stringr::str_remove_all(
    string = .,
    pattern =
      stringr::regex(
        "(?<=\\v)\\s{2,}(?=\\bPerformance\\b)",
        ignore_case = TRUE
      )
  ) %>%
  # Replace the term "Performance Goals" with "Performance Metric"
  stringr::str_replace_all(
    string = .,
    pattern =
      stringr::regex("\\v\\s{0,}Performance Goals\\b", ignore_case = TRUE),
    replacement = "\nPerformance Metric"
  ) %>%
  # When it appears at the beginning of a line, replace the term
  # "PMA Submissions Received" with "Performance Metric".
  stringr::str_replace_all(
    string = .,
    pattern =
      stringr::regex("\\v\\s*PMA Submissions Received\\b", ignore_case = TRUE),
    replacement = "\nPerformance Metric"
  ) %>%
  # Some tables run the table number into the office name with no spaces. Let's
  # fix it.
  stringr::str_replace_all(
    string = .,
    pattern = "(?<=^Table\\s\\d{1,5}\\.\\d{1,5}\\.{0,1})O",
    replacement = " O"
  )

## Extract the tables -----
report_tables <- vector(mode = "list", length = max(seq_along(raw_text)))

for (i in seq_along(raw_text)) {
  report_tables[[i]] <- process_page(raw_text[[i]])
}

## Build regular expressions for each submission type -----
submission_type <-
  c(
    "510.{0,4}",
    "De Novo",
    "PMA Original and Panel-Track Supplements",
    "PMA",
    "Pre-Submissions",
    "Pre-Submission"
  ) %>%
  unique() %>%
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

## Clean the data -----

mdufa3 <-
  report_tables %>%
  dplyr::bind_rows() %>%
  # Table 5.3, which provides multi-year PMA rolling averages is a hot mess,
  # and, frankly, not that interesting. Rather than fix it's crazy headers,
  # let's just ignore it. Pull requests welcome though.
  dplyr::filter(
    stringr::str_detect(
      string = .data$source,
      pattern = stringr::fixed("Table 5.3"),
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
        pattern =
          paste0(
            "CDRH|CBER|ODE|OIR|DAGRID|DCD|DNPMD|DOD|DOED|DRGUD|DSD|",
            "DCTD|DIHD|DMD|DMGP|DRH"
          )
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
  # Standardize program type a bit
  dplyr::mutate(
    program =
      dplyr::case_when(
        stringr::str_detect(
          string = .data$program,
          pattern = "Pre-Submission"
        ) ~ "Pre-Submission",
        stringr::str_detect(
          string = .data$program,
          pattern = "PMA"
        ) ~ "PMA",
        TRUE ~ .data$program
      )
  ) %>%
  # Fix empty metric labels in Tables 6.5
  dplyr::mutate(
    performance_metric =
      dplyr::case_when(
        (dplyr::lag(.data$performance_metric) ==
           "Number with MDUFA III decision" &
          is.na(.data$performance_metric) &
          .data$table_number == "6.5") ~
          "Average FDA days to MDUFA III decision",
        TRUE ~
          stringr::str_remove(
            string = .data$performance_metric,
            pattern = stringr::regex("^decision\\s")
          ) %>%
          stringr::str_replace(
            string = .,
            pattern = "days\\sto\\sMDUFA\\sIII$",
            replacement = "days to MDUFA III decision"
          ) %>%
          stringr::str_remove(
            string = .,
            pattern =
              stringr::regex(
                paste0(
                  "(Maximum Industry days to MDUFA III decision\\s)",
                  "(?=(Average Total days to MDUFA III decision))"
                )
              )
          )
      )
  ) %>%
  dplyr::left_join(
    y = metric_types(report_mdufa_period = "MDUFA III"),
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
    .data$fy_2013,
    .data$fy_2014,
    .data$fy_2015,
    .data$fy_2016,
    .data$fy_2017,
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
  # Remove performance goal text rows.
  dplyr::filter(
    !(is.na(.data$performance_metric) &
        stringr::str_detect(
          string = .data$value,
          pattern =
            stringr::regex("within|days|SI|substantive", ignore_case = TRUE)
        )
    )
  ) %>%
  # Squish all character fields
  dplyr::mutate(
    dplyr::across(
      .cols = tidyselect:::where(is.character),
      ~ stringr::str_squish(.x)
    )
  )  %>%
  # No idea why these aren't matching. Smells like a character encoding issue?
  dplyr::mutate(
    metric_type =
      dplyr::case_when(
        is.na(.data$metric_type) &
          stringr::str_detect(
            string = .data$performance_metric,
            pattern = "^MDUFA III Decisions within.*Days$"
          ) ~ "integer",
        TRUE ~ .data$metric_type
      )
  ) %>%
  # Remove rows not associated with a metric, including some rows that describe
  # MDUFA goals
  dplyr::filter(!is.na(.data$metric_type))



View(
  mdufa3 %>%
    dplyr::select(
      -"report_description",
      -"report_link",
      -"report_date",
      -"report_mdufa_period"
    ) %>%
    dplyr::filter(
      stringr::str_detect(
        string = .data$table_title,
        pattern = "Performance Metric$"
      )
    )
)

## Write out the result -----

usethis::use_data(mdufa3, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

documentation_text <-
  c(
    "MDUFA III",
    "",
    "FDA's MDUFA III Quarterly Medical Device User Performance Metrics in a ",
    "tidy data format. ",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa3),
      " rows and ",
      length(mdufa3),
      " fields: "
    ),
    "",
    "\\describe{",
    dplyr::glimpse(mdufa3, width = 76) %>%
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
      "# Do not hand edit this file. Edit data-raw/mdufa3.R ",
      "instead."
    ),
    .,
    "\"mdufa3\""
  ) %>%
  stringr::str_squish() %T>%
  readr::write_lines(
    x = .,
    file = "R/mdufa3.R",
    append = FALSE
  )

devtools::document()

testthat::expect_equal(
  object = mdufa3,
  expected = old_version
)
waldo::compare(x = old_version, y = mdufa3, max_diffs = Inf)
