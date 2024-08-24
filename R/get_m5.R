#' Get MDUFA V Data
#'
#' @param report_description A description of the report from the FDA website.
#'   Typically something along the lines of 'March 1, 2023 MDUFA V Performance
#'   Report' or similar.
#' @param report_link The url at which the report PDF can be accessed.
#' @param report_date The date of the report.
#'
#' @return A table of MDUFA V data.
#' @export
#'
get_m5 <- function(report_description, report_link, report_date) {
  chk::chk_chr(report_description)
  chk::chk_chr(report_link)
  chk::chk_date(report_date)
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The get_m5() function requires the pdftools package.")
  }
  current_report <-
    tibble::tribble(
      ~report_description, ~report_link, ~report_date, ~report_mdufa_period,
      report_description, report_link, report_date, "MDUFA V"
    )
  raw_data <- pdftools::pdf_data(report_link) # nolint: object_usage_linter.
  raw_text <- extract_text(report_link)

  # Extract the tables
  report_tables <- vector(mode = "list", length = max(seq_along(raw_text)))

  for (i in seq_along(raw_text)) {
    report_tables[[i]] <- process_page(raw_text[[i]])
  }

  mdufa5 <- # nolint: object_usage_linter.
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
          pattern = submission_types
        ),
      table_title =
        stringr::str_extract(
          string = .data$source,
          pattern = paste0("(?=", submission_types, ").*$")
        )
    ) %>%
    dplyr::filter(.data$performance_metric != "") %>%
    dplyr::left_join(
      y = metric_types("MDUFA V"),
      by = c("performance_metric" = "performance_metric")
    ) %>%
    dplyr::select(
      "source",
      "page",
      "table_number",
      "organization",
      "program",
      "table_title",
      "metric_type",
      "performance_metric",
      "fy_2023",
      "fy_2024",
      "fy_2025",
      "fy_2026",
      "fy_2027",
      dplyr::everything()
    ) %>%
    # remove all NA
    dplyr::select(tidyselect::where(~ !all(is.na(.)))) %>%
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
        .cols = tidyselect::where(is.character),
        ~ stringr::str_squish(.x)
      )
    )
}
