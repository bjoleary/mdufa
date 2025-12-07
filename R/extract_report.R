#' Extract MDUFA Report Data from PDF
#'
#' Extracts and processes table data from an MDUFA performance report PDF.
#' This is the core extraction function used for both building package datasets
#' and regression testing.
#'
#' @param pdf_path Path to the PDF file (local file or URL)
#' @param mdufa_period Character string indicating the MDUFA period
#'   (e.g., "MDUFA III", "MDUFA IV", "MDUFA V")
#' @param report_date Optional date of the report. If NULL, will not be added
#'   to output.
#' @param report_description Optional description of the report.
#' @param report_link Optional link to the report.
#'
#' @return A tibble containing the extracted table data with columns:
#'   source, page, table_number, organization, program, table_title,
#'   metric_type, performance_metric, fy, value
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract from a local PDF
#' data <- extract_report(
#'   pdf_path = "path/to/mdufa-4_2023-11-16.pdf",
#'   mdufa_period = "MDUFA IV"
#' )
#' }
extract_report <- function(pdf_path,
                           mdufa_period,
                           report_date = NULL,
                           report_description = NULL,
                           report_link = NULL) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required. Please install it.")
  }

  # Determine which extraction method to use based on MDUFA period
  if (mdufa_period == "MDUFA V") {
    return(extract_report_m5(
      pdf_path = pdf_path,
      mdufa_period = mdufa_period,
      report_date = report_date,
      report_description = report_description,
      report_link = report_link
    ))
  }

  # Extract raw text from PDF
  raw_text <- pdftools::pdf_text(pdf_path)

  # For MDUFA III, skip first 10 pages
  if (mdufa_period == "MDUFA III") {
    raw_text <- raw_text[-seq(1, min(10, length(raw_text)), 1)]
  }

  # Filter to pages with table titles
  raw_text <- raw_text[
    stringr::str_detect(
      string = raw_text,
      pattern = table_title_text_pattern()
    )
  ]


  # NOTE: Previous appendix removal for MDUFA IV was discarding CBER tables
  # (11.x, 12.x). These tables are in the appendix but contain valid data.
  # The table pattern filter already excludes non-table pages.

  # Apply text cleaning for MDUFA III
  if (mdufa_period == "MDUFA III") {
    raw_text <- raw_text |>
      # Remove whitespace before the term "performance"
      stringr::str_remove_all(
        pattern = stringr::regex(
          "(?<=\\v)\\s{2,}(?=\\bPerformance\\b)",
          ignore_case = TRUE
        )
      ) |>
      # Replace the term "Performance Goals" with "Performance Metric"
      stringr::str_replace_all(
        pattern = stringr::regex(
          "\\v\\s{0,}Performance Goals\\b",
          ignore_case = TRUE
        ),
        replacement = "\nPerformance Metric"
      ) |>
      # Replace "PMA Submissions Received" at beginning of line
      stringr::str_replace_all(
        pattern = stringr::regex(
          "\\v\\s*PMA Submissions Received\\b",
          ignore_case = TRUE
        ),
        replacement = "\nPerformance Metric"
      ) |>
      # Fix table numbers running into office names
      stringr::str_replace_all(
        pattern = "(?<=^Table\\s\\d{1,5}\\.\\d{1,5}\\.{0,1})O",
        replacement = " O"
      ) |>
      # Fix "MDUFA III Decisions within X FDA Days" rows where the label is on
      # a separate line from the values (common in CBER tables)
      stringr::str_replace_all(
        pattern = stringr::regex(
          "(MDUFA III Decisions within \\d+ FDA Days)\\s*\\n(\\s+)",
          ignore_case = FALSE
        ),
        replacement = "\\1  "
      )
  }

  # Extract tables from each page
  report_tables <- vector(mode = "list", length = length(raw_text))
  for (i in seq_along(raw_text)) {
    report_tables[[i]] <- tryCatch(
      process_page(raw_text[[i]]),
      error = function(e) NULL
    )
  }

  # Combine all tables
  combined <- dplyr::bind_rows(report_tables)

  if (nrow(combined) == 0) {
    return(tibble::tibble())
  }

  # Build submission type regex
  submission_type <- build_submission_type_regex(mdufa_period)

  # Clean and structure the data
  result <- combined |>
    # Filter out problematic tables for MDUFA III/IV
    dplyr::filter(
      # Table 9.2 has unusual structure - only parseable for MDUFA V
      !stringr::str_detect(
        string = .data$source,
        pattern = stringr::fixed("Table 9.2")
      ),
      # Filter out Appendix A - Variable Definitions pages
      # These have source patterns like "Table 1.1 and Tables 1.1.x"
      !stringr::str_detect(
        string = .data$source,
        pattern = "and Tables \\d"
      )
    ) |>
    # Remove empty columns
    janitor::remove_empty(which = c("cols")) |>
    # Extract table metadata
    dplyr::mutate(
      # Lookahead handles malformed headers like "Table 9.1DAGRID"
      table_number = stringr::str_extract(
        string = .data$source,
        pattern = "(?<=^Table\\s)\\d+\\.\\d+(?=\\.|[A-Z]|\\s|-)"
      ),
      organization = stringr::str_extract(
        string = .data$source,
        pattern = paste0(
          "CDRH|CBER|ODE|OIR|DAGRID|DCD|DNPMD|DOD|DOED|DRGUD|DSD|",
          "DCTD|DIHD|DMD|DMGP|DRH|",
          "OHT1|OHT2|OHT3|OHT4|OHT5|OHT6|OHT7|OHT8"
        )
      ),
      program = stringr::str_extract(
        string = .data$source,
        pattern = submission_type
      ),
      table_title = stringr::str_extract(
        string = .data$source,
        pattern = paste0("(?=", submission_type, ").*$")
      )
    )


  # Fix empty metric labels (MDUFA III specific)
  # Several tables have rows with empty performance_metric cells that follow

  # a known pattern. Use lag() to detect and fill these.
  if (mdufa_period == "MDUFA III") {
    # Table 6.5: After "Number with MDUFA III decision" comes
    # "Average FDA days to MDUFA III decision"
    prev_metric <- "Number with MDUFA III decision"
    avg_fda_days <- "Average FDA days to MDUFA III decision"

    # Tables 1.4, 1.5, 2.2, 3.1, 6.4: After "MDUFA III Decisions" comes
    # "MDUFA III Decisions within X FDA Days" (X varies by table)
    decisions_within_map <- c(
      "1.4" = "MDUFA III Decisions within 180 FDA Days",
      "1.5" = "MDUFA III Decisions within 320 FDA Days",
      "2.2" = "MDUFA III Decisions within 180 FDA Days",
      "3.1" = "MDUFA III Decisions within 90 FDA Days",
      "6.4" = "MDUFA III Decisions within 90 FDA Days"
    )

    result <- result |>
      dplyr::mutate(
        performance_metric = dplyr::case_when(
          # Table 6.5 fix
          (dplyr::lag(.data$performance_metric) == prev_metric &
            is.na(.data$performance_metric) &
            .data$table_number == "6.5") ~ avg_fda_days,
          # Tables 1.4, 1.5, 2.2, 3.1, 6.4 fix
          (dplyr::lag(.data$performance_metric) == "MDUFA III Decisions" &
            is.na(.data$performance_metric) &
            .data$table_number %in% names(decisions_within_map)) ~
            decisions_within_map[.data$table_number],
          TRUE ~ stringr::str_remove(
            .data$performance_metric,
            stringr::regex("^decision\\s")
          ) |>
            stringr::str_replace(
              "days\\sto\\sMDUFA\\sIII$",
              "days to MDUFA III decision"
            ) |>
            # Fix concatenated "Max Industry days...Avg Total days..." text
            stringr::str_remove(
              stringr::regex(
                paste0(
                  "(Maximum Industry days to MDUFA III decision\\s)",
                  "(?=(Average Total days to MDUFA III decision))"
                )
              )
            )
        )
      )
  }

  # Add metric types
  result <- result |>
    dplyr::left_join(
      y = metric_types(report_mdufa_period = mdufa_period),
      by = c("performance_metric" = "performance_metric")
    )

  # Pivot to long format
  fy_cols <- names(result)[stringr::str_detect(names(result), "^fy_")]

  if (length(fy_cols) > 0) {
    result <- result |>
      dplyr::select(
        dplyr::any_of(c(
          "source", "page", "table_number", "organization",
          "program", "table_title", "metric_type", "performance_metric"
        )),
        dplyr::starts_with("fy_")
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("fy_"),
        names_to = "fy",
        names_prefix = "fy_",
        values_to = "value"
      )
  }

  # Remove performance goal text rows (MDUFA III specific)
  if (mdufa_period == "MDUFA III") {
    result <- result |>
      dplyr::filter(
        !(is.na(.data$performance_metric) &
          stringr::str_detect(
            .data$value,
            stringr::regex("within|days|SI|substantive", ignore_case = TRUE)
          ))
      )
  }


  # Remove rows without metric types (MDUFA III only)
  # MDUFA IV and V have valid data with NA metric_types (CLIA waiver tables)
  if (mdufa_period == "MDUFA III") {
    result <- result |>
      dplyr::filter(!is.na(.data$metric_type))
  }

  # Remove duplicate rows
  result <- dplyr::distinct(result)

  # Remove garbage rows where performance_metric, value, AND organization
  # are all NA (stray headers from definitions pages)
  result <- result |>
    dplyr::filter(
      !is.na(.data$performance_metric) |
        !is.na(.data$value) |
        !is.na(.data$organization)
    )

  # Add report metadata if provided
  if (!is.null(report_date)) {
    result$report_date <- report_date
  }
  if (!is.null(report_description)) {
    result$report_description <- report_description
  }
  if (!is.null(report_link)) {
    result$report_link <- report_link
  }
  result$report_mdufa_period <- mdufa_period

  # Squish all character fields
  result <- result |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::where(is.character),
        ~ stringr::str_squish(.x)
      )
    )

  result
}

#' Build Submission Type Regex
#'
#' Creates a regex pattern for matching submission types based on MDUFA period.
#'
#' @param mdufa_period The MDUFA period
#'
#' @return A regex pattern
#' @keywords internal
build_submission_type_regex <- function(mdufa_period) {
  if (mdufa_period == "MDUFA III") {
    types <- c(
      "510.{0,4}",
      "De Novo",
      "PMA Original and Panel-Track Supplements",
      "PMA",
      "Pre-Submissions",
      "Pre-Submission"
    )
  } else {
    types <- c(
      "510.{0,4}",
      "De Novo",
      "PMA Original and Panel\\-Track Supplements",
      "PMA Originals and Panel Tracked Supplements",
      "PMA 180\\-Day Supplements",
      "PMA Real-Time Supplements",
      "Pre\\-Market Report Submissions",
      "IDE",
      "Pre\\-Sub"
    )
  }

  types <- unique(types)
  types <- paste0("\\b", types, "\\b")
  types <- c(types, "PMAs \\(All Review Tracks\\)")
  paste(types, collapse = "|") |>
    stringr::regex()
}

#' Extract MDUFA V Report Data
#'
#' Internal function for extracting MDUFA V reports which use a different
#' format.
#'
#' @inheritParams extract_report
#' @return A tibble containing the extracted table data
#' @keywords internal
extract_report_m5 <- function(pdf_path,
                              mdufa_period,
                              report_date = NULL,
                              report_description = NULL,
                              report_link = NULL) {
  # Use M5-specific extraction (returns tibble with page_number, raw_text)
  raw_text_df <- extract_text_m5(pdf_path)

  if (is.null(raw_text_df) || nrow(raw_text_df) == 0) {
    return(tibble::tibble())
  }

  # Extract tables from each page - iterate over rows of the tibble
  report_tables <- vector(mode = "list", length = nrow(raw_text_df))
  for (i in seq_len(nrow(raw_text_df))) {
    report_tables[[i]] <- tryCatch(
      process_page_m5(raw_text_df$raw_text[[i]], raw_text_df$page_number[[i]]),
      error = function(e) NULL
    )
  }

  # Combine all tables
  combined <- dplyr::bind_rows(report_tables)

  if (nrow(combined) == 0) {
    return(tibble::tibble())
  }

  # Clean and structure similar to extract_report
  submission_type <- build_submission_type_regex(mdufa_period)

  result <- combined |>
    janitor::remove_empty(which = c("cols")) |>
    dplyr::mutate(
      # Lookahead handles malformed headers like "Table 9.1DAGRID"
      table_number = stringr::str_extract(
        string = .data$source,
        pattern = "(?<=^Table\\s)\\d+\\.\\d+(?=\\.|[A-Z]|\\s|-)"
      ),
      # Extract organization - prefer OHT (more specific) over CDRH/CBER
      # First try OHT, then fall back to CDRH/CBER if no OHT found
      organization = dplyr::coalesce(
        stringr::str_extract(
          string = .data$source,
          pattern = "OHT[1-8]"
        ),
        stringr::str_extract(
          string = .data$source,
          pattern = "CDRH|CBER"
        )
      ),
      program = stringr::str_extract(
        string = .data$source,
        pattern = submission_type
      ),
      table_title = stringr::str_extract(
        string = .data$source,
        pattern = paste0("(?=", submission_type, ").*$")
      )
    ) |>
    # Filter out problematic tables (consistent with get_m5)
    dplyr::filter(
      # Table 13.x are TAP program tables - exclude for now
      !stringr::str_detect(
        string = .data$table_number,
        pattern = "^13\\."
      )
    )

  # Squish performance_metric before join (OHT tables may have leading spaces)
  result <- result |>
    dplyr::mutate(
      performance_metric = stringr::str_squish(.data$performance_metric)
    )

  # Add metric types
  result <- result |>
    dplyr::left_join(
      y = metric_types(report_mdufa_period = mdufa_period),
      by = c("performance_metric" = "performance_metric")
    )

  # Pivot to long format
  fy_cols <- names(result)[stringr::str_detect(names(result), "^fy_")]

  if (length(fy_cols) > 0) {
    result <- result |>
      dplyr::select(
        dplyr::any_of(c(
          "source", "page", "table_number", "organization",
          "program", "table_title", "metric_type", "performance_metric"
        )),
        dplyr::starts_with("fy_")
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("fy_"),
        names_to = "fy",
        names_prefix = "fy_",
        values_to = "value"
      )
  }

  # Remove duplicate rows
  result <- dplyr::distinct(result)


  # Remove garbage rows (orphaned header lines with NA performance_metric)
  result <- result |>
    dplyr::filter(!is.na(.data$performance_metric))

  # Add report metadata
  if (!is.null(report_date)) {
    result$report_date <- report_date
  }
  if (!is.null(report_description)) {
    result$report_description <- report_description
  }
  if (!is.null(report_link)) {
    result$report_link <- report_link
  }
  result$report_mdufa_period <- mdufa_period

  # Squish all character fields
  result <- result |>
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::where(is.character),
        ~ stringr::str_squish(.x)
      )
    )

  result
}
