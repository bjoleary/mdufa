# Create report_dates dataset with report dates and cutoff dates
#
# This script extracts cutoff dates from the first few pages of each
# MDUFA performance report PDF. The cutoff date is the date through
# which data in the report covers (e.g., "Actions through September 30, 2025").
#
# Columns:
# - report_date: Date the report was published
# - report_cutoff_date: Date through which data is included
# - report_mdufa_period: MDUFA period (e.g., "MDUFA V")
# - report_description: Full description from FDA website
# - report_link: URL to the report PDF

library(pdftools)
library(stringr)
library(dplyr)
library(lubridate)
library(rvest)
library(tidyr)
library(readr)

# Directory containing downloaded PDF reports
pdf_dir <- "data-raw/pdf_reports"

#' Check if a file is a valid PDF
#'
#' @param file_path Path to file
#' @return TRUE if valid PDF, FALSE otherwise
is_valid_pdf <- function(file_path) {
  # Check magic bytes (PDF files start with %PDF)
  con <- file(file_path, "rb")
  on.exit(close(con))
  header <- rawToChar(readBin(con, raw(), n = 5))
  if (grepl("^%PDF", header)) {
    return(TRUE)
  }
  # If no PDF magic bytes, only accept if file is large (could be misidentified)
  # Small files without PDF magic bytes are likely HTML error pages (~20KB)
  file.info(file_path)$size > 100000
}

#' Extract cutoff date from a PDF report
#'
#' Searches the first 20 pages of a PDF for text matching
#' "Actions through [date]" or "through [date]"
#'
#' @param pdf_path Path to PDF file
#' @return Date object or NA if not found
extract_cutoff_date <- function(pdf_path) {
  # Check if file is a valid PDF first
  if (!is_valid_pdf(pdf_path)) {
    return(as.Date(NA))
  }

  tryCatch(
    {
      # Read first 20 pages (some reports have cutoff date further in)
      n_pages <- min(20, pdftools::pdf_info(pdf_path)$pages)
      text <- pdftools::pdf_text(pdf_path)[1:n_pages]
      text <- paste(text, collapse = " ")

      # Look for "through [date]" patterns
      # Handles formats like:
      #   "Actions through September 30, 2025"
      #   "Action through 31 December 2013"
      #   "Actions through Sept 30, 2012" (abbreviated month)
      #   "Actions through Sept. 30, 2012" (abbreviated with period)
      #   "through 31 December 2019" (without "Action" prefix)

      # Month pattern: full name or 3-letter abbreviation with optional period
      month_pattern <- "[A-Za-z]{3,9}\\.?"

      # Try pattern with "Action(s) through" first (more specific)
      pattern1 <- paste0(
        "Action[s]?\\s+through\\s+",
        "(",
        month_pattern, "\\s+\\d{1,2},?\\s+\\d{4}", # Sept 30, 2025
        "|",
        "\\d{1,2}\\s+", month_pattern, "\\s+\\d{4}", # 31 December 2013
        ")"
      )
      match <- stringr::str_match(
        text, stringr::regex(pattern1, ignore_case = TRUE)
      )

      # If not found, try just "through [date]" (less specific but catches more)
      if (is.na(match[1, 2])) {
        # Try day-month-year format: "through 30 September 2012"
        pattern2 <- paste0(
          "through\\s+",
          "(",
          "\\d{1,2}\\s+", month_pattern, "\\s+\\d{4}",
          ")"
        )
        match <- stringr::str_match(
          text, stringr::regex(pattern2, ignore_case = TRUE)
        )
      }

      # If still not found, try month-day-year: "through Sept 30, 2012"
      if (is.na(match[1, 2])) {
        pattern3 <- paste0(
          "through\\s+",
          "(",
          month_pattern, "\\s+\\d{1,2},?\\s+\\d{4}",
          ")"
        )
        match <- stringr::str_match(
          text, stringr::regex(pattern3, ignore_case = TRUE)
        )
      }

      if (!is.na(match[1, 2])) {
        date_str <- match[1, 2]
        # Remove any periods from abbreviated months for parsing
        date_str <- stringr::str_replace(date_str, "\\.", "")
        # Normalize 4-letter month abbreviations to 3-letter (Sept -> Sep)
        # Only replace "Sept" when followed by space or end (not "September")
        date_str <- stringr::str_replace(date_str, "Sept(?=\\s|$)", "Sep")
        # Try parsing with lubridate (handles multiple formats)
        parsed <- lubridate::mdy(date_str, quiet = TRUE)
        if (is.na(parsed)) {
          parsed <- lubridate::dmy(date_str, quiet = TRUE)
        }
        return(parsed)
      }
      as.Date(NA)
    },
    error = function(e) {
      warning(
        "Error extracting cutoff from ", basename(pdf_path), ": ", e$message
      )
      as.Date(NA)
    }
  )
}

# Get all PDF reports
pdfs <- list.files(pdf_dir, pattern = "[.]pdf$", full.names = TRUE)

cat("Processing", length(pdfs), "PDF reports...\n\n")

# Extract cutoff dates from each PDF
results <- lapply(pdfs, function(pdf) {
  filename <- basename(pdf)

  # Parse filename: mdufa-X_YYYY-MM-DD_quarterly-report.pdf
  parts <- stringr::str_match(filename, "mdufa-([^_]+)_([0-9-]+)_")

  mdufa_num <- parts[1, 2]
  report_date_str <- parts[1, 3]

  # Convert MDUFA number to period name
  # Note: "2i" files are MDUFA III reports (content shows
  # "MDUFA III (FY 2013-2017)"). The "2i" naming was a local convention
  # but they're listed as MDUFA III on FDA site.
  mdufa_period <- dplyr::case_match(
    mdufa_num,
    "2" ~ "MDUFA II",
    "2i" ~ "MDUFA III",
    "3" ~ "MDUFA III",
    "4" ~ "MDUFA IV",
    "5" ~ "MDUFA V",
    .default = paste("MDUFA", mdufa_num)
  )

  report_date <- lubridate::ymd(report_date_str)
  cutoff_date <- extract_cutoff_date(pdf)

  cat(
    format(report_date, "%Y-%m-%d"), " | ",
    mdufa_period, " | ",
    "Cutoff:", format(cutoff_date, "%Y-%m-%d"), "\n"
  )

  tibble::tibble(
    report_date = report_date,
    report_cutoff_date = cutoff_date,
    report_mdufa_period = mdufa_period,
    filename = filename
  )
})

# Combine results and deduplicate
# (mdufa-2i and mdufa-3 files for the same date are the same report)
report_dates_raw <- dplyr::bind_rows(results) |>
  dplyr::distinct(report_date, report_mdufa_period, .keep_all = TRUE) |>
  dplyr::arrange(report_date)

# Get report descriptions and links by scraping FDA website
# (same approach as download_all_reports.R)
url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

session <- rvest::session(url_report_page)
report_page_links <- rvest::html_nodes(session, "a")

report_metadata <- tibble::tibble(
  report_description = report_page_links |> rvest::html_text(),
  report_link = report_page_links |> rvest::html_attr("href")
) |>
  # Keep PDF links, download links, or archive links
  dplyr::filter(
    stringr::str_detect(
      string = report_link,
      pattern = stringr::regex(
        "(\\.pdf$)|(download)|(pagefreezer\\.com)|(archive-it\\.org)"
      )
    )
  ) |>
  # Prepend fda.gov only for relative paths
  dplyr::mutate(
    report_link = dplyr::case_when(
      stringr::str_detect(report_link, "^https?:") ~ report_link,
      TRUE ~ paste0("https://www.fda.gov", report_link)
    ),
    # Extract date from description
    report_date = stringr::str_extract(
      string = report_description,
      pattern = "^\\w+\\s\\d{1,2},\\s\\d{4}"
    ) |>
      lubridate::mdy(),
    # Extract MDUFA period
    report_mdufa_period = stringr::str_extract(
      string = report_description,
      pattern = "MDUFA\\s[IVXL]+"
    ) |>
      tidyr::replace_na("MDUFA II") |>
      stringr::str_replace("III", "MDUFA III") |>
      stringr::str_replace("IV", "MDUFA IV") |>
      stringr::str_replace("^MDUFA II$", "MDUFA II") |>
      stringr::str_replace("^MDUFA V$", "MDUFA V") |>
      # Normalize to full names
      dplyr::case_match(
        "MDUFA II" ~ "MDUFA II",
        "MDUFA III" ~ "MDUFA III",
        "MDUFA MDUFA III" ~ "MDUFA III",
        "MDUFA MDUFA IV" ~ "MDUFA IV",
        "MDUFA IV" ~ "MDUFA IV",
        "MDUFA V" ~ "MDUFA V",
        .default = "MDUFA II"
      )
  ) |>
  dplyr::filter(!is.na(report_date))

# Join to get descriptions and links
report_dates <- report_dates_raw |>
  dplyr::left_join(
    report_metadata,
    by = c("report_date", "report_mdufa_period"),
    relationship = "many-to-many"
  ) |>
  dplyr::select(
    "report_date",
    "report_cutoff_date",
    "report_mdufa_period",
    "report_description",
    "report_link"
  ) |>
  # Deduplicate after join (FDA page may list same report multiple times)
  dplyr::distinct(report_date, report_mdufa_period, .keep_all = TRUE) |>
  dplyr::arrange(report_date)

# Report summary
cat("\n")
cat("report_dates dataset created\n")
cat("-----------------------------\n")
cat("Total reports:", nrow(report_dates), "\n\n")

cat("Reports by MDUFA period:\n")
print(table(report_dates$report_mdufa_period))

cat(
  "\nReports with cutoff dates:",
  sum(!is.na(report_dates$report_cutoff_date)), "\n"
)
cat(
  "Reports missing cutoff dates:",
  sum(is.na(report_dates$report_cutoff_date)), "\n"
)

cat("\nSample data:\n")
report_dates |>
  dplyr::filter(!is.na(report_cutoff_date)) |>
  tail(10) |>
  print()

# Check for missing cutoffs
missing_cutoffs <- report_dates |>
  dplyr::filter(is.na(report_cutoff_date))

if (nrow(missing_cutoffs) > 0) {
  cat("\nReports with missing cutoff dates:\n")
  print(missing_cutoffs |> dplyr::select(report_date, report_mdufa_period))
}

# Save the dataset
usethis::use_data(report_dates, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------
str_nolint <- " # nolint: line_length_linter."

glimpse_output <- dplyr::glimpse(report_dates, width = 76) |>
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
    "Report Dates",
    "",
    "Mapping of MDUFA quarterly report publication dates to their data cutoff",
    "dates. Each report contains performance data through a specific cutoff",
    "date (typically the end of a fiscal quarter).",
    "",
    paste0(
      "@format A tibble with ",
      nrow(report_dates),
      " rows and ",
      length(report_dates),
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
        "# Do not hand edit this file. Edit data-raw/report_dates.R ",
        "instead."
      ),
      x,
      "\"report_dates\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/report_dates.R",
  append = FALSE
)

devtools::document()

cat("\nreport_dates saved to data/report_dates.rda\n")
cat("Documentation written to R/report_dates.R\n")
