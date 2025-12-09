## Download all MDUFA reports from FDA website
library(rvest)
library(httr)
library(dplyr)
library(stringr)
library(purrr)

# Create output directory if it doesn't exist
output_dir <- "data-raw/pdf_reports"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# The FDA MDUFA reports page
url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

# Scrape all links from the page (use session to handle redirects)
session <- rvest::session(url_report_page)
report_page_links <- rvest::html_nodes(session, "a")

# Build a tibble of report info
mdufa_reports <-
  tibble::tibble(
    report_description = report_page_links |> rvest::html_text(),
    report_link = report_page_links |> rvest::html_attr("href")
  ) |>
  # Keep PDF links, download links, or archive links (pagefreezer/archive-it)
  dplyr::filter(
    stringr::str_detect(
      string = report_link,
      pattern = stringr::regex(
        "(\\.pdf$)|(download)|(pagefreezer\\.com)|(archive-it\\.org)"
      )
    )
  ) |>
  # Prepend fda.gov only for relative paths (not archive URLs)
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
    # Extract MDUFA period and convert Roman numerals to Arabic
    mdufa_period = stringr::str_extract(
      string = report_description,
      pattern = "MDUFA\\s[IVXL]+"
    ) |>
      tidyr::replace_na("MDUFA II") |>
      stringr::str_replace("II", "2") |>
      stringr::str_replace("III", "3") |>
      stringr::str_replace("IV", "4") |>
      stringr::str_replace("V", "5"),
    # Create a clean filename
    filename = paste0(
      stringr::str_replace_all(mdufa_period, " ", "-") |> tolower(),
      "_",
      format(report_date, "%Y-%m-%d"),
      "_quarterly-report.pdf"
    )
  ) |>
  # Remove rows without valid dates (non-report links)
  dplyr::filter(!is.na(report_date))

# Show what we found
cat("Found", nrow(mdufa_reports), "reports to download:\n\n")
print(mdufa_reports |> dplyr::select(mdufa_period, report_date, filename))

# Convert pagefreezer URLs to Wayback Machine URLs
# Pagefreezer requires JavaScript; Wayback Machine works with direct downloads
convert_to_wayback <- function(url) {
  if (stringr::str_detect(url, "pagefreezer\\.com")) {
    # Extract the original FDA URL from pagefreezer URL
    # Format: .../browse/FDA/DATE/https://www.fda.gov/...
    original_url <- stringr::str_extract(url, "https://www\\.fda\\.gov/.*$")
    if (!is.na(original_url)) {
      # Use Wayback Machine without specifying year
      return(paste0("https://web.archive.org/web/", original_url))
    }
  }
  url
}

# Download each report using httr (handles FDA bot detection)
download_report <- function(url, filename, output_dir) {
  filepath <- file.path(output_dir, filename)

  # Check if file exists and is valid (> 10KB to catch failed redirects)
  if (file.exists(filepath)) {
    file_size <- file.info(filepath)$size
    if (file_size > 10000) {
      cat("Skipping (exists):", filename, "\n")
      return(invisible(NULL))
    } else {
      cat("Re-downloading (invalid):", filename, "... ")
      file.remove(filepath)
    }
  } else {
    cat("Downloading:", filename, "... ")
  }

  # Remove ?attachment suffix for FDA direct links
  clean_url <- stringr::str_remove(url, "\\?attachment$")

  # Convert pagefreezer URLs to Wayback Machine
  clean_url <- convert_to_wayback(clean_url)

  tryCatch(
    {
      # Use httr with user agent to avoid FDA bot detection
      resp <- httr::GET(
        clean_url,
        httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
        httr::write_disk(filepath, overwrite = TRUE)
      )
      # Verify download succeeded
      if (httr::status_code(resp) == 200 &&
        file.exists(filepath) &&
        file.info(filepath)$size > 10000) {
        cat("Done\n")
      } else {
        cat("FAILED (status:", httr::status_code(resp), ")\n")
      }
    },
    error = function(e) {
      cat("FAILED:", conditionMessage(e), "\n")
    }
  )
}

# Download all reports
cat("\n--- Starting downloads ---\n\n")
purrr::pwalk(
  list(
    url = mdufa_reports$report_link,
    filename = mdufa_reports$filename,
    output_dir = output_dir
  ),
  download_report
)

cat("\n--- Downloads complete ---\n")
cat("Files saved to:", normalizePath(output_dir), "\n")
