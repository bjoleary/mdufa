## code to prepare `mdufa5` dataset goes here
# Build the dataset ------------------------------------------------------------
# Load libraries
library(rvest)
str_nolint <- " # nolint: line_length_linter."

# Capture installed package version BEFORE load_all() overrides it
old_version <- mdufa::mdufa5

devtools::load_all()

url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

## Scrape FDA website for latest MDUFA V report -----
# Use httr to handle potential connection issues
session <- rvest::session(url_report_page)
report_page_links <-
  rvest::html_nodes(session, "a")

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

# Get the most recent MDUFA V report
current_report <-
  mdufa_reports |>
  dplyr::filter(.data$report_mdufa_period == "MDUFA V") |>
  dplyr::filter(.data$report_date == max(.data$report_date, na.rm = TRUE)) |>
  head(1)

# Strip ?attachment suffix that breaks pdftools
pdf_url <- current_report$report_link |>
  stringr::str_remove("\\?attachment$")

cat("Extracting:", current_report$report_description, "\n")
cat("URL:", pdf_url, "\n\n")

# Download PDF to temp file using httr (handles FDA bot detection better)
temp_pdf <- tempfile(fileext = ".pdf")
resp <- httr::GET(
  pdf_url,
  httr::user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
  httr::write_disk(temp_pdf, overwrite = TRUE)
)
if (httr::status_code(resp) != 200) {
  stop(
    "Failed to download PDF. Status: ", httr::status_code(resp),
    "\nYou may need to connect to Mullvad VPN."
  )
}
cat("Downloaded to:", temp_pdf, "\n\n")

mdufa5 <- extract_report(
  pdf_path = temp_pdf,
  mdufa_period = "MDUFA V",
  report_date = current_report$report_date,
  report_description = current_report$report_description,
  report_link = current_report$report_link
)

# Standardize column order (report metadata first)
mdufa5 <- standardize_columns(mdufa5, mdufa_cols)

# Validate column structure before saving
validate_columns(mdufa5, mdufa_cols, "mdufa5")

usethis::use_data(mdufa5, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(mdufa5, width = 76) |>
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
        "# Do not hand edit this file. Edit data-raw/mdufa5.R ",
        "instead."
      ),
      x,
      "\"mdufa5\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa5.R",
  append = FALSE
)

devtools::document()

# Compare extraction results -----
key_cols <- c(
  "table_number", "organization", "performance_metric", "fy", "value"
)
compare_cols <- key_cols

old_compare <- old_version[, compare_cols] |>
  dplyr::arrange(table_number, organization, performance_metric, fy)
new_compare <- mdufa5[, compare_cols] |>
  dplyr::arrange(table_number, organization, performance_metric, fy)

new_rows <- dplyr::anti_join(new_compare, old_compare, by = key_cols)
removed_rows <- dplyr::anti_join(old_compare, new_compare, by = key_cols)

cat("\n=== MDUFA V Data Comparison ===\n")
cat("Old version:", nrow(old_version), "rows\n")
cat("New version:", nrow(mdufa5), "rows\n")
cat("Net change: ", nrow(mdufa5) - nrow(old_version), " rows\n\n")
cat("Rows added:", nrow(new_rows), "\n")
cat("Rows removed:", nrow(removed_rows), "\n\n")

# Export CSVs for diff viewing
write.csv(old_compare, "/tmp/mdufa5_old.csv", row.names = FALSE)
write.csv(new_compare, "/tmp/mdufa5_new.csv", row.names = FALSE)

cat("To view differences:\n")
cat("  ksdiff /tmp/mdufa5_old.csv /tmp/mdufa5_new.csv\n")
