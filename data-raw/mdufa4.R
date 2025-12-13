## code to prepare `mdufa4` dataset goes here
# Build the dataset ------------------------------------------------------------
str_nolint <- " # nolint: line_length_linter."

# Capture installed package version BEFORE load_all() overrides it
old_version <- mdufa::mdufa4

devtools::load_all()

url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

## Use local PDF copy -----
# MDUFA IV ended September 30, 2022. The final report was published
# November 16, 2023.
# Update the filename below when refreshing with a new report.
# Run data-raw/download_all_reports.R first to download new reports.
local_pdf <- "data-raw/pdf_reports/mdufa-4_2023-11-16_quarterly-report.pdf"

# Extract metadata from filename
pdf_date <- stringr::str_extract(local_pdf, "\\d{4}-\\d{2}-\\d{2}") |>
  as.Date()

# Extract data using extract_report()
mdufa4 <- extract_report(
  pdf_path = local_pdf,
  mdufa_period = "MDUFA IV",
  report_date = pdf_date,
  report_description = paste0(
    format(pdf_date, "%B %d, %Y"),
    " MDUFA IV Performance Report"
  ),
  report_link = "https://www.fda.gov/media/174193/download"
)

# Standardize column order (report metadata first)
mdufa4 <- standardize_columns(mdufa4, mdufa_cols)

# Validate column structure before saving
validate_columns(mdufa4, mdufa_cols, "mdufa4")

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
    "PDF downloaded 2024-12-06."
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

# Compare extraction results -----
key_cols <- c(
  "table_number", "organization", "performance_metric", "fy", "value"
)
compare_cols <- key_cols

old_compare <- old_version[, compare_cols] |>
  dplyr::arrange(table_number, organization, performance_metric, fy)
new_compare <- mdufa4[, compare_cols] |>
  dplyr::arrange(table_number, organization, performance_metric, fy)

new_rows <- dplyr::anti_join(new_compare, old_compare, by = key_cols)
removed_rows <- dplyr::anti_join(old_compare, new_compare, by = key_cols)

cat("\n=== MDUFA IV Data Comparison ===\n")
cat("Old version:", nrow(old_version), "rows\n")
cat("New version:", nrow(mdufa4), "rows\n")
cat("Net change: ", nrow(mdufa4) - nrow(old_version), " rows\n\n")
cat("Rows added:", nrow(new_rows), "\n")
cat("Rows removed:", nrow(removed_rows), "\n\n")

# Export CSVs for diff viewing
write.csv(old_compare, "/tmp/mdufa4_old.csv", row.names = FALSE)
write.csv(new_compare, "/tmp/mdufa4_new.csv", row.names = FALSE)

cat("To view differences:\n")
cat("  ksdiff /tmp/mdufa4_old.csv /tmp/mdufa4_new.csv\n")
