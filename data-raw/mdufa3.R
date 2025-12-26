## code to prepare `mdufa3` dataset goes here
# Build the dataset ------------------------------------------------------------
# Load libraries
str_nolint <- " # nolint: line_length_linter."

# Capture installed package version BEFORE load_all() overrides it
old_version <- mdufa::mdufa3

devtools::load_all()

## Use local PDF copy -----
# MDUFA III ended September 30, 2017. The final report was published
# December 10, 2018. FDA archive URLs for this report are broken.
url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

local_pdf <- "data-raw/pdf_reports/mdufa-3_2018-12-10_quarterly-report.pdf"

# Extract data using extract_report()
mdufa3 <- extract_report(
  pdf_path = local_pdf,
  mdufa_period = "MDUFA III",
  report_date = as.Date("2018-12-10"),
  report_description = "December 10, 2018 MDUFA III Performance Report",
  report_link = "https://www.fda.gov/media/120475/download"
)

# Standardize column order (report metadata first)
mdufa3 <- standardize_columns(mdufa3, mdufa_cols)

# Validate column structure before saving
validate_columns(mdufa3, mdufa_cols, "mdufa3")

# Validate unique metrics (no duplicates)
validate_unique_metrics(mdufa3, "mdufa3")

## Write out the result -----

usethis::use_data(mdufa3, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(mdufa3, width = 76) |>
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
        "# Do not hand edit this file. Edit data-raw/mdufa3.R ",
        "instead."
      ),
      x,
      "\"mdufa3\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa3.R",
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
new_compare <- mdufa3[, compare_cols] |>
  dplyr::arrange(table_number, organization, performance_metric, fy)

new_rows <- dplyr::anti_join(new_compare, old_compare, by = key_cols)
removed_rows <- dplyr::anti_join(old_compare, new_compare, by = key_cols)

cat("\n=== MDUFA III Data Comparison ===\n")
cat("Old version:", nrow(old_version), "rows\n")
cat("New version:", nrow(mdufa3), "rows\n")
cat("Net change: ", nrow(mdufa3) - nrow(old_version), " rows\n\n")
cat("Rows added:", nrow(new_rows), "\n")
cat("Rows removed:", nrow(removed_rows), "\n\n")

# Export CSVs for diff viewing
write.csv(old_compare, "/tmp/mdufa3_old.csv", row.names = FALSE)
write.csv(new_compare, "/tmp/mdufa3_new.csv", row.names = FALSE)

cat("To view differences:\n")
cat("  ksdiff /tmp/mdufa3_old.csv /tmp/mdufa3_new.csv\n")
