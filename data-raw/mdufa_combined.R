# Create combined MDUFA dataset across all periods
#
# This script creates a unified dataset combining data from MDUFA II, III, IV,
# and V with consistent organization naming.
#
# The resulting dataset is a SUPERSET that includes:
# - All original data from mdufa2, mdufa3, mdufa4, mdufa5
# - Derived OHT-level data from mdufa3_oht (for cross-period OHT comparison)
# - An `org` column with full organization names for plotting
# - A `derived` column indicating aggregated vs. directly extracted data
# - A `metric_harmonized` column for cross-period metric comparison

library(dplyr)
library(stringr)
library(readr)
library(lubridate)

devtools::load_all()

str_nolint <- " # nolint: line_length_linter."
url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

# MDUFA II (add derived = FALSE)
m2 <- mdufa2 |>
  dplyr::mutate(derived = FALSE)

# MDUFA III - full dataset with all divisions, centers, offices
# (add derived = FALSE since all directly extracted)
m3 <- mdufa3 |>
  dplyr::mutate(derived = FALSE)

# MDUFA III OHT - derived/aggregated OHT-level data
# (already has derived column from create_mdufa3_oht())
m3_oht <- mdufa3_oht

# MDUFA IV and V (add derived = FALSE)
m4 <- mdufa4 |>
  dplyr::mutate(derived = FALSE)

m5 <- mdufa5 |>
  dplyr::mutate(derived = FALSE)

# Combine all datasets (m3 and m3_oht both included)
mdufa_combined <- dplyr::bind_rows(m2, m3, m3_oht, m4, m5)

# Harmonize program names
mdufa_combined <- mdufa_combined |>
  dplyr::mutate(
    program = dplyr::case_when(
      # Detect DUAL CLIA Waiver from source column and fix program name
      stringr::str_detect(.data$source, "DUAL.*510.*CLIA") ~
        "Dual 510(k) and CLIA Waiver",
      TRUE ~ .data$program
    )
  )

# Remove footnote rows that were incorrectly parsed as metrics
footnote_count <- sum(is_footnote_row(
  mdufa_combined$performance_metric,
  mdufa_combined$value
))
mdufa_combined <- remove_footnote_rows(mdufa_combined)
cat("Removed", footnote_count, "footnote rows\n\n")

# Add full organization names using org_names lookup
# This creates the `org` column used in blog post visualizations
mdufa_combined <- mdufa_combined |>
  dplyr::mutate(
    org = dplyr::case_when(
      # Use org_names lookup for known organizations
      .data$organization %in% names(org_names) ~ org_names[.data$organization],
      # Keep original for any unknown organizations
      TRUE ~ .data$organization
    )
  )

# Apply metric harmonization for cross-period comparison
mdufa_combined <- mdufa_combined |>
  harmonize_metric_names()

# Reorder columns for consistency
mdufa_combined <- mdufa_combined |>
  dplyr::select(
    # Source metadata
    "report_description", "report_link", "report_date", "report_mdufa_period",
    # Table location
    "source", "page", "table_number", "organization", "org", "program",
    "table_title",
    # Metric data
    "metric_type", "performance_metric", "metric_harmonized", "fy", "value",
    # Flags
    "derived"
  )

# Sort for consistent ordering
mdufa_combined <- mdufa_combined |>
  dplyr::arrange(
    .data$report_mdufa_period,
    .data$table_number,
    .data$organization,
    .data$fy
  )

# Validate column structure before saving
validate_columns(mdufa_combined, mdufa_combined_cols, "mdufa_combined")

# Report summary
cat("mdufa_combined dataset created\n")
cat("----------------------------\n")
cat("Total rows:", nrow(mdufa_combined), "\n\n")

cat("Rows by MDUFA period:\n")
print(table(mdufa_combined$report_mdufa_period))

cat("\nOrganizations by period:\n")
mdufa_combined |>
  dplyr::group_by(.data$report_mdufa_period) |>
  dplyr::summarise(
    n_orgs = dplyr::n_distinct(.data$organization),
    orgs = paste(sort(unique(.data$organization)), collapse = ", ")
  ) |>
  print(width = Inf)

cat("\nDerived rows by period and organization:\n")
mdufa_combined |>
  dplyr::filter(.data$derived) |>
  dplyr::group_by(.data$report_mdufa_period, .data$organization) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  print()

# Save the dataset
usethis::use_data(mdufa_combined, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(mdufa_combined, width = 76) |>
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
    "Combined MDUFA Dataset",
    "",
    "A unified dataset combining FDA quarterly Medical Device User Fee",
    "performance metrics from MDUFA II, III, IV, and V. Includes harmonized",
    "organization names and metric names for cross-period comparison.",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa_combined),
      " rows and ",
      length(mdufa_combined),
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
        "# Do not hand edit this file. Edit data-raw/mdufa_combined.R ",
        "instead."
      ),
      x,
      "\"mdufa_combined\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa_combined.R",
  append = FALSE
)

devtools::document()

cat("\nmdufa_combined saved to data/mdufa_combined.rda\n")
cat("Documentation written to R/mdufa_combined.R\n")
