# Create cohort status dataset for all orgs, programs, and fiscal years
#
# This script calculates cohort closure status from submissions pending
# and number received metrics. The resulting dataset can be joined to
# any MDUFA metrics data for use with plot_quintile_days().
#
# Columns:
# - fy: Fiscal year
# - org: Full organization name (for joining)
# - program: Submission type (510(k), PMA, De Novo, etc.)
# - percent_closed: Proportion of cohort with decisions (0-1)
# - status: "Cohort Closed" or "Cohort Open"

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

# Get submissions pending (most recent report per fy/org/program)
# Filter out NA values - these are future fiscal years without data
# Also capture report_date and report_mdufa_period for matching and coloring
metrics_pending <- find_metrics(mdufa_combined, ".*Pending.MDUFA.*Decision$")

data_pending <- mdufa_combined |>
  dplyr::filter(.data$performance_metric %in% metrics_pending) |>
  convert_integers() |>
  dplyr::filter(!is.na(.data$value)) |>
  dplyr::group_by(.data$fy, .data$org, .data$program) |>
  dplyr::slice_max(.data$report_date, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    "fy", "organization", "org", "program",
    submissions_pending = "value",
    "report_date",
    "report_mdufa_period"
  )

# Get number received (most recent report per fy/org/program)
# Filter out NA values - these are future fiscal years without data
metrics_received <- find_metrics(mdufa_combined, "Number Received")

data_received <- mdufa_combined |>
  dplyr::filter(.data$performance_metric %in% metrics_received) |>
  convert_integers() |>
  dplyr::filter(!is.na(.data$value)) |>
  dplyr::group_by(.data$fy, .data$org, .data$program) |>
  dplyr::slice_max(.data$report_date, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select("fy", "organization", "org", "program", number_received = "value")

# Calculate percent closed
cohort_status <- dplyr::left_join(
  data_pending,
  data_received,
  by = c("fy", "organization", "org", "program")
) |>
  dplyr::mutate(
    percent_closed = (.data$number_received - .data$submissions_pending) /
      .data$number_received,
    status = dplyr::if_else(
      .data$percent_closed >= 1.0,
      "Cohort Closed",
      "Cohort Open"
    )
  ) |>
  dplyr::select("fy", "organization", "org", "program", "percent_closed", "status", "report_date", "report_mdufa_period") |>
  dplyr::arrange(.data$program, .data$org, .data$fy)

# Report summary
cat("cohort_status dataset created\n")
cat("-----------------------------\n")
cat("Total rows:", nrow(cohort_status), "\n\n")

cat("Rows by program:\n")
print(table(cohort_status$program))

cat("\nStatus distribution:\n")
print(table(cohort_status$status))

cat("\nSample data:\n")
cohort_status |>
dplyr::filter(.data$program == "510(k)") |>
  head(20) |>
  print()

# Save the dataset
usethis::use_data(cohort_status, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

glimpse_output <- dplyr::glimpse(cohort_status, width = 76) |>
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
    "Cohort Status",
    "",
    "Cohort closure status calculated from submissions pending and number",
    "received metrics. Used with plot_quintile_days() to indicate whether",
    "a fiscal year cohort has all decisions or is still open.",
    "",
    paste0(
      "@format A tibble with ",
      nrow(cohort_status),
      " rows and ",
      length(cohort_status),
      " fields: "
    ),
    "",
    "\\describe{",
    formatted_fields,
    "}",
    "",
    "@source Derived from mdufa_combined dataset.",
    paste0("Generated ", lubridate::today(), ".")
  ) |>
  (\(x) paste0("#' ", x))() |>
  (\(x) {
    c(
      paste0(
        "# Do not hand edit this file. Edit data-raw/cohort_status.R ",
        "instead."
      ),
      x,
      "\"cohort_status\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/cohort_status.R",
  append = FALSE
)

devtools::document()

cat("\ncohort_status saved to data/cohort_status.rda\n")
cat("Documentation written to R/cohort_status.R\n")
