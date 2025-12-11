## code to prepare `mdufa3_oht` dataset goes here
# Build the dataset ------------------------------------------------------------
str_nolint <- " # nolint: line_length_linter."

devtools::load_all()

# Generate OHT-level data from MDUFA III
# - 1:1 divisions (DCD, DNPMD, DOD, DRH) are renamed to OHTs
# - IVD divisions (DCTD, DIHD, DMD, DMGP) are aggregated to OHT7
mdufa3_oht <- create_mdufa3_oht(mdufa3)

## Write out the result -----
usethis::use_data(mdufa3_oht, overwrite = TRUE)

# Document the dataset ---------------------------------------------------------

url_report_page <- paste0(
  "https://www.fda.gov/industry/",
  "medical-device-user-fee-amendments-mdufa-fees/mdufa-reports"
)

glimpse_output <- dplyr::glimpse(mdufa3_oht, width = 76) |>
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
    "MDUFA III at OHT Level",
    "",
    "FDA's MDUFA III Performance Metrics reorganized at the OHT (Office of",
    "Health Technology) level to enable comparison with MDUFA IV and V data.",
    "",
    "This dataset contains:",
    "\\itemize{",
    "  \\item OHT2 (Cardiovascular): renamed from DCD",
    "  \\item OHT5 (Neurological): renamed from DNPMD",
    "  \\item OHT6 (Orthopedic): renamed from DOD",
    "  \\item OHT7 (IVD): aggregated from DCTD, DIHD, DMD, DMGP",
    "  \\item OHT8 (Radiological): renamed from DRH",
    "}",
    "",
    "OHT7 values are calculated:",
    "\\itemize{",
    "  \\item Count metrics: summed across IVD divisions",
    "  \\item Average metrics: weighted average by decision count",
    "  \\item Percent metrics: recalculated from aggregated counts",
    "  \\item Percentile metrics: excluded (cannot aggregate without raw data)",
    "}",
    "",
    paste0(
      "@format A tibble with ",
      nrow(mdufa3_oht),
      " rows and ",
      length(mdufa3_oht),
      " fields: "
    ),
    "",
    "\\describe{",
    formatted_fields,
    "}",
    "",
    "@source ",
    paste0("[FDA MDUFA Reports](", url_report_page, ")", str_nolint),
    "Derived from mdufa3 dataset.",
    "",
    "@seealso ",
    "\\code{\\link{mdufa3}} for the original division-level data,",
    "\\code{\\link{create_mdufa3_oht}} to regenerate this dataset,",
    "\\code{\\link{combine_mdufa_oht}} to combine with MDUFA IV and V"
  ) |>
  (\(x) paste0("#' ", x))() |>
  (\(x) {
    c(
      paste0(
        "# Do not hand edit this file. Edit data-raw/mdufa3_oht.R ",
        "instead."
      ),
      x,
      "\"mdufa3_oht\""
    )
  })() |>
  stringr::str_squish()

readr::write_lines(
  x = documentation_text,
  file = "R/mdufa3_oht.R",
  append = FALSE
)

devtools::document()

# Summary statistics -----
cat("\n=== MDUFA III OHT Dataset Summary ===\n")
cat("Total rows:", nrow(mdufa3_oht), "\n\n")

cat("Organizations:\n")
print(table(mdufa3_oht$organization))

cat("\nRows by OHT:\n")
oht_counts <- mdufa3_oht |>
  dplyr::count(organization, name = "n_rows") |>
  dplyr::arrange(organization)
print(oht_counts)

cat("\nOHT7 aggregation details:\n")
oht7_data <- mdufa3_oht |>
  dplyr::filter(organization == "OHT7")
cat("  Total OHT7 rows:", nrow(oht7_data), "\n")
cat("  Metric types:\n")
print(table(oht7_data$metric_type))
