# nolint start
# Verified extraction tests for MDUFA V 2025-02-27 report
# Generated: 2025-12-08
# Verifier: Brendan O'Leary
# Sample size: 36 metrics, 180 values
# Statistical basis: LB of 95% CI > 90% (Wilson score)

# Helper function to find local PDF (works from testthat directory)
find_local_pdf <- function(pattern) {
  # testthat runs from tests/testthat, so go up two levels
  pdf_dir <- testthat::test_path("..", "..", "data-raw", "pdf_reports")
  files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }
  files[1]
}

test_that("MDUFA V 2025-02-27 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2025-02-27")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- extract_report(pdf_path, mdufa_period = "MDUFA V")
  # Table 1.4 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "90"
  )
  # Table 1.4 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "90"
  )
  # Table 1.4 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "74"
  )
  # Table 1.4 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 1.4 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 1.9 | CDRH | Rate of Withdrawal... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2023"],
    "4.69%"
  )
  # Table 1.9 | CDRH | Rate of Withdrawal... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2024"],
    "7.69%"
  )
  # Table 1.9 | CDRH | Rate of Withdrawal... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2025"]
  ))
  # Table 1.9 | CDRH | Rate of Withdrawal... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2026"]
  ))
  # Table 1.9 | CDRH | Rate of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2027"]
  ))
  # Table 1.7 | OHT1 | Average FDA Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Average FDA Days to MDUFA Decision" &
      data$fy == "2023"],
    "144.86"
  )
  # Table 1.7 | OHT1 | Average FDA Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Average FDA Days to MDUFA Decision" &
      data$fy == "2024"],
    "180.00"
  )
  # Table 1.7 | OHT1 | Average FDA Days to MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Average FDA Days to MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 1.7 | OHT1 | Average FDA Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Average FDA Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.7 | OHT1 | Average FDA Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Average FDA Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.3 | OHT2 | SI Pending Within Goal... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT2" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.3 | OHT2 | SI Pending Within Goal... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT2" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2024"],
    "1"
  )
  # Table 1.3 | OHT2 | SI Pending Within Goal... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT2" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2025"],
    "7"
  )
  # Table 1.3 | OHT2 | SI Pending Within Goal... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT2" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2026"]
  ))
  # Table 1.3 | OHT2 | SI Pending Within Goal... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT2" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2027"]
  ))
  # Table 1.7 | OHT6 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023"],
    "179"
  )
  # Table 1.7 | OHT6 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024"],
    "180"
  )
  # Table 1.7 | OHT6 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.7 | OHT6 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.7 | OHT6 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.7 | OHT7 | Average Industry Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2023"],
    "141.17"
  )
  # Table 1.7 | OHT7 | Average Industry Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2024"],
    "39.50"
  )
  # Table 1.7 | OHT7 | Average Industry Days to MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 1.7 | OHT7 | Average Industry Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.7 | OHT7 | Average Industry Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 6.3 | OHT1 | 20th Percentile FDA Days to Substantive ... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "55"
  )
  # Table 6.3 | OHT1 | 20th Percentile FDA Days to Substantive ... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "53"
  )
  # Table 6.3 | OHT1 | 20th Percentile FDA Days to Substantive ... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "48"
  )
  # Table 6.3 | OHT1 | 20th Percentile FDA Days to Substantive ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 6.3 | OHT1 | 20th Percentile FDA Days to Substantive ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT1 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2023"],
    "82"
  )
  # Table 6.5 | OHT1 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2024"],
    "78"
  )
  # Table 6.5 | OHT1 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2025"],
    "21"
  )
  # Table 6.5 | OHT1 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT1 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT1" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.8 | OHT2 | 510(k)s Pending MDUFA V Decision... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT2" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2023"]
  ))
  # Table 6.8 | OHT2 | 510(k)s Pending MDUFA V Decision... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT2" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2024"]
  ))
  # Table 6.8 | OHT2 | 510(k)s Pending MDUFA V Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT2" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2025"]
  ))
  # Table 6.8 | OHT2 | 510(k)s Pending MDUFA V Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT2" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.8 | OHT2 | 510(k)s Pending MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT2" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT4 | 20th Percentile Total Days to MDUFA V De... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2023"],
    "60"
  )
  # Table 6.5 | OHT4 | 20th Percentile Total Days to MDUFA V De... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2024"],
    "55"
  )
  # Table 6.5 | OHT4 | 20th Percentile Total Days to MDUFA V De... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2025"],
    "25"
  )
  # Table 6.5 | OHT4 | 20th Percentile Total Days to MDUFA V De... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT4 | 20th Percentile Total Days to MDUFA V De... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT4 | 60th Percentile Total Days to MDUFA V De... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2023"],
    "127"
  )
  # Table 6.5 | OHT4 | 60th Percentile Total Days to MDUFA V De... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2024"],
    "102"
  )
  # Table 6.5 | OHT4 | 60th Percentile Total Days to MDUFA V De... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2025"],
    "29"
  )
  # Table 6.5 | OHT4 | 60th Percentile Total Days to MDUFA V De... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT4 | 60th Percentile Total Days to MDUFA V De... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.1 | OHT5 | Number Without a RTA or TS Review and <=... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received ¹" &
      data$fy == "2023"],
    "0"
  )
  # Table 6.1 | OHT5 | Number Without a RTA or TS Review and <=... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received ¹" &
      data$fy == "2024"],
    "0"
  )
  # Table 6.1 | OHT5 | Number Without a RTA or TS Review and <=... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received ¹" &
      data$fy == "2025"],
    "14"
  )
  # Table 6.1 | OHT5 | Number Without a RTA or TS Review and <=... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received ¹" &
      data$fy == "2026"]
  ))
  # Table 6.1 | OHT5 | Number Without a RTA or TS Review and <=... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received ¹" &
      data$fy == "2027"]
  ))
  # Table 6.4 | OHT5 | MDUFA V Decision (SE/NSE)... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT5" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2023"],
    "263"
  )
  # Table 6.4 | OHT5 | MDUFA V Decision (SE/NSE)... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT5" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2024"],
    "202"
  )
  # Table 6.4 | OHT5 | MDUFA V Decision (SE/NSE)... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT5" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2025"],
    "7"
  )
  # Table 6.4 | OHT5 | MDUFA V Decision (SE/NSE)... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT5" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2026"]
  ))
  # Table 6.4 | OHT5 | MDUFA V Decision (SE/NSE)... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT5" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2027"]
  ))
  # Table 6.6 | OHT5 | Rate of NSE Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2023"],
    "6.84%"
  )
  # Table 6.6 | OHT5 | Rate of NSE Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2024"],
    "8.42%"
  )
  # Table 6.6 | OHT5 | Rate of NSE Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2025"],
    "0.00%"
  )
  # Table 6.6 | OHT5 | Rate of NSE Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2026"]
  ))
  # Table 6.6 | OHT5 | Rate of NSE Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2027"]
  ))
  # Table 6.2 | OHT6 | SI Pending Over 60 FDA Days... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT6" &
      data$performance_metric == "SI Pending Over 60 FDA Days" &
      data$fy == "2023"],
    "0"
  )
  # Table 6.2 | OHT6 | SI Pending Over 60 FDA Days... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT6" &
      data$performance_metric == "SI Pending Over 60 FDA Days" &
      data$fy == "2024"],
    "0"
  )
  # Table 6.2 | OHT6 | SI Pending Over 60 FDA Days... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT6" &
      data$performance_metric == "SI Pending Over 60 FDA Days" &
      data$fy == "2025"],
    "0"
  )
  # Table 6.2 | OHT6 | SI Pending Over 60 FDA Days... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT6" &
      data$performance_metric == "SI Pending Over 60 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 6.2 | OHT6 | SI Pending Over 60 FDA Days... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT6" &
      data$performance_metric == "SI Pending Over 60 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 6.9 | OHT6 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2023"]
  ))
  # Table 6.9 | OHT6 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2024"]
  ))
  # Table 6.9 | OHT6 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2025"]
  ))
  # Table 6.9 | OHT6 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 6.9 | OHT6 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 6.3 | OHT7 | Maximum FDA Days to Substantive Interact... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "60"
  )
  # Table 6.3 | OHT7 | Maximum FDA Days to Substantive Interact... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "81"
  )
  # Table 6.3 | OHT7 | Maximum FDA Days to Substantive Interact... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "60"
  )
  # Table 6.3 | OHT7 | Maximum FDA Days to Substantive Interact... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 6.3 | OHT7 | Maximum FDA Days to Substantive Interact... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT7 | Average Number of Total Days to MDUFA V ... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Number of Total Days to MDUFA V Decision" &
      data$fy == "2023"],
    "156.55"
  )
  # Table 6.5 | OHT7 | Average Number of Total Days to MDUFA V ... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Number of Total Days to MDUFA V Decision" &
      data$fy == "2024"],
    "118.90"
  )
  # Table 6.5 | OHT7 | Average Number of Total Days to MDUFA V ... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Number of Total Days to MDUFA V Decision" &
      data$fy == "2025"],
    "26.20"
  )
  # Table 6.5 | OHT7 | Average Number of Total Days to MDUFA V ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Number of Total Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT7 | Average Number of Total Days to MDUFA V ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Number of Total Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT7 | 80th Percentile Total Days to MDUFA V De... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2023"],
    "265"
  )
  # Table 6.5 | OHT7 | 80th Percentile Total Days to MDUFA V De... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2024"],
    "204"
  )
  # Table 6.5 | OHT7 | 80th Percentile Total Days to MDUFA V De... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2025"],
    "29"
  )
  # Table 6.5 | OHT7 | 80th Percentile Total Days to MDUFA V De... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT7 | 80th Percentile Total Days to MDUFA V De... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT7" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.6 | OHT7 | Number of Withdrawal... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2023"],
    "28"
  )
  # Table 6.6 | OHT7 | Number of Withdrawal... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2024"],
    "15"
  )
  # Table 6.6 | OHT7 | Number of Withdrawal... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2025"],
    "0"
  )
  # Table 6.6 | OHT7 | Number of Withdrawal... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2026"]
  ))
  # Table 6.6 | OHT7 | Number of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2027"]
  ))
  # Table 6.7 | OHT8 | Number of Submissions that Missed the Go... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "OHT8" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2023"],
    "0"
  )
  # Table 6.7 | OHT8 | Number of Submissions that Missed the Go... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "OHT8" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2024"],
    "0"
  )
  # Table 6.7 | OHT8 | Number of Submissions that Missed the Go... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "OHT8" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2025"],
    "0"
  )
  # Table 6.7 | OHT8 | Number of Submissions that Missed the Go... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.7" &
      data$organization == "OHT8" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 6.7 | OHT8 | Number of Submissions that Missed the Go... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.7" &
      data$organization == "OHT8" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 8.7 | CDRH | MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.7" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2023"],
    "19"
  )
  # Table 8.7 | CDRH | MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.7" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2024"],
    "10"
  )
  # Table 8.7 | CDRH | MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.7" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.7 | CDRH | MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.7 | CDRH | MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.6 | OHT1 | De Novos Pending MDUFA Decision Over 150... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.6" &
      data$organization == "OHT1" &
      data$performance_metric == "De Novos Pending MDUFA Decision Over 150 FDA Days" &
      data$fy == "2023"]
  ))
  # Table 8.6 | OHT1 | De Novos Pending MDUFA Decision Over 150... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.6" &
      data$organization == "OHT1" &
      data$performance_metric == "De Novos Pending MDUFA Decision Over 150 FDA Days" &
      data$fy == "2024"]
  ))
  # Table 8.6 | OHT1 | De Novos Pending MDUFA Decision Over 150... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.6" &
      data$organization == "OHT1" &
      data$performance_metric == "De Novos Pending MDUFA Decision Over 150 FDA Days" &
      data$fy == "2025"]
  ))
  # Table 8.6 | OHT1 | De Novos Pending MDUFA Decision Over 150... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.6" &
      data$organization == "OHT1" &
      data$performance_metric == "De Novos Pending MDUFA Decision Over 150 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 8.6 | OHT1 | De Novos Pending MDUFA Decision Over 150... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.6" &
      data$organization == "OHT1" &
      data$performance_metric == "De Novos Pending MDUFA Decision Over 150 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 8.4 | OHT2 | Rate of Granted Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Granted Decision" &
      data$fy == "2023"],
    "60.00%"
  )
  # Table 8.4 | OHT2 | Rate of Granted Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Granted Decision" &
      data$fy == "2024"],
    "66.67%"
  )
  # Table 8.4 | OHT2 | Rate of Granted Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Granted Decision" &
      data$fy == "2025"]
  ))
  # Table 8.4 | OHT2 | Rate of Granted Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Granted Decision" &
      data$fy == "2026"]
  ))
  # Table 8.4 | OHT2 | Rate of Granted Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Granted Decision" &
      data$fy == "2027"]
  ))
  # Table 8.2 | OHT6 | De Novos Pending MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 8.2 | OHT6 | De Novos Pending MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2024"],
    "2"
  )
  # Table 8.2 | OHT6 | De Novos Pending MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2025"],
    "1"
  )
  # Table 8.2 | OHT6 | De Novos Pending MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.2 | OHT6 | De Novos Pending MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT6 | Maximum Industry Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
      data$fy == "2023"],
    "180"
  )
  # Table 8.3 | OHT6 | Maximum Industry Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.3 | OHT6 | Maximum Industry Days to MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.3 | OHT6 | Maximum Industry Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT6 | Maximum Industry Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.2 | OHT7 | De Novos Pending MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 8.2 | OHT7 | De Novos Pending MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2024"],
    "8"
  )
  # Table 8.2 | OHT7 | De Novos Pending MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2025"],
    "3"
  )
  # Table 8.2 | OHT7 | De Novos Pending MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.2 | OHT7 | De Novos Pending MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.2 | OHT8 | MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2023"],
    "4"
  )
  # Table 8.2 | OHT8 | MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2024"],
    "2"
  )
  # Table 8.2 | OHT8 | MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.2 | OHT8 | MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.2 | OHT8 | MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT8 | Average Total Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "239.25"
  )
  # Table 8.3 | OHT8 | Average Total Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "212.00"
  )
  # Table 8.3 | OHT8 | Average Total Days to MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 8.3 | OHT8 | Average Total Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT8 | Average Total Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT8 | 20th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "190"
  )
  # Table 8.3 | OHT8 | 20th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "150"
  )
  # Table 8.3 | OHT8 | 20th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.3 | OHT8 | 20th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT8 | 20th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT8" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 9.2 | OHT3 | Number in MDUFA Cohort (up to max 4300)⁴... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT3" &
      data$performance_metric == "Number in MDUFA Cohort (up to max 4300)⁴" &
      data$fy == "2023"],
    "443"
  )
  # Table 9.2 | OHT3 | Number in MDUFA Cohort (up to max 4300)⁴... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT3" &
      data$performance_metric == "Number in MDUFA Cohort (up to max 4300)⁴" &
      data$fy == "2024"],
    "485"
  )
  # Table 9.2 | OHT3 | Number in MDUFA Cohort (up to max 4300)⁴... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT3" &
      data$performance_metric == "Number in MDUFA Cohort (up to max 4300)⁴" &
      data$fy == "2025"],
    "112"
  )
  # Table 9.2 | OHT3 | Number in MDUFA Cohort (up to max 4300)⁴... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT3" &
      data$performance_metric == "Number in MDUFA Cohort (up to max 4300)⁴" &
      data$fy == "2026"]
  ))
  # Table 9.2 | OHT3 | Number in MDUFA Cohort (up to max 4300)⁴... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT3" &
      data$performance_metric == "Number in MDUFA Cohort (up to max 4300)⁴" &
      data$fy == "2027"]
  ))
  # Table 9.3 | OHT3 | Average FDA Days to Written Feedback... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.3" &
      data$organization == "OHT3" &
      data$performance_metric == "Average FDA Days to Written Feedback" &
      data$fy == "2023"],
    "62.12"
  )
  # Table 9.3 | OHT3 | Average FDA Days to Written Feedback... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.3" &
      data$organization == "OHT3" &
      data$performance_metric == "Average FDA Days to Written Feedback" &
      data$fy == "2024"],
    "61.75"
  )
  # Table 9.3 | OHT3 | Average FDA Days to Written Feedback... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.3" &
      data$organization == "OHT3" &
      data$performance_metric == "Average FDA Days to Written Feedback" &
      data$fy == "2025"],
    "59.32"
  )
  # Table 9.3 | OHT3 | Average FDA Days to Written Feedback... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.3" &
      data$organization == "OHT3" &
      data$performance_metric == "Average FDA Days to Written Feedback" &
      data$fy == "2026"]
  ))
  # Table 9.3 | OHT3 | Average FDA Days to Written Feedback... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.3" &
      data$organization == "OHT3" &
      data$performance_metric == "Average FDA Days to Written Feedback" &
      data$fy == "2027"]
  ))
  # Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "OHT8" &
      data$performance_metric == "Rate of Submissions Not Accepted for Review on First RTA Cycle" &
      data$fy == "2023"],
    "1.50%"
  )
  # Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "OHT8" &
      data$performance_metric == "Rate of Submissions Not Accepted for Review on First RTA Cycle" &
      data$fy == "2024"],
    "0.00%"
  )
  # Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "OHT8" &
      data$performance_metric == "Rate of Submissions Not Accepted for Review on First RTA Cycle" &
      data$fy == "2025"],
    "1.27%"
  )
  # Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.1" &
      data$organization == "OHT8" &
      data$performance_metric == "Rate of Submissions Not Accepted for Review on First RTA Cycle" &
      data$fy == "2026"]
  ))
  # Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.1" &
      data$organization == "OHT8" &
      data$performance_metric == "Rate of Submissions Not Accepted for Review on First RTA Cycle" &
      data$fy == "2027"]
  ))
  # Table 6.5 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2023"],
    "84"
  )
  # Table 6.5 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2024"],
    "79"
  )
  # Table 6.5 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2025"],
    "26"
  )
  # Table 6.5 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | CBER | Average Industry Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2023"],
    "0.00"
  )
  # Table 8.3 | CBER | Average Industry Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2024"],
    "0.00"
  )
  # Table 8.3 | CBER | Average Industry Days to MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2025"],
    "0.00"
  )
  # Table 8.3 | CBER | Average Industry Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | CBER | Average Industry Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "Average Industry Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 9.5 | CBER | Number of Meetings Required ¹... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.5" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2023"],
    "24"
  )
  # Table 9.5 | CBER | Number of Meetings Required ¹... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.5" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2024"],
    "31"
  )
  # Table 9.5 | CBER | Number of Meetings Required ¹... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.5" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2025"],
    "2"
  )
  # Table 9.5 | CBER | Number of Meetings Required ¹... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.5" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2026"]
  ))
  # Table 9.5 | CBER | Number of Meetings Required ¹... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.5" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2027"]
  ))
})
# nolint end
