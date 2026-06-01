# nolint start
# Verified extraction tests for MDUFA V 2026-05-19 report
# Generated: 2026-06-01
# Verifier: Brendan O'Leary
# Sample size: 35 metrics, 175 values
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

test_that("MDUFA V 2026-05-19 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2026-05-19")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA V"))
  # Table 1.10 | CDRH | Number of Withdrawal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "CDRH" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2023")],
    "0"
  )
  # Table 1.10 | CDRH | Number of Withdrawal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "CDRH" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2024")],
    "0"
  )
  # Table 1.10 | CDRH | Number of Withdrawal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "CDRH" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.10 | CDRH | Number of Withdrawal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "CDRH" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.10 | CDRH | Number of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.10" &
      data$organization == "CDRH" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2027")]
  ))
  # Table 1.3 | OHT1 | SI Pending Within Goal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.3" &
      data$organization == "OHT1" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2023")],
    "0"
  )
  # Table 1.3 | OHT1 | SI Pending Within Goal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.3" &
      data$organization == "OHT1" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2024")],
    "0"
  )
  # Table 1.3 | OHT1 | SI Pending Within Goal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.3" &
      data$organization == "OHT1" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.3 | OHT1 | SI Pending Within Goal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.3" &
      data$organization == "OHT1" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2026")],
    "2"
  )
  # Table 1.3 | OHT1 | SI Pending Within Goal... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.3" &
      data$organization == "OHT1" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2027")]
  ))
  # Table 1.10 | OHT2 | Number Filed... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2023")],
    "3"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2024")],
    "1"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2027")]
  ))
  # Table 1.7 | OHT5 | 40th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT5" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023")],
    "217"
  )
  # Table 1.7 | OHT5 | 40th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT5" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024")],
    "261"
  )
  # Table 1.7 | OHT5 | 40th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT5" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025")],
    "301"
  )
  # Table 1.7 | OHT5 | 40th Percentile Total Days to MDUFA Deci... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT5" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.7 | OHT5 | 40th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT5" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 1.8 | OHT6 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023")],
    "0"
  )
  # Table 1.8 | OHT6 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024")],
    "0"
  )
  # Table 1.8 | OHT6 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.8 | OHT6 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.8 | OHT6 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 1.7 | OHT8 | 40th Percentile Industry Days to MDUFA D... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT8" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2023")],
    "0"
  )
  # Table 1.7 | OHT8 | 40th Percentile Industry Days to MDUFA D... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT8" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2024")],
    "340"
  )
  # Table 1.7 | OHT8 | 40th Percentile Industry Days to MDUFA D... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT8" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.7 | OHT8 | 40th Percentile Industry Days to MDUFA D... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT8" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.7 | OHT8 | 40th Percentile Industry Days to MDUFA D... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT8" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 2.1 | OHT1 | Substantive Interaction (SI) Goal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2023")],
    "95% SI Within 90 FDA Days"
  )
  # Table 2.1 | OHT1 | Substantive Interaction (SI) Goal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2024")],
    "95% SI Within 90 FDA Days"
  )
  # Table 2.1 | OHT1 | Substantive Interaction (SI) Goal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2025")],
    "95% SI Within 90 FDA Days"
  )
  # Table 2.1 | OHT1 | Substantive Interaction (SI) Goal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2026")],
    "95% SI Within 90 FDA Days"
  )
  # Table 2.1 | OHT1 | Substantive Interaction (SI) Goal... | FY 2027
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2027")],
    "95% SI Within 90 FDA Days"
  )
  # Table 2.1 | OHT2 | Current SI Performance Percent Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2023")],
    "100.00%"
  )
  # Table 2.1 | OHT2 | Current SI Performance Percent Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2024")],
    "100.00%"
  )
  # Table 2.1 | OHT2 | Current SI Performance Percent Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2025")],
    "94.67%"
  )
  # Table 2.1 | OHT2 | Current SI Performance Percent Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2026")],
    "100.00%"
  )
  # Table 2.1 | OHT2 | Current SI Performance Percent Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2027")]
  ))
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2023")],
    "20"
  )
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2024")],
    "17"
  )
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2025")],
    "18"
  )
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2026")],
    "5"
  )
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2027")]
  ))
  # Table 2.1 | OHT8 | SI Pending Within Goal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2023")],
    "0"
  )
  # Table 2.1 | OHT8 | SI Pending Within Goal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2024")],
    "0"
  )
  # Table 2.1 | OHT8 | SI Pending Within Goal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2025")],
    "0"
  )
  # Table 2.1 | OHT8 | SI Pending Within Goal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2026")],
    "0"
  )
  # Table 2.1 | OHT8 | SI Pending Within Goal... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Pending Within Goal" &
      data$fy == "2027")]
  ))
  # Table 3.3 | CDRH | Mean FDA Days for Submissions that Misse... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2023")],
    "109.50"
  )
  # Table 3.3 | CDRH | Mean FDA Days for Submissions that Misse... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2024")],
    "119.50"
  )
  # Table 3.3 | CDRH | Mean FDA Days for Submissions that Misse... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2025")],
    "102.00"
  )
  # Table 3.3 | CDRH | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "3.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2026")]
  ))
  # Table 3.3 | CDRH | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "3.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2027")]
  ))
  # Table 3.1 | OHT1 | Current Performance Percent Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2023")],
    "100.00%"
  )
  # Table 3.1 | OHT1 | Current Performance Percent Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2024")],
    "100.00%"
  )
  # Table 3.1 | OHT1 | Current Performance Percent Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2025")],
    "100.00%"
  )
  # Table 3.1 | OHT1 | Current Performance Percent Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2026")],
    "100.00%"
  )
  # Table 3.1 | OHT1 | Current Performance Percent Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2027")]
  ))
  # Table 3.1 | OHT3 | MDUFA Decision Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2023")],
    "18"
  )
  # Table 3.1 | OHT3 | MDUFA Decision Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2024")],
    "21"
  )
  # Table 3.1 | OHT3 | MDUFA Decision Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2025")],
    "17"
  )
  # Table 3.1 | OHT3 | MDUFA Decision Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2026")],
    "8"
  )
  # Table 3.1 | OHT3 | MDUFA Decision Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "3.1" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2027")]
  ))
  # Table 3.3 | OHT5 | Number of Submissions that Missed the Go... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2023")],
    "1"
  )
  # Table 3.3 | OHT5 | Number of Submissions that Missed the Go... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2024")],
    "1"
  )
  # Table 3.3 | OHT5 | Number of Submissions that Missed the Go... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2025")],
    "0"
  )
  # Table 3.3 | OHT5 | Number of Submissions that Missed the Go... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "3.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2026")],
    "0"
  )
  # Table 3.3 | OHT5 | Number of Submissions that Missed the Go... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "3.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2027")]
  ))
  # Table 6.5 | CDRH | Maximum FDA Days to MDUFA V Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
      data$fy == "2023")],
    "448"
  )
  # Table 6.5 | CDRH | Maximum FDA Days to MDUFA V Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
      data$fy == "2024")],
    "349"
  )
  # Table 6.5 | CDRH | Maximum FDA Days to MDUFA V Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
      data$fy == "2025")],
    "301"
  )
  # Table 6.5 | CDRH | Maximum FDA Days to MDUFA V Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
      data$fy == "2026")],
    "90"
  )
  # Table 6.5 | CDRH | Maximum FDA Days to MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
      data$fy == "2027")]
  ))
  # Table 6.7 | OHT1 | Number of Submissions that Missed the Go... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2023")],
    "10"
  )
  # Table 6.7 | OHT1 | Number of Submissions that Missed the Go... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2024")],
    "6"
  )
  # Table 6.7 | OHT1 | Number of Submissions that Missed the Go... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2025")],
    "7"
  )
  # Table 6.7 | OHT1 | Number of Submissions that Missed the Go... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2026")],
    "0"
  )
  # Table 6.7 | OHT1 | Number of Submissions that Missed the Go... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Submissions that Missed the Goal" &
      data$fy == "2027")]
  ))
  # Table 6.3 | OHT4 | Average Number of FDA Days to Substantiv... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Average Number of FDA Days to Substantive Interaction" &
      data$fy == "2023")],
    "52.62"
  )
  # Table 6.3 | OHT4 | Average Number of FDA Days to Substantiv... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Average Number of FDA Days to Substantive Interaction" &
      data$fy == "2024")],
    "52.84"
  )
  # Table 6.3 | OHT4 | Average Number of FDA Days to Substantiv... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Average Number of FDA Days to Substantive Interaction" &
      data$fy == "2025")],
    "53.05"
  )
  # Table 6.3 | OHT4 | Average Number of FDA Days to Substantiv... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Average Number of FDA Days to Substantive Interaction" &
      data$fy == "2026")],
    "50.00"
  )
  # Table 6.3 | OHT4 | Average Number of FDA Days to Substantiv... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Average Number of FDA Days to Substantive Interaction" &
      data$fy == "2027")]
  ))
  # Table 6.5 | OHT4 | 80th Percentile Total Days to MDUFA V De... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2023")],
    "211"
  )
  # Table 6.5 | OHT4 | 80th Percentile Total Days to MDUFA V De... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2024")],
    "205"
  )
  # Table 6.5 | OHT4 | 80th Percentile Total Days to MDUFA V De... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2025")],
    "184"
  )
  # Table 6.5 | OHT4 | 80th Percentile Total Days to MDUFA V De... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2026")],
    "89"
  )
  # Table 6.5 | OHT4 | 80th Percentile Total Days to MDUFA V De... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT4" &
      data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
      data$fy == "2027")]
  ))
  # Table 6.7 | OHT5 | Mean FDA Days for Submissions that Misse... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2023")],
    "175.00"
  )
  # Table 6.7 | OHT5 | Mean FDA Days for Submissions that Misse... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2024")],
    "103.11"
  )
  # Table 6.7 | OHT5 | Mean FDA Days for Submissions that Misse... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2025")],
    "96.67"
  )
  # Table 6.7 | OHT5 | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2026")]
  ))
  # Table 6.7 | OHT5 | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT1 | Maximum Total Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "Maximum Total Days to MDUFA Decision" &
      data$fy == "2023")],
    "343"
  )
  # Table 8.3 | OHT1 | Maximum Total Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "Maximum Total Days to MDUFA Decision" &
      data$fy == "2024")],
    "328"
  )
  # Table 8.3 | OHT1 | Maximum Total Days to MDUFA Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "Maximum Total Days to MDUFA Decision" &
      data$fy == "2025")],
    "338"
  )
  # Table 8.3 | OHT1 | Maximum Total Days to MDUFA Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "Maximum Total Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.3 | OHT1 | Maximum Total Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "Maximum Total Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.1 | OHT2 | Number Without a RTA or TS Review and <=... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received" &
      data$fy == "2023")],
    "0"
  )
  # Table 8.1 | OHT2 | Number Without a RTA or TS Review and <=... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received" &
      data$fy == "2024")],
    "0"
  )
  # Table 8.1 | OHT2 | Number Without a RTA or TS Review and <=... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received" &
      data$fy == "2025")],
    "0"
  )
  # Table 8.1 | OHT2 | Number Without a RTA or TS Review and <=... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received" &
      data$fy == "2026")],
    "1"
  )
  # Table 8.1 | OHT2 | Number Without a RTA or TS Review and <=... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Without a RTA or TS Review and <= 15 Days Since Date Received" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT3 | 60th Percentile Industry Days to MDUFA D... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT3" &
      data$performance_metric == "60th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2023")],
    "163"
  )
  # Table 8.3 | OHT3 | 60th Percentile Industry Days to MDUFA D... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT3" &
      data$performance_metric == "60th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2024")],
    "180"
  )
  # Table 8.3 | OHT3 | 60th Percentile Industry Days to MDUFA D... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT3" &
      data$performance_metric == "60th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2025")],
    "164"
  )
  # Table 8.3 | OHT3 | 60th Percentile Industry Days to MDUFA D... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT3" &
      data$performance_metric == "60th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.3 | OHT3 | 60th Percentile Industry Days to MDUFA D... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT3" &
      data$performance_metric == "60th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT5 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023")],
    "150"
  )
  # Table 8.3 | OHT5 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024")],
    "150"
  )
  # Table 8.3 | OHT5 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025")],
    "150"
  )
  # Table 8.3 | OHT5 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.3 | OHT5 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.2 | OHT6 | MDUFA Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2023")],
    "3"
  )
  # Table 8.2 | OHT6 | MDUFA Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2024")],
    "2"
  )
  # Table 8.2 | OHT6 | MDUFA Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2025")],
    "3"
  )
  # Table 8.2 | OHT6 | MDUFA Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.2 | OHT6 | MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT7 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023")],
    "85"
  )
  # Table 8.3 | OHT7 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024")],
    "73"
  )
  # Table 8.3 | OHT7 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025")],
    "144"
  )
  # Table 8.3 | OHT7 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.3 | OHT7 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 9.5 | OHT1 | Percent of Submissions With Meetings for... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Percent of Submissions With Meetings for Which Industry Provided Minutes Within 15 Days" &
      data$fy == "2023")],
    "71.89%"
  )
  # Table 9.5 | OHT1 | Percent of Submissions With Meetings for... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Percent of Submissions With Meetings for Which Industry Provided Minutes Within 15 Days" &
      data$fy == "2024")],
    "82.18%"
  )
  # Table 9.5 | OHT1 | Percent of Submissions With Meetings for... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Percent of Submissions With Meetings for Which Industry Provided Minutes Within 15 Days" &
      data$fy == "2025")],
    "80.28%"
  )
  # Table 9.5 | OHT1 | Percent of Submissions With Meetings for... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Percent of Submissions With Meetings for Which Industry Provided Minutes Within 15 Days" &
      data$fy == "2026")],
    "74.19%"
  )
  # Table 9.5 | OHT1 | Percent of Submissions With Meetings for... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Percent of Submissions With Meetings for Which Industry Provided Minutes Within 15 Days" &
      data$fy == "2027")]
  ))
  # Table 9.2 | OHT2 | Number with Non-MDUFA Action ³... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Number with Non-MDUFA Action ³" &
      data$fy == "2023")],
    "4"
  )
  # Table 9.2 | OHT2 | Number with Non-MDUFA Action ³... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Number with Non-MDUFA Action ³" &
      data$fy == "2024")],
    "4"
  )
  # Table 9.2 | OHT2 | Number with Non-MDUFA Action ³... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Number with Non-MDUFA Action ³" &
      data$fy == "2025")],
    "4"
  )
  # Table 9.2 | OHT2 | Number with Non-MDUFA Action ³... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Number with Non-MDUFA Action ³" &
      data$fy == "2026")],
    "1"
  )
  # Table 9.2 | OHT2 | Number with Non-MDUFA Action ³... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Number with Non-MDUFA Action ³" &
      data$fy == "2027")]
  ))
  # Table 9.1 | OHT3 | Number Accepted First RTA Cycle ¹... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Accepted First RTA Cycle ¹" &
      data$fy == "2023")],
    "438"
  )
  # Table 9.1 | OHT3 | Number Accepted First RTA Cycle ¹... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Accepted First RTA Cycle ¹" &
      data$fy == "2024")],
    "484"
  )
  # Table 9.1 | OHT3 | Number Accepted First RTA Cycle ¹... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Accepted First RTA Cycle ¹" &
      data$fy == "2025")],
    "522"
  )
  # Table 9.1 | OHT3 | Number Accepted First RTA Cycle ¹... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Accepted First RTA Cycle ¹" &
      data$fy == "2026")],
    "279"
  )
  # Table 9.1 | OHT3 | Number Accepted First RTA Cycle ¹... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Accepted First RTA Cycle ¹" &
      data$fy == "2027")]
  ))
  # Table 9.2 | OHT5 | Number Accepted / Eligible for MDUFA Act... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Accepted / Eligible for MDUFA Action" &
      data$fy == "2023")],
    "390"
  )
  # Table 9.2 | OHT5 | Number Accepted / Eligible for MDUFA Act... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Accepted / Eligible for MDUFA Action" &
      data$fy == "2024")],
    "422"
  )
  # Table 9.2 | OHT5 | Number Accepted / Eligible for MDUFA Act... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Accepted / Eligible for MDUFA Action" &
      data$fy == "2025")],
    "459"
  )
  # Table 9.2 | OHT5 | Number Accepted / Eligible for MDUFA Act... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Accepted / Eligible for MDUFA Action" &
      data$fy == "2026")],
    "258"
  )
  # Table 9.2 | OHT5 | Number Accepted / Eligible for MDUFA Act... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Accepted / Eligible for MDUFA Action" &
      data$fy == "2027")]
  ))
  # Table 9.3 | OHT6 | 20th Percentile FDA Days to Written Feed... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to Written Feedback" &
      data$fy == "2023")],
    "45"
  )
  # Table 9.3 | OHT6 | 20th Percentile FDA Days to Written Feed... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to Written Feedback" &
      data$fy == "2024")],
    "43"
  )
  # Table 9.3 | OHT6 | 20th Percentile FDA Days to Written Feed... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to Written Feedback" &
      data$fy == "2025")],
    "45"
  )
  # Table 9.3 | OHT6 | 20th Percentile FDA Days to Written Feed... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to Written Feedback" &
      data$fy == "2026")],
    "43"
  )
  # Table 9.3 | OHT6 | 20th Percentile FDA Days to Written Feed... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile FDA Days to Written Feedback" &
      data$fy == "2027")]
  ))
  # Table 9.1 | OHT7 | Number Without First Cycle RTA Review an... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2023")],
    "35"
  )
  # Table 9.1 | OHT7 | Number Without First Cycle RTA Review an... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2024")],
    "15"
  )
  # Table 9.1 | OHT7 | Number Without First Cycle RTA Review an... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2025")],
    "28"
  )
  # Table 9.1 | OHT7 | Number Without First Cycle RTA Review an... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2026")],
    "11"
  )
  # Table 9.1 | OHT7 | Number Without First Cycle RTA Review an... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2027")]
  ))
  # Table 1.5 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "CBER" &
      data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
      data$fy == "2023")],
    "0"
  )
  # Table 1.5 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "CBER" &
      data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
      data$fy == "2024")],
    "0"
  )
  # Table 1.5 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "CBER" &
      data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.5 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "CBER" &
      data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.5 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.5" &
      data$organization == "CBER" &
      data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
      data$fy == "2027")]
  ))
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2023")],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2024")],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2025")],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2026")],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2027
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2027")],
    "95% SI Within 60 FDA Days"
  )
  # Table 8.7 | CBER | MDUFA Decision Within 150 FDA Days... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.7" &
      data$organization == "CBER" &
      data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
      data$fy == "2023")],
    "0"
  )
  # Table 8.7 | CBER | MDUFA Decision Within 150 FDA Days... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.7" &
      data$organization == "CBER" &
      data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
      data$fy == "2024")],
    "0"
  )
  # Table 8.7 | CBER | MDUFA Decision Within 150 FDA Days... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.7" &
      data$organization == "CBER" &
      data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
      data$fy == "2025")],
    "0"
  )
  # Table 8.7 | CBER | MDUFA Decision Within 150 FDA Days... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.7" &
      data$organization == "CBER" &
      data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.7 | CBER | MDUFA Decision Within 150 FDA Days... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.7" &
      data$organization == "CBER" &
      data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
      data$fy == "2027")]
  ))
  # Table 9.1 | CBER | Number Without First Cycle RTA Review an... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2023")],
    "2"
  )
  # Table 9.1 | CBER | Number Without First Cycle RTA Review an... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2024")],
    "0"
  )
  # Table 9.1 | CBER | Number Without First Cycle RTA Review an... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2025")],
    "0"
  )
  # Table 9.1 | CBER | Number Without First Cycle RTA Review an... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2026")],
    "0"
  )
  # Table 9.1 | CBER | Number Without First Cycle RTA Review an... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Without First Cycle RTA Review and > 15 Days Since Date Received ²" &
      data$fy == "2027")]
  ))
})
# nolint end
