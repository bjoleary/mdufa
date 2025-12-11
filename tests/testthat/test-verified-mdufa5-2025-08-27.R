# nolint start
# Verified extraction tests for MDUFA V 2025-08-27 report
# Generated: 2025-12-08
# Verifier: Brendan O'Leary
# Sample size: 75 metrics, 375 values
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

test_that("MDUFA V 2025-08-27 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2025-08-27")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA V"))
  # Table 1.4 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "91"
  )
  # Table 1.4 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "153"
  )
  # Table 1.4 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "102"
  )
  # Table 1.4 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 1.4 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 1.1 | OHT1 | Number Without a First Cycle RTA Review ... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.1 | OHT1 | Number Without a First Cycle RTA Review ... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.1 | OHT1 | Number Without a First Cycle RTA Review ... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.1 | OHT1 | Number Without a First Cycle RTA Review ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2026"]
  ))
  # Table 1.1 | OHT1 | Number Without a First Cycle RTA Review ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2027"]
  ))
  # Table 1.9 | OHT1 | Number of Not Approvable... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.9 | OHT1 | Number of Not Approvable... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2024"],
    "1"
  )
  # Table 1.9 | OHT1 | Number of Not Approvable... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.9 | OHT1 | Number of Not Approvable... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2026"]
  ))
  # Table 1.9 | OHT1 | Number of Not Approvable... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2027"]
  ))
  # Table 1.12 | OHT1 | Mean FDA Days for Submissions that Misse... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2023"]
  ))
  # Table 1.12 | OHT1 | Mean FDA Days for Submissions that Misse... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2024"]
  ))
  # Table 1.12 | OHT1 | Mean FDA Days for Submissions that Misse... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2025"]
  ))
  # Table 1.12 | OHT1 | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 1.12 | OHT1 | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 1.1 | OHT2 | Rate of Submissions Not Accepted for Fil... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Submissions Not Accepted for Filing Review on First Cycle" &
      data$fy == "2023"],
    "5.00%"
  )
  # Table 1.1 | OHT2 | Rate of Submissions Not Accepted for Fil... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Submissions Not Accepted for Filing Review on First Cycle" &
      data$fy == "2024"],
    "5.00%"
  )
  # Table 1.1 | OHT2 | Rate of Submissions Not Accepted for Fil... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Submissions Not Accepted for Filing Review on First Cycle" &
      data$fy == "2025"],
    "5.88%"
  )
  # Table 1.1 | OHT2 | Rate of Submissions Not Accepted for Fil... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Submissions Not Accepted for Filing Review on First Cycle" &
      data$fy == "2026"]
  ))
  # Table 1.1 | OHT2 | Rate of Submissions Not Accepted for Fil... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.1" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Submissions Not Accepted for Filing Review on First Cycle" &
      data$fy == "2027"]
  ))
  # Table 1.8 | OHT2 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023"],
    "319"
  )
  # Table 1.8 | OHT2 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024"],
    "248"
  )
  # Table 1.8 | OHT2 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.8 | OHT2 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.8 | OHT2 | 20th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.10 | OHT2 | Number Filed... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2023"],
    "3"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2024"],
    "1"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.10 | OHT2 | Number Filed... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2026"]
  ))
  # Table 1.10 | OHT2 | Number Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.10" &
      data$organization == "OHT2" &
      data$performance_metric == "Number Filed" &
      data$fy == "2027"]
  ))
  # Table 1.13 | OHT2 | PMAs Pending MDUFA Decision... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT2" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2023"]
  ))
  # Table 1.13 | OHT2 | PMAs Pending MDUFA Decision... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT2" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2024"]
  ))
  # Table 1.13 | OHT2 | PMAs Pending MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT2" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 1.13 | OHT2 | PMAs Pending MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT2" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.13 | OHT2 | PMAs Pending MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT2" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.4 | OHT3 | 20th Percentile FDA Days to Substantive ... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "87"
  )
  # Table 1.4 | OHT3 | 20th Percentile FDA Days to Substantive ... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "88"
  )
  # Table 1.4 | OHT3 | 20th Percentile FDA Days to Substantive ... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "86"
  )
  # Table 1.4 | OHT3 | 20th Percentile FDA Days to Substantive ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 1.4 | OHT3 | 20th Percentile FDA Days to Substantive ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "20th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 1.4 | OHT3 | Maximum FDA Days to Substantive Interact... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "90"
  )
  # Table 1.4 | OHT3 | Maximum FDA Days to Substantive Interact... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "153"
  )
  # Table 1.4 | OHT3 | Maximum FDA Days to Substantive Interact... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "90"
  )
  # Table 1.4 | OHT3 | Maximum FDA Days to Substantive Interact... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 1.4 | OHT3 | Maximum FDA Days to Substantive Interact... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 1.12 | OHT3 | Mean Industry Days for Submissions that ... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT3" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2023"]
  ))
  # Table 1.12 | OHT3 | Mean Industry Days for Submissions that ... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT3" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2024"]
  ))
  # Table 1.12 | OHT3 | Mean Industry Days for Submissions that ... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT3" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2025"]
  ))
  # Table 1.12 | OHT3 | Mean Industry Days for Submissions that ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT3" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 1.12 | OHT3 | Mean Industry Days for Submissions that ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT3" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 1.14 | OHT3 | Performance Metric... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT3" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2023"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT3 | Performance Metric... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT3" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2024"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT3 | Performance Metric... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT3" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2025"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT3 | Performance Metric... | FY 2026
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT3" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2026"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT3 | Performance Metric... | FY 2027
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT3" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2027"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT4 | Current Performance Percent Goal Met... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT4" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2023"]
  ))
  # Table 1.14 | OHT4 | Current Performance Percent Goal Met... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT4" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2024"]
  ))
  # Table 1.14 | OHT4 | Current Performance Percent Goal Met... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT4" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2025"]
  ))
  # Table 1.14 | OHT4 | Current Performance Percent Goal Met... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT4" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2026"]
  ))
  # Table 1.14 | OHT4 | Current Performance Percent Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT4" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2027"]
  ))
  # Table 1.9 | OHT5 | Rate of Withdrawal... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2023"],
    "0.00%"
  )
  # Table 1.9 | OHT5 | Rate of Withdrawal... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2024"],
    "20.00%"
  )
  # Table 1.9 | OHT5 | Rate of Withdrawal... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2025"]
  ))
  # Table 1.9 | OHT5 | Rate of Withdrawal... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2026"]
  ))
  # Table 1.9 | OHT5 | Rate of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Rate of Withdrawal" &
      data$fy == "2027"]
  ))
  # Table 1.12 | OHT5 | Mean Industry Days for Submissions that ... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2023"]
  ))
  # Table 1.12 | OHT5 | Mean Industry Days for Submissions that ... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2024"]
  ))
  # Table 1.12 | OHT5 | Mean Industry Days for Submissions that ... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2025"]
  ))
  # Table 1.12 | OHT5 | Mean Industry Days for Submissions that ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 1.12 | OHT5 | Mean Industry Days for Submissions that ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.12" &
      data$organization == "OHT5" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 1.7 | OHT6 | 80th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "530"
  )
  # Table 1.7 | OHT6 | 80th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "324"
  )
  # Table 1.7 | OHT6 | 80th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "107"
  )
  # Table 1.7 | OHT6 | 80th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.7 | OHT6 | 80th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.8 | OHT6 | 40th Percentile Industry Days to MDUFA D... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.8 | OHT6 | 40th Percentile Industry Days to MDUFA D... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.8 | OHT6 | 40th Percentile Industry Days to MDUFA D... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.8 | OHT6 | 40th Percentile Industry Days to MDUFA D... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.8 | OHT6 | 40th Percentile Industry Days to MDUFA D... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.8" &
      data$organization == "OHT6" &
      data$performance_metric == "40th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.13 | OHT6 | Non-MDUFA Decision... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT6" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2023"]
  ))
  # Table 1.13 | OHT6 | Non-MDUFA Decision... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT6" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2024"]
  ))
  # Table 1.13 | OHT6 | Non-MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT6" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 1.13 | OHT6 | Non-MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT6" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.13 | OHT6 | Non-MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "OHT6" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.2 | OHT7 | Rate of Submissions Not Filed... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Rate of Submissions Not Filed" &
      data$fy == "2023"],
    "4.76%"
  )
  # Table 1.2 | OHT7 | Rate of Submissions Not Filed... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Rate of Submissions Not Filed" &
      data$fy == "2024"],
    "0.00%"
  )
  # Table 1.2 | OHT7 | Rate of Submissions Not Filed... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Rate of Submissions Not Filed" &
      data$fy == "2025"],
    "0.00%"
  )
  # Table 1.2 | OHT7 | Rate of Submissions Not Filed... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Rate of Submissions Not Filed" &
      data$fy == "2026"]
  ))
  # Table 1.2 | OHT7 | Rate of Submissions Not Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Rate of Submissions Not Filed" &
      data$fy == "2027"]
  ))
  # Table 1.3 | OHT7 | Closed Without SI... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Closed Without SI" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.3 | OHT7 | Closed Without SI... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Closed Without SI" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.3 | OHT7 | Closed Without SI... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Closed Without SI" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.3 | OHT7 | Closed Without SI... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Closed Without SI" &
      data$fy == "2026"]
  ))
  # Table 1.3 | OHT7 | Closed Without SI... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Closed Without SI" &
      data$fy == "2027"]
  ))
  # Table 1.2 | OHT8 | Number Not Filed... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT8" &
      data$performance_metric == "Number Not Filed" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.2 | OHT8 | Number Not Filed... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT8" &
      data$performance_metric == "Number Not Filed" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.2 | OHT8 | Number Not Filed... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT8" &
      data$performance_metric == "Number Not Filed" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.2 | OHT8 | Number Not Filed... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT8" &
      data$performance_metric == "Number Not Filed" &
      data$fy == "2026"]
  ))
  # Table 1.2 | OHT8 | Number Not Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.2" &
      data$organization == "OHT8" &
      data$performance_metric == "Number Not Filed" &
      data$fy == "2027"]
  ))
  # Table 1.3 | OHT8 | SI Goal Not Met... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.3 | OHT8 | SI Goal Not Met... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.3 | OHT8 | SI Goal Not Met... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.3 | OHT8 | SI Goal Not Met... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2026"]
  ))
  # Table 1.3 | OHT8 | SI Goal Not Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.3" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2027"]
  ))
  # Table 1.5 | OHT8 | PMAs Pending MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.5" &
      data$organization == "OHT8" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.5 | OHT8 | PMAs Pending MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.5" &
      data$organization == "OHT8" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2024"],
    "3"
  )
  # Table 1.5 | OHT8 | PMAs Pending MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.5" &
      data$organization == "OHT8" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.5 | OHT8 | PMAs Pending MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.5" &
      data$organization == "OHT8" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 1.5 | OHT8 | PMAs Pending MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.5" &
      data$organization == "OHT8" &
      data$performance_metric == "PMAs Pending MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 1.14 | OHT8 | Performance Metric... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT8" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2023"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT8 | Performance Metric... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT8" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2024"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT8 | Performance Metric... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT8" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2025"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT8 | Performance Metric... | FY 2026
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT8" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2026"],
    "90% Within 320 FDA Days"
  )
  # Table 1.14 | OHT8 | Performance Metric... | FY 2027
  expect_equal(
    data$value[data$table_number == "1.14" &
      data$organization == "OHT8" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2027"],
    "90% Within 320 FDA Days"
  )
  # Table 2.2 | CDRH | Non-MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2023"],
    "8"
  )
  # Table 2.2 | CDRH | Non-MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2024"],
    "6"
  )
  # Table 2.2 | CDRH | Non-MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 2.2 | CDRH | Non-MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 2.2 | CDRH | Non-MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 2.2 | OHT2 | Supplements Pending MDUFA Decision Past ... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Supplements Pending MDUFA Decision Past Goal" &
      data$fy == "2023"],
    "0"
  )
  # Table 2.2 | OHT2 | Supplements Pending MDUFA Decision Past ... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Supplements Pending MDUFA Decision Past Goal" &
      data$fy == "2024"],
    "0"
  )
  # Table 2.2 | OHT2 | Supplements Pending MDUFA Decision Past ... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Supplements Pending MDUFA Decision Past Goal" &
      data$fy == "2025"],
    "0"
  )
  # Table 2.2 | OHT2 | Supplements Pending MDUFA Decision Past ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Supplements Pending MDUFA Decision Past Goal" &
      data$fy == "2026"]
  ))
  # Table 2.2 | OHT2 | Supplements Pending MDUFA Decision Past ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Supplements Pending MDUFA Decision Past Goal" &
      data$fy == "2027"]
  ))
  # Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2023"],
    "95.24%"
  )
  # Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2024"],
    "100.00%"
  )
  # Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2025"],
    "90.91%"
  )
  # Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2026"]
  ))
  # Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Current SI Performance Percent Goal Met" &
      data$fy == "2027"]
  ))
  # Table 2.3 | OHT4 | Number of Not Approvable... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2023"],
    "0"
  )
  # Table 2.3 | OHT4 | Number of Not Approvable... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2024"],
    "0"
  )
  # Table 2.3 | OHT4 | Number of Not Approvable... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2025"],
    "0"
  )
  # Table 2.3 | OHT4 | Number of Not Approvable... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2026"]
  ))
  # Table 2.3 | OHT4 | Number of Not Approvable... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2027"]
  ))
  # Table 2.2 | OHT6 | MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2023"],
    "6"
  )
  # Table 2.2 | OHT6 | MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2024"],
    "5"
  )
  # Table 2.2 | OHT6 | MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2025"],
    "2"
  )
  # Table 2.2 | OHT6 | MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 2.2 | OHT6 | MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 2.2 | OHT6 | Current Performance Percent Goal Met... | FY 2023
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2023"],
    "100.00%"
  )
  # Table 2.2 | OHT6 | Current Performance Percent Goal Met... | FY 2024
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2024"],
    "100.00%"
  )
  # Table 2.2 | OHT6 | Current Performance Percent Goal Met... | FY 2025
  expect_equal(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2025"],
    "100.00%"
  )
  # Table 2.2 | OHT6 | Current Performance Percent Goal Met... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2026"]
  ))
  # Table 2.2 | OHT6 | Current Performance Percent Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "2.2" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Goal Met" &
      data$fy == "2027"]
  ))
  # Table 3.2 | CDRH | Number Received... | FY 2023
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Received" &
      data$fy == "2023"],
    "240"
  )
  # Table 3.2 | CDRH | Number Received... | FY 2024
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Received" &
      data$fy == "2024"],
    "276"
  )
  # Table 3.2 | CDRH | Number Received... | FY 2025
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Received" &
      data$fy == "2025"],
    "199"
  )
  # Table 3.2 | CDRH | Number Received... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Received" &
      data$fy == "2026"]
  ))
  # Table 3.2 | CDRH | Number Received... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Received" &
      data$fy == "2027"]
  ))
  # Table 3.2 | OHT5 | Number Received... | FY 2023
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Received" &
      data$fy == "2023"],
    "16"
  )
  # Table 3.2 | OHT5 | Number Received... | FY 2024
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Received" &
      data$fy == "2024"],
    "37"
  )
  # Table 3.2 | OHT5 | Number Received... | FY 2025
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Received" &
      data$fy == "2025"],
    "39"
  )
  # Table 3.2 | OHT5 | Number Received... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Received" &
      data$fy == "2026"]
  ))
  # Table 3.2 | OHT5 | Number Received... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Received" &
      data$fy == "2027"]
  ))
  # Table 6.3 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "60"
  )
  # Table 6.3 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "60"
  )
  # Table 6.3 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "60"
  )
  # Table 6.3 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 6.3 | CDRH | 80th Percentile FDA Days to Substantive ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 6.3 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "212"
  )
  # Table 6.3 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "95"
  )
  # Table 6.3 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "115"
  )
  # Table 6.3 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 6.3 | CDRH | Maximum FDA Days to Substantive Interact... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 6.5 | CDRH | 20th Percentile Industry Days to MDUFA V... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 6.5 | CDRH | 20th Percentile Industry Days to MDUFA V... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 6.5 | CDRH | 20th Percentile Industry Days to MDUFA V... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 6.5 | CDRH | 20th Percentile Industry Days to MDUFA V... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | CDRH | 20th Percentile Industry Days to MDUFA V... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.6 | CDRH | Rate of NSE Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2023"],
    "4.46%"
  )
  # Table 6.6 | CDRH | Rate of NSE Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2024"],
    "4.43%"
  )
  # Table 6.6 | CDRH | Rate of NSE Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2025"],
    "1.21%"
  )
  # Table 6.6 | CDRH | Rate of NSE Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2026"]
  ))
  # Table 6.6 | CDRH | Rate of NSE Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of NSE Decision" &
      data$fy == "2027"]
  ))
  # Table 6.7 | CDRH | Mean Industry Days for Submissions that ... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2023"],
    "129.15"
  )
  # Table 6.7 | CDRH | Mean Industry Days for Submissions that ... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2024"],
    "132.14"
  )
  # Table 6.7 | CDRH | Mean Industry Days for Submissions that ... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.7" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2025"],
    "27.29"
  )
  # Table 6.7 | CDRH | Mean Industry Days for Submissions that ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.7" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 6.7 | CDRH | Mean Industry Days for Submissions that ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.7" &
      data$organization == "CDRH" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
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
    "49"
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
  # Table 6.2 | OHT2 | Substantive Interaction (SI) Goal... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2023"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | OHT2 | Substantive Interaction (SI) Goal... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2024"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | OHT2 | Substantive Interaction (SI) Goal... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2025"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | OHT2 | Substantive Interaction (SI) Goal... | FY 2026
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2026"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | OHT2 | Substantive Interaction (SI) Goal... | FY 2027
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT2" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2027"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.5 | OHT2 | 80th Percentile Industry Days to MDUFA V... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2023"],
    "155"
  )
  # Table 6.5 | OHT2 | 80th Percentile Industry Days to MDUFA V... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2024"],
    "159"
  )
  # Table 6.5 | OHT2 | 80th Percentile Industry Days to MDUFA V... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2025"],
    "47"
  )
  # Table 6.5 | OHT2 | 80th Percentile Industry Days to MDUFA V... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT2 | 80th Percentile Industry Days to MDUFA V... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.9 | OHT2 | Current Performance Percent Within 90 FD... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Current Performance Percent Within 90 FDA Days" &
      data$fy == "2023"]
  ))
  # Table 6.9 | OHT2 | Current Performance Percent Within 90 FD... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Current Performance Percent Within 90 FDA Days" &
      data$fy == "2024"]
  ))
  # Table 6.9 | OHT2 | Current Performance Percent Within 90 FD... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Current Performance Percent Within 90 FDA Days" &
      data$fy == "2025"]
  ))
  # Table 6.9 | OHT2 | Current Performance Percent Within 90 FD... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Current Performance Percent Within 90 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 6.9 | OHT2 | Current Performance Percent Within 90 FD... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Current Performance Percent Within 90 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 6.2 | OHT5 | Deleted or Withdrawn Prior to SI... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Deleted or Withdrawn Prior to SI" &
      data$fy == "2023"],
    "0"
  )
  # Table 6.2 | OHT5 | Deleted or Withdrawn Prior to SI... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Deleted or Withdrawn Prior to SI" &
      data$fy == "2024"],
    "0"
  )
  # Table 6.2 | OHT5 | Deleted or Withdrawn Prior to SI... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Deleted or Withdrawn Prior to SI" &
      data$fy == "2025"],
    "0"
  )
  # Table 6.2 | OHT5 | Deleted or Withdrawn Prior to SI... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Deleted or Withdrawn Prior to SI" &
      data$fy == "2026"]
  ))
  # Table 6.2 | OHT5 | Deleted or Withdrawn Prior to SI... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.2" &
      data$organization == "OHT5" &
      data$performance_metric == "Deleted or Withdrawn Prior to SI" &
      data$fy == "2027"]
  ))
  # Table 6.3 | OHT5 | 80th Percentile FDA Days to Substantive ... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2023"],
    "60"
  )
  # Table 6.3 | OHT5 | 80th Percentile FDA Days to Substantive ... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2024"],
    "60"
  )
  # Table 6.3 | OHT5 | 80th Percentile FDA Days to Substantive ... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2025"],
    "60"
  )
  # Table 6.3 | OHT5 | 80th Percentile FDA Days to Substantive ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2026"]
  ))
  # Table 6.3 | OHT5 | 80th Percentile FDA Days to Substantive ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.3" &
      data$organization == "OHT5" &
      data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
      data$fy == "2027"]
  ))
  # Table 6.8 | OHT5 | 510(k)s Pending MDUFA V Decision... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2023"]
  ))
  # Table 6.8 | OHT5 | 510(k)s Pending MDUFA V Decision... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2024"]
  ))
  # Table 6.8 | OHT5 | 510(k)s Pending MDUFA V Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2025"]
  ))
  # Table 6.8 | OHT5 | 510(k)s Pending MDUFA V Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.8 | OHT5 | 510(k)s Pending MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.9 | OHT5 | Performance Metric... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2023"],
    "95% Within 90 FDA Days"
  )
  # Table 6.9 | OHT5 | Performance Metric... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2024"],
    "95% Within 90 FDA Days"
  )
  # Table 6.9 | OHT5 | Performance Metric... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2025"],
    "95% Within 90 FDA Days"
  )
  # Table 6.9 | OHT5 | Performance Metric... | FY 2026
  expect_equal(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2026"],
    "95% Within 90 FDA Days"
  )
  # Table 6.9 | OHT5 | Performance Metric... | FY 2027
  expect_equal(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "Performance Metric" &
      data$fy == "2027"],
    "95% Within 90 FDA Days"
  )
  # Table 6.9 | OHT5 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2023"]
  ))
  # Table 6.9 | OHT5 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2024"]
  ))
  # Table 6.9 | OHT5 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2025"]
  ))
  # Table 6.9 | OHT5 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 6.9 | OHT5 | 510(k)s Pending MDUFA V Decision Over 90... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT5" &
      data$performance_metric == "510(k)s Pending MDUFA V Decision Over 90 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 6.4 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2023"],
    "554"
  )
  # Table 6.4 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2024"],
    "492"
  )
  # Table 6.4 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2025"],
    "223"
  )
  # Table 6.4 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2026"]
  ))
  # Table 6.4 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.4" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT6 | Number With MDUFA V Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2023"],
    "554"
  )
  # Table 6.5 | OHT6 | Number With MDUFA V Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2024"],
    "492"
  )
  # Table 6.5 | OHT6 | Number With MDUFA V Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2025"],
    "223"
  )
  # Table 6.5 | OHT6 | Number With MDUFA V Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT6 | Number With MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.6 | OHT6 | Number of Withdrawal... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2023"],
    "37"
  )
  # Table 6.6 | OHT6 | Number of Withdrawal... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2024"],
    "37"
  )
  # Table 6.6 | OHT6 | Number of Withdrawal... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2025"],
    "3"
  )
  # Table 6.6 | OHT6 | Number of Withdrawal... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2026"]
  ))
  # Table 6.6 | OHT6 | Number of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.6" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2027"]
  ))
  # Table 6.9 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2023"]
  ))
  # Table 6.9 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2024"]
  ))
  # Table 6.9 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2025"]
  ))
  # Table 6.9 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2026"]
  ))
  # Table 6.9 | OHT6 | MDUFA V Decision (SE/NSE)... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.9" &
      data$organization == "OHT6" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2027"]
  ))
  # Table 6.5 | OHT8 | Average Number of FDA Days to MDUFA V De... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Number of FDA Days to MDUFA V Decision" &
      data$fy == "2023"],
    "71.52"
  )
  # Table 6.5 | OHT8 | Average Number of FDA Days to MDUFA V De... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Number of FDA Days to MDUFA V Decision" &
      data$fy == "2024"],
    "74.71"
  )
  # Table 6.5 | OHT8 | Average Number of FDA Days to MDUFA V De... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Number of FDA Days to MDUFA V Decision" &
      data$fy == "2025"],
    "70.02"
  )
  # Table 6.5 | OHT8 | Average Number of FDA Days to MDUFA V De... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Number of FDA Days to MDUFA V Decision" &
      data$fy == "2026"]
  ))
  # Table 6.5 | OHT8 | Average Number of FDA Days to MDUFA V De... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Average Number of FDA Days to MDUFA V Decision" &
      data$fy == "2027"]
  ))
  # Table 6.8 | OHT8 | MDUFA V Decision (SE/NSE)... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2023"]
  ))
  # Table 6.8 | OHT8 | MDUFA V Decision (SE/NSE)... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2024"]
  ))
  # Table 6.8 | OHT8 | MDUFA V Decision (SE/NSE)... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2025"]
  ))
  # Table 6.8 | OHT8 | MDUFA V Decision (SE/NSE)... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2026"]
  ))
  # Table 6.8 | OHT8 | MDUFA V Decision (SE/NSE)... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "6.8" &
      data$organization == "OHT8" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2027"]
  ))
  # Table 8.3 | CDRH | 20th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "214"
  )
  # Table 8.3 | CDRH | 20th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "170"
  )
  # Table 8.3 | CDRH | 20th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "150"
  )
  # Table 8.3 | CDRH | 20th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | CDRH | 20th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT2 | 20th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "197"
  )
  # Table 8.3 | OHT2 | 20th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "210"
  )
  # Table 8.3 | OHT2 | 20th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "242"
  )
  # Table 8.3 | OHT2 | 20th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT2 | 20th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.5 | OHT2 | Mean FDA Days for Submissions That Misse... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT2" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2023"]
  ))
  # Table 8.5 | OHT2 | Mean FDA Days for Submissions That Misse... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT2" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2024"]
  ))
  # Table 8.5 | OHT2 | Mean FDA Days for Submissions That Misse... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT2" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2025"]
  ))
  # Table 8.5 | OHT2 | Mean FDA Days for Submissions That Misse... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT2" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 8.5 | OHT2 | Mean FDA Days for Submissions That Misse... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT2" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 8.1 | OHT3 | Number Not Accepted or Failed TS on Firs... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Not Accepted or Failed TS on First Cycle" &
      data$fy == "2023"],
    "2"
  )
  # Table 8.1 | OHT3 | Number Not Accepted or Failed TS on Firs... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Not Accepted or Failed TS on First Cycle" &
      data$fy == "2024"],
    "1"
  )
  # Table 8.1 | OHT3 | Number Not Accepted or Failed TS on Firs... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Not Accepted or Failed TS on First Cycle" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.1 | OHT3 | Number Not Accepted or Failed TS on Firs... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Not Accepted or Failed TS on First Cycle" &
      data$fy == "2026"]
  ))
  # Table 8.1 | OHT3 | Number Not Accepted or Failed TS on Firs... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT3" &
      data$performance_metric == "Number Not Accepted or Failed TS on First Cycle" &
      data$fy == "2027"]
  ))
  # Table 8.4 | OHT3 | Rate of Declined Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Rate of Declined Decision" &
      data$fy == "2023"],
    "9.09%"
  )
  # Table 8.4 | OHT3 | Rate of Declined Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Rate of Declined Decision" &
      data$fy == "2024"],
    "28.57%"
  )
  # Table 8.4 | OHT3 | Rate of Declined Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Rate of Declined Decision" &
      data$fy == "2025"]
  ))
  # Table 8.4 | OHT3 | Rate of Declined Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Rate of Declined Decision" &
      data$fy == "2026"]
  ))
  # Table 8.4 | OHT3 | Rate of Declined Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Rate of Declined Decision" &
      data$fy == "2027"]
  ))
  # Table 8.7 | OHT3 | Non-MDUFA Decision... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT3" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2023"]
  ))
  # Table 8.7 | OHT3 | Non-MDUFA Decision... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT3" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2024"]
  ))
  # Table 8.7 | OHT3 | Non-MDUFA Decision... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT3" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2025"]
  ))
  # Table 8.7 | OHT3 | Non-MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT3" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.7 | OHT3 | Non-MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT3" &
      data$performance_metric == "Non-MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.5 | OHT4 | Mean FDA Days for Submissions That Misse... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT4" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2023"],
    "177.00"
  )
  # Table 8.5 | OHT4 | Mean FDA Days for Submissions That Misse... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT4" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2024"]
  ))
  # Table 8.5 | OHT4 | Mean FDA Days for Submissions That Misse... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT4" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2025"]
  ))
  # Table 8.5 | OHT4 | Mean FDA Days for Submissions That Misse... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT4" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2026"]
  ))
  # Table 8.5 | OHT4 | Mean FDA Days for Submissions That Misse... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.5" &
      data$organization == "OHT4" &
      data$performance_metric == "Mean FDA Days for Submissions That Missed the Goal" &
      data$fy == "2027"]
  ))
  # Table 8.2 | OHT5 | De Novos Pending MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT5" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2023"],
    "0"
  )
  # Table 8.2 | OHT5 | De Novos Pending MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT5" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.2 | OHT5 | De Novos Pending MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT5" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2025"],
    "8"
  )
  # Table 8.2 | OHT5 | De Novos Pending MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT5" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.2 | OHT5 | De Novos Pending MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.2" &
      data$organization == "OHT5" &
      data$performance_metric == "De Novos Pending MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT5 | Maximum FDA Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Maximum FDA Days to MDUFA Decision" &
      data$fy == "2023"],
    "150"
  )
  # Table 8.3 | OHT5 | Maximum FDA Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Maximum FDA Days to MDUFA Decision" &
      data$fy == "2024"],
    "150"
  )
  # Table 8.3 | OHT5 | Maximum FDA Days to MDUFA Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Maximum FDA Days to MDUFA Decision" &
      data$fy == "2025"],
    "150"
  )
  # Table 8.3 | OHT5 | Maximum FDA Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Maximum FDA Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT5 | Maximum FDA Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT5" &
      data$performance_metric == "Maximum FDA Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.1 | OHT6 | Number Without a RTA or TS Review and > ... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT6" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ¹" &
      data$fy == "2023"],
    "0"
  )
  # Table 8.1 | OHT6 | Number Without a RTA or TS Review and > ... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT6" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ¹" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.1 | OHT6 | Number Without a RTA or TS Review and > ... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT6" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ¹" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.1 | OHT6 | Number Without a RTA or TS Review and > ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT6" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ¹" &
      data$fy == "2026"]
  ))
  # Table 8.1 | OHT6 | Number Without a RTA or TS Review and > ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.1" &
      data$organization == "OHT6" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ¹" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT6 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023"],
    "149"
  )
  # Table 8.3 | OHT6 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024"],
    "119"
  )
  # Table 8.3 | OHT6 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025"],
    "57"
  )
  # Table 8.3 | OHT6 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT6 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | OHT6 | 20th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "220"
  )
  # Table 8.3 | OHT6 | 20th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "269"
  )
  # Table 8.3 | OHT6 | 20th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "238"
  )
  # Table 8.3 | OHT6 | 20th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | OHT6 | 20th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.7 | OHT6 | Current Performance Percent Within 150 F... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Within 150 FDA Days" &
      data$fy == "2023"]
  ))
  # Table 8.7 | OHT6 | Current Performance Percent Within 150 F... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Within 150 FDA Days" &
      data$fy == "2024"]
  ))
  # Table 8.7 | OHT6 | Current Performance Percent Within 150 F... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Within 150 FDA Days" &
      data$fy == "2025"]
  ))
  # Table 8.7 | OHT6 | Current Performance Percent Within 150 F... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Within 150 FDA Days" &
      data$fy == "2026"]
  ))
  # Table 8.7 | OHT6 | Current Performance Percent Within 150 F... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.7" &
      data$organization == "OHT6" &
      data$performance_metric == "Current Performance Percent Within 150 FDA Days" &
      data$fy == "2027"]
  ))
  # Table 9.1 | CDRH | Number Closed Before First RTA Action... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Closed Before First RTA Action" &
      data$fy == "2023"],
    "39"
  )
  # Table 9.1 | CDRH | Number Closed Before First RTA Action... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Closed Before First RTA Action" &
      data$fy == "2024"],
    "27"
  )
  # Table 9.1 | CDRH | Number Closed Before First RTA Action... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.1" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Closed Before First RTA Action" &
      data$fy == "2025"],
    "37"
  )
  # Table 9.1 | CDRH | Number Closed Before First RTA Action... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.1" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Closed Before First RTA Action" &
      data$fy == "2026"]
  ))
  # Table 9.1 | CDRH | Number Closed Before First RTA Action... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.1" &
      data$organization == "CDRH" &
      data$performance_metric == "Number Closed Before First RTA Action" &
      data$fy == "2027"]
  ))
  # Table 9.4 | OHT3 | Average Days to Scheduling for Meetings ... | FY 2023
  expect_equal(
    data$value[data$table_number == "9.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Average Days to Scheduling for Meetings Scheduled After Day 30" &
      data$fy == "2023"],
    "41.85"
  )
  # Table 9.4 | OHT3 | Average Days to Scheduling for Meetings ... | FY 2024
  expect_equal(
    data$value[data$table_number == "9.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Average Days to Scheduling for Meetings Scheduled After Day 30" &
      data$fy == "2024"],
    "43.50"
  )
  # Table 9.4 | OHT3 | Average Days to Scheduling for Meetings ... | FY 2025
  expect_equal(
    data$value[data$table_number == "9.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Average Days to Scheduling for Meetings Scheduled After Day 30" &
      data$fy == "2025"],
    "42.80"
  )
  # Table 9.4 | OHT3 | Average Days to Scheduling for Meetings ... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Average Days to Scheduling for Meetings Scheduled After Day 30" &
      data$fy == "2026"]
  ))
  # Table 9.4 | OHT3 | Average Days to Scheduling for Meetings ... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.4" &
      data$organization == "OHT3" &
      data$performance_metric == "Average Days to Scheduling for Meetings Scheduled After Day 30" &
      data$fy == "2027"]
  ))
  # Table 9.2 | OHT5 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2023 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
      data$fy == "2023"]
  ))
  # Table 9.2 | OHT5 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2024 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
      data$fy == "2024"]
  ))
  # Table 9.2 | OHT5 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2025 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
      data$fy == "2025"]
  ))
  # Table 9.2 | OHT5 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
      data$fy == "2026"]
  ))
  # Table 9.2 | OHT5 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "9.2" &
      data$organization == "OHT5" &
      data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
      data$fy == "2027"]
  ))
  # Table 10.1 | OHT1 | Number of IDEs Received... | FY 2023
  expect_equal(
    data$value[data$table_number == "10.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of IDEs Received" &
      data$fy == "2023"],
    "42"
  )
  # Table 10.1 | OHT1 | Number of IDEs Received... | FY 2024
  expect_equal(
    data$value[data$table_number == "10.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of IDEs Received" &
      data$fy == "2024"],
    "32"
  )
  # Table 10.1 | OHT1 | Number of IDEs Received... | FY 2025
  expect_equal(
    data$value[data$table_number == "10.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of IDEs Received" &
      data$fy == "2025"],
    "19"
  )
  # Table 10.1 | OHT1 | Number of IDEs Received... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "10.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of IDEs Received" &
      data$fy == "2026"]
  ))
  # Table 10.1 | OHT1 | Number of IDEs Received... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "10.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number of IDEs Received" &
      data$fy == "2027"]
  ))
  # Table 1.13 | CBER | Number of PMAs Filed... | FY 2023
  expect_equal(
    data$value[data$table_number == "1.13" &
      data$organization == "CBER" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2023"],
    "0"
  )
  # Table 1.13 | CBER | Number of PMAs Filed... | FY 2024
  expect_equal(
    data$value[data$table_number == "1.13" &
      data$organization == "CBER" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2024"],
    "0"
  )
  # Table 1.13 | CBER | Number of PMAs Filed... | FY 2025
  expect_equal(
    data$value[data$table_number == "1.13" &
      data$organization == "CBER" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2025"],
    "0"
  )
  # Table 1.13 | CBER | Number of PMAs Filed... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "CBER" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2026"]
  ))
  # Table 1.13 | CBER | Number of PMAs Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "1.13" &
      data$organization == "CBER" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2027"]
  ))
  # Table 3.2 | CBER | Number of Not Approvable... | FY 2023
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2023"],
    "0"
  )
  # Table 3.2 | CBER | Number of Not Approvable... | FY 2024
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2024"],
    "0"
  )
  # Table 3.2 | CBER | Number of Not Approvable... | FY 2025
  expect_equal(
    data$value[data$table_number == "3.2" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2025"],
    "0"
  )
  # Table 3.2 | CBER | Number of Not Approvable... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2026"]
  ))
  # Table 3.2 | CBER | Number of Not Approvable... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "3.2" &
      data$organization == "CBER" &
      data$performance_metric == "Number of Not Approvable" &
      data$fy == "2027"]
  ))
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2023
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2023"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2024
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2024"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2025
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2025"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2026
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2026"],
    "95% SI Within 60 FDA Days"
  )
  # Table 6.2 | CBER | Substantive Interaction (SI) Goal... | FY 2027
  expect_equal(
    data$value[data$table_number == "6.2" &
      data$organization == "CBER" &
      data$performance_metric == "Substantive Interaction (SI) Goal" &
      data$fy == "2027"],
    "95% SI Within 60 FDA Days"
  )
  # Table 8.3 | CBER | 20th Percentile Industry Days to MDUFA D... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2023"],
    "177"
  )
  # Table 8.3 | CBER | 20th Percentile Industry Days to MDUFA D... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.3 | CBER | 20th Percentile Industry Days to MDUFA D... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.3 | CBER | 20th Percentile Industry Days to MDUFA D... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | CBER | 20th Percentile Industry Days to MDUFA D... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.3 | CBER | 40th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023"],
    "252"
  )
  # Table 8.3 | CBER | 40th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.3 | CBER | 40th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.3 | CBER | 40th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026"]
  ))
  # Table 8.3 | CBER | 40th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.3" &
      data$organization == "CBER" &
      data$performance_metric == "40th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027"]
  ))
  # Table 8.4 | CBER | Number With Declined Decision... | FY 2023
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "CBER" &
      data$performance_metric == "Number With Declined Decision" &
      data$fy == "2023"],
    "1"
  )
  # Table 8.4 | CBER | Number With Declined Decision... | FY 2024
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "CBER" &
      data$performance_metric == "Number With Declined Decision" &
      data$fy == "2024"],
    "0"
  )
  # Table 8.4 | CBER | Number With Declined Decision... | FY 2025
  expect_equal(
    data$value[data$table_number == "8.4" &
      data$organization == "CBER" &
      data$performance_metric == "Number With Declined Decision" &
      data$fy == "2025"],
    "0"
  )
  # Table 8.4 | CBER | Number With Declined Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "CBER" &
      data$performance_metric == "Number With Declined Decision" &
      data$fy == "2026"]
  ))
  # Table 8.4 | CBER | Number With Declined Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[data$table_number == "8.4" &
      data$organization == "CBER" &
      data$performance_metric == "Number With Declined Decision" &
      data$fy == "2027"]
  ))
})
# nolint end
