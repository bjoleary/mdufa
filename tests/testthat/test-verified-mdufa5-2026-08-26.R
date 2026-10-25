# nolint start
# Verified extraction tests for MDUFA V 2026-08-26 report
# Generated: 2026-10-24
# Verifier: Brendan O'Leary
# Sample size: 35 metrics, 175 values
# Statistical basis: LB of 95% CI > 90% (Wilson score)
#
# find_local_pdf() is provided by tests/testthat/helper-pdf.R (S3 fallback).

test_that("MDUFA V 2026-08-26 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2026-08-26")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA V"))
  # Table 1.9 | OHT2 | Rate of Not Approvable... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2023")],
    "17.65%"
  )
  # Table 1.9 | OHT2 | Rate of Not Approvable... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2024")],
    "0.00%"
  )
  # Table 1.9 | OHT2 | Rate of Not Approvable... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2025")],
    "17.39%"
  )
  # Table 1.9 | OHT2 | Rate of Not Approvable... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2026")],
    "0.00%"
  )
  # Table 1.9 | OHT2 | Rate of Not Approvable... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.9" &
      data$organization == "OHT2" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2027")]
  ))
  # Table 1.5 | OHT3 | MDUFA Decision Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2023")],
    "2"
  )
  # Table 1.5 | OHT3 | MDUFA Decision Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2024")],
    "6"
  )
  # Table 1.5 | OHT3 | MDUFA Decision Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2025")],
    "3"
  )
  # Table 1.5 | OHT3 | MDUFA Decision Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.5 | OHT3 | MDUFA Decision Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2027")]
  ))
  # Table 1.5 | OHT6 | Number of PMAs Filed... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2023")],
    "6"
  )
  # Table 1.5 | OHT6 | Number of PMAs Filed... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2024")],
    "3"
  )
  # Table 1.5 | OHT6 | Number of PMAs Filed... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2025")],
    "4"
  )
  # Table 1.5 | OHT6 | Number of PMAs Filed... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2026")],
    "1"
  )
  # Table 1.5 | OHT6 | Number of PMAs Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.5" &
      data$organization == "OHT6" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2027")]
  ))
  # Table 1.7 | OHT6 | 20th Percentile Industry Days to MDUFA D... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2023")],
    "96"
  )
  # Table 1.7 | OHT6 | 20th Percentile Industry Days to MDUFA D... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2024")],
    "144"
  )
  # Table 1.7 | OHT6 | 20th Percentile Industry Days to MDUFA D... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2025")],
    "0"
  )
  # Table 1.7 | OHT6 | 20th Percentile Industry Days to MDUFA D... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.7 | OHT6 | 20th Percentile Industry Days to MDUFA D... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.7" &
      data$organization == "OHT6" &
      data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 1.13 | OHT7 | Number of PMAs Filed... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "1.13" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2023")],
    "6"
  )
  # Table 1.13 | OHT7 | Number of PMAs Filed... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "1.13" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2024")],
    "4"
  )
  # Table 1.13 | OHT7 | Number of PMAs Filed... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "1.13" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2025")],
    "4"
  )
  # Table 1.13 | OHT7 | Number of PMAs Filed... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "1.13" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2026")],
    "0"
  )
  # Table 1.13 | OHT7 | Number of PMAs Filed... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "1.13" &
      data$organization == "OHT7" &
      data$performance_metric == "Number of PMAs Filed" &
      data$fy == "2027")]
  ))
  # Table 2.2 | CDRH | MDUFA Decision Goal Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2023")],
    "157"
  )
  # Table 2.2 | CDRH | MDUFA Decision Goal Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2024")],
    "178"
  )
  # Table 2.2 | CDRH | MDUFA Decision Goal Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2025")],
    "168"
  )
  # Table 2.2 | CDRH | MDUFA Decision Goal Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2026")],
    "25"
  )
  # Table 2.2 | CDRH | MDUFA Decision Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.2" &
      data$organization == "CDRH" &
      data$performance_metric == "MDUFA Decision Goal Met" &
      data$fy == "2027")]
  ))
  # Table 2.3 | CDRH | Rate of Not Approvable... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2023")],
    "4.40%"
  )
  # Table 2.3 | CDRH | Rate of Not Approvable... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2024")],
    "8.38%"
  )
  # Table 2.3 | CDRH | Rate of Not Approvable... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2025")],
    "5.85%"
  )
  # Table 2.3 | CDRH | Rate of Not Approvable... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Not Approvable" &
      data$fy == "2026")],
    "0.00%"
  )
  # Table 2.3 | CDRH | Rate of Not Approvable... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Rate of Not Approvable" &
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
    "7"
  )
  # Table 2.1 | OHT3 | SI Goal Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Goal Met" &
      data$fy == "2027")]
  ))
  # Table 2.3 | OHT4 | Number with MDUFA Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number with MDUFA Decision" &
      data$fy == "2023")],
    "8"
  )
  # Table 2.3 | OHT4 | Number with MDUFA Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number with MDUFA Decision" &
      data$fy == "2024")],
    "11"
  )
  # Table 2.3 | OHT4 | Number with MDUFA Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number with MDUFA Decision" &
      data$fy == "2025")],
    "8"
  )
  # Table 2.3 | OHT4 | Number with MDUFA Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number with MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 2.3 | OHT4 | Number with MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.3" &
      data$organization == "OHT4" &
      data$performance_metric == "Number with MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 2.1 | OHT7 | SI Goal Not Met... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT7" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2023")],
    "1"
  )
  # Table 2.1 | OHT7 | SI Goal Not Met... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT7" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2024")],
    "0"
  )
  # Table 2.1 | OHT7 | SI Goal Not Met... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT7" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2025")],
    "1"
  )
  # Table 2.1 | OHT7 | SI Goal Not Met... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT7" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2026")],
    "1"
  )
  # Table 2.1 | OHT7 | SI Goal Not Met... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "2.1" &
      data$organization == "OHT7" &
      data$performance_metric == "SI Goal Not Met" &
      data$fy == "2027")]
  ))
  # Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
      data$fy == "2023")],
    "3"
  )
  # Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
      data$fy == "2024")],
    "4"
  )
  # Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
      data$fy == "2025")],
    "3"
  )
  # Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
      data$fy == "2026")],
    "7"
  )
  # Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.1" &
      data$organization == "OHT1" &
      data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
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
    "10"
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
  # Table 6.7 | OHT1 | Mean Industry Days for Submissions that ... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2023")],
    "150.40"
  )
  # Table 6.7 | OHT1 | Mean Industry Days for Submissions that ... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2024")],
    "165.83"
  )
  # Table 6.7 | OHT1 | Mean Industry Days for Submissions that ... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2025")],
    "132.40"
  )
  # Table 6.7 | OHT1 | Mean Industry Days for Submissions that ... | FY 2026 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2026")]
  ))
  # Table 6.7 | OHT1 | Mean Industry Days for Submissions that ... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.7" &
      data$organization == "OHT1" &
      data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
      data$fy == "2027")]
  ))
  # Table 6.6 | OHT2 | Number of SE Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT2" &
      data$performance_metric == "Number of SE Decision" &
      data$fy == "2023")],
    "308"
  )
  # Table 6.6 | OHT2 | Number of SE Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT2" &
      data$performance_metric == "Number of SE Decision" &
      data$fy == "2024")],
    "295"
  )
  # Table 6.6 | OHT2 | Number of SE Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT2" &
      data$performance_metric == "Number of SE Decision" &
      data$fy == "2025")],
    "308"
  )
  # Table 6.6 | OHT2 | Number of SE Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT2" &
      data$performance_metric == "Number of SE Decision" &
      data$fy == "2026")],
    "98"
  )
  # Table 6.6 | OHT2 | Number of SE Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT2" &
      data$performance_metric == "Number of SE Decision" &
      data$fy == "2027")]
  ))
  # Table 6.2 | OHT3 | SI Pending Within 60 FDA Days... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Pending Within 60 FDA Days" &
      data$fy == "2023")],
    "0"
  )
  # Table 6.2 | OHT3 | SI Pending Within 60 FDA Days... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Pending Within 60 FDA Days" &
      data$fy == "2024")],
    "0"
  )
  # Table 6.2 | OHT3 | SI Pending Within 60 FDA Days... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Pending Within 60 FDA Days" &
      data$fy == "2025")],
    "0"
  )
  # Table 6.2 | OHT3 | SI Pending Within 60 FDA Days... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Pending Within 60 FDA Days" &
      data$fy == "2026")],
    "64"
  )
  # Table 6.2 | OHT3 | SI Pending Within 60 FDA Days... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT3" &
      data$performance_metric == "SI Pending Within 60 FDA Days" &
      data$fy == "2027")]
  ))
  # Table 6.2 | OHT4 | Eligible For SI... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT4" &
      data$performance_metric == "Eligible For SI" &
      data$fy == "2023")],
    "677"
  )
  # Table 6.2 | OHT4 | Eligible For SI... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT4" &
      data$performance_metric == "Eligible For SI" &
      data$fy == "2024")],
    "599"
  )
  # Table 6.2 | OHT4 | Eligible For SI... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT4" &
      data$performance_metric == "Eligible For SI" &
      data$fy == "2025")],
    "618"
  )
  # Table 6.2 | OHT4 | Eligible For SI... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT4" &
      data$performance_metric == "Eligible For SI" &
      data$fy == "2026")],
    "386"
  )
  # Table 6.2 | OHT4 | Eligible For SI... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT4" &
      data$performance_metric == "Eligible For SI" &
      data$fy == "2027")]
  ))
  # Table 6.6 | OHT5 | Number of Withdrawal... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2023")],
    "10"
  )
  # Table 6.6 | OHT5 | Number of Withdrawal... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2024")],
    "10"
  )
  # Table 6.6 | OHT5 | Number of Withdrawal... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2025")],
    "16"
  )
  # Table 6.6 | OHT5 | Number of Withdrawal... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2026")],
    "1"
  )
  # Table 6.6 | OHT5 | Number of Withdrawal... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT5" &
      data$performance_metric == "Number of Withdrawal" &
      data$fy == "2027")]
  ))
  # Table 6.4 | OHT7 | MDUFA V Decision (SE/NSE)... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.4" &
      data$organization == "OHT7" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2023")],
    "220"
  )
  # Table 6.4 | OHT7 | MDUFA V Decision (SE/NSE)... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.4" &
      data$organization == "OHT7" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2024")],
    "195"
  )
  # Table 6.4 | OHT7 | MDUFA V Decision (SE/NSE)... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.4" &
      data$organization == "OHT7" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2025")],
    "226"
  )
  # Table 6.4 | OHT7 | MDUFA V Decision (SE/NSE)... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.4" &
      data$organization == "OHT7" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2026")],
    "65"
  )
  # Table 6.4 | OHT7 | MDUFA V Decision (SE/NSE)... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.4" &
      data$organization == "OHT7" &
      data$performance_metric == "MDUFA V Decision (SE/NSE)" &
      data$fy == "2027")]
  ))
  # Table 6.2 | OHT8 | SI Over 60 FDA Days... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Over 60 FDA Days" &
      data$fy == "2023")],
    "0"
  )
  # Table 6.2 | OHT8 | SI Over 60 FDA Days... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Over 60 FDA Days" &
      data$fy == "2024")],
    "1"
  )
  # Table 6.2 | OHT8 | SI Over 60 FDA Days... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Over 60 FDA Days" &
      data$fy == "2025")],
    "3"
  )
  # Table 6.2 | OHT8 | SI Over 60 FDA Days... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Over 60 FDA Days" &
      data$fy == "2026")],
    "0"
  )
  # Table 6.2 | OHT8 | SI Over 60 FDA Days... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.2" &
      data$organization == "OHT8" &
      data$performance_metric == "SI Over 60 FDA Days" &
      data$fy == "2027")]
  ))
  # Table 6.5 | OHT8 | Number With MDUFA V Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2023")],
    "445"
  )
  # Table 6.5 | OHT8 | Number With MDUFA V Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2024")],
    "422"
  )
  # Table 6.5 | OHT8 | Number With MDUFA V Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2025")],
    "461"
  )
  # Table 6.5 | OHT8 | Number With MDUFA V Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2026")],
    "152"
  )
  # Table 6.5 | OHT8 | Number With MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2027")]
  ))
  # Table 6.5 | OHT8 | Maximum Industry Days to MDUFA V Decisio... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
      data$fy == "2023")],
    "182"
  )
  # Table 6.5 | OHT8 | Maximum Industry Days to MDUFA V Decisio... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
      data$fy == "2024")],
    "215"
  )
  # Table 6.5 | OHT8 | Maximum Industry Days to MDUFA V Decisio... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
      data$fy == "2025")],
    "185"
  )
  # Table 6.5 | OHT8 | Maximum Industry Days to MDUFA V Decisio... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
      data$fy == "2026")],
    "141"
  )
  # Table 6.5 | OHT8 | Maximum Industry Days to MDUFA V Decisio... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.5" &
      data$organization == "OHT8" &
      data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
      data$fy == "2027")]
  ))
  # Table 6.6 | OHT8 | Number With MDUFA V Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2023")],
    "445"
  )
  # Table 6.6 | OHT8 | Number With MDUFA V Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2024")],
    "422"
  )
  # Table 6.6 | OHT8 | Number With MDUFA V Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2025")],
    "461"
  )
  # Table 6.6 | OHT8 | Number With MDUFA V Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2026")],
    "152"
  )
  # Table 6.6 | OHT8 | Number With MDUFA V Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.6" &
      data$organization == "OHT8" &
      data$performance_metric == "Number With MDUFA V Decision" &
      data$fy == "2027")]
  ))
  # Table 8.3 | CDRH | 80th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023")],
    "329"
  )
  # Table 8.3 | CDRH | 80th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024")],
    "327"
  )
  # Table 8.3 | CDRH | 80th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025")],
    "328"
  )
  # Table 8.3 | CDRH | 80th Percentile Total Days to MDUFA Deci... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026")],
    "140"
  )
  # Table 8.3 | CDRH | 80th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "CDRH" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT1 | 80th Percentile Total Days to MDUFA Deci... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2023")],
    "328"
  )
  # Table 8.3 | OHT1 | 80th Percentile Total Days to MDUFA Deci... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2024")],
    "321"
  )
  # Table 8.3 | OHT1 | 80th Percentile Total Days to MDUFA Deci... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2025")],
    "330"
  )
  # Table 8.3 | OHT1 | 80th Percentile Total Days to MDUFA Deci... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2026")],
    "73"
  )
  # Table 8.3 | OHT1 | 80th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT1" &
      data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT2 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2023")],
    "150"
  )
  # Table 8.3 | OHT2 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2024")],
    "150"
  )
  # Table 8.3 | OHT2 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2025")],
    "149"
  )
  # Table 8.3 | OHT2 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2026")],
    "147"
  )
  # Table 8.3 | OHT2 | 80th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT2" &
      data$performance_metric == "80th Percentile FDA Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.2 | OHT3 | MDUFA Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2023")],
    "11"
  )
  # Table 8.2 | OHT3 | MDUFA Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2024")],
    "8"
  )
  # Table 8.2 | OHT3 | MDUFA Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2025")],
    "8"
  )
  # Table 8.2 | OHT3 | MDUFA Decision... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2026")],
    "0"
  )
  # Table 8.2 | OHT3 | MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT3" &
      data$performance_metric == "MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 8.2 | OHT7 | De Novos Accepted... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Accepted" &
      data$fy == "2023")],
    "20"
  )
  # Table 8.2 | OHT7 | De Novos Accepted... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Accepted" &
      data$fy == "2024")],
    "18"
  )
  # Table 8.2 | OHT7 | De Novos Accepted... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Accepted" &
      data$fy == "2025")],
    "11"
  )
  # Table 8.2 | OHT7 | De Novos Accepted... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Accepted" &
      data$fy == "2026")],
    "12"
  )
  # Table 8.2 | OHT7 | De Novos Accepted... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.2" &
      data$organization == "OHT7" &
      data$performance_metric == "De Novos Accepted" &
      data$fy == "2027")]
  ))
  # Table 8.3 | OHT7 | Average Total Days to MDUFA Decision... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2023")],
    "295.80"
  )
  # Table 8.3 | OHT7 | Average Total Days to MDUFA Decision... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2024")],
    "230.94"
  )
  # Table 8.3 | OHT7 | Average Total Days to MDUFA Decision... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2025")],
    "259.63"
  )
  # Table 8.3 | OHT7 | Average Total Days to MDUFA Decision... | FY 2026 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2026")]
  ))
  # Table 8.3 | OHT7 | Average Total Days to MDUFA Decision... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "8.3" &
      data$organization == "OHT7" &
      data$performance_metric == "Average Total Days to MDUFA Decision" &
      data$fy == "2027")]
  ))
  # Table 9.3 | CDRH | Maximum FDA Days to Written Feedback... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Written Feedback" &
      data$fy == "2023")],
    "141"
  )
  # Table 9.3 | CDRH | Maximum FDA Days to Written Feedback... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Written Feedback" &
      data$fy == "2024")],
    "245"
  )
  # Table 9.3 | CDRH | Maximum FDA Days to Written Feedback... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Written Feedback" &
      data$fy == "2025")],
    "307"
  )
  # Table 9.3 | CDRH | Maximum FDA Days to Written Feedback... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Written Feedback" &
      data$fy == "2026")],
    "157"
  )
  # Table 9.3 | CDRH | Maximum FDA Days to Written Feedback... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.3" &
      data$organization == "CDRH" &
      data$performance_metric == "Maximum FDA Days to Written Feedback" &
      data$fy == "2027")]
  ))
  # Table 9.5 | CDRH | Meeting Minutes Not Submitted and <= 15 ... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2023")],
    "0"
  )
  # Table 9.5 | CDRH | Meeting Minutes Not Submitted and <= 15 ... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2024")],
    "0"
  )
  # Table 9.5 | CDRH | Meeting Minutes Not Submitted and <= 15 ... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2025")],
    "0"
  )
  # Table 9.5 | CDRH | Meeting Minutes Not Submitted and <= 15 ... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2026")],
    "69"
  )
  # Table 9.5 | CDRH | Meeting Minutes Not Submitted and <= 15 ... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.5" &
      data$organization == "CDRH" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2027")]
  ))
  # Table 9.5 | OHT1 | Meeting Minutes Not Submitted and <= 15 ... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2023")],
    "0"
  )
  # Table 9.5 | OHT1 | Meeting Minutes Not Submitted and <= 15 ... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2024")],
    "0"
  )
  # Table 9.5 | OHT1 | Meeting Minutes Not Submitted and <= 15 ... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2025")],
    "0"
  )
  # Table 9.5 | OHT1 | Meeting Minutes Not Submitted and <= 15 ... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2026")],
    "4"
  )
  # Table 9.5 | OHT1 | Meeting Minutes Not Submitted and <= 15 ... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT1" &
      data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
      data$fy == "2027")]
  ))
  # Table 9.5 | OHT3 | Number of Meetings Required ¹... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT3" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2023")],
    "254"
  )
  # Table 9.5 | OHT3 | Number of Meetings Required ¹... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT3" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2024")],
    "268"
  )
  # Table 9.5 | OHT3 | Number of Meetings Required ¹... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT3" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2025")],
    "295"
  )
  # Table 9.5 | OHT3 | Number of Meetings Required ¹... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT3" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2026")],
    "169"
  )
  # Table 9.5 | OHT3 | Number of Meetings Required ¹... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.5" &
      data$organization == "OHT3" &
      data$performance_metric == "Number of Meetings Required ¹" &
      data$fy == "2027")]
  ))
  # Table 9.1 | OHT5 | Number Without a First Cycle RTA Review ... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2023")],
    "0"
  )
  # Table 9.1 | OHT5 | Number Without a First Cycle RTA Review ... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2024")],
    "0"
  )
  # Table 9.1 | OHT5 | Number Without a First Cycle RTA Review ... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2025")],
    "0"
  )
  # Table 9.1 | OHT5 | Number Without a First Cycle RTA Review ... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2026")],
    "19"
  )
  # Table 9.1 | OHT5 | Number Without a First Cycle RTA Review ... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.1" &
      data$organization == "OHT5" &
      data$performance_metric == "Number Without a First Cycle RTA Review and <= 15 Days Since Date Received (First RTA Action Pending)" &
      data$fy == "2027")]
  ))
  # Table 9.2 | OHT7 | Number Pending MDUFA Action... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Pending MDUFA Action" &
      data$fy == "2023")],
    "0"
  )
  # Table 9.2 | OHT7 | Number Pending MDUFA Action... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Pending MDUFA Action" &
      data$fy == "2024")],
    "0"
  )
  # Table 9.2 | OHT7 | Number Pending MDUFA Action... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Pending MDUFA Action" &
      data$fy == "2025")],
    "0"
  )
  # Table 9.2 | OHT7 | Number Pending MDUFA Action... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Pending MDUFA Action" &
      data$fy == "2026")],
    "110"
  )
  # Table 9.2 | OHT7 | Number Pending MDUFA Action... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "9.2" &
      data$organization == "OHT7" &
      data$performance_metric == "Number Pending MDUFA Action" &
      data$fy == "2027")]
  ))
  # Table 6.1 | CBER | Number Received... | FY 2023
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Received" &
      data$fy == "2023")],
    "41"
  )
  # Table 6.1 | CBER | Number Received... | FY 2024
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Received" &
      data$fy == "2024")],
    "33"
  )
  # Table 6.1 | CBER | Number Received... | FY 2025
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Received" &
      data$fy == "2025")],
    "52"
  )
  # Table 6.1 | CBER | Number Received... | FY 2026
  expect_equal(
    data$value[which(data$table_number == "6.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Received" &
      data$fy == "2026")],
    "31"
  )
  # Table 6.1 | CBER | Number Received... | FY 2027 = NA
  expect_true(is.na(
    data$value[which(data$table_number == "6.1" &
      data$organization == "CBER" &
      data$performance_metric == "Number Received" &
      data$fy == "2027")]
  ))
})
# nolint end
