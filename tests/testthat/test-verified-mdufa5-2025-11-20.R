# nolint start
# Verified extraction tests for MDUFA V 2025-11-20 report
# Generated: 2025-12-10
# Verifier: Brendan O'Leary
# Sample size: 90 metrics, 450 values
# Statistical basis: LB of 95% CI > 90% (Wilson score)

# Helper function to find local PDF (works from testthat directory)
find_local_pdf <- function(pattern) {
  # testthat runs from tests/testthat, so go up two levels
  pdf_dir <- testthat::test_path("..", "..", "data-raw", "pdf_reports")
  files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[1]
}

test_that("MDUFA V 2025-11-20 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-5_2025-11-20")
  skip_if(is.null(pdf_path), "MDUFA V PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA V"))
# Table 2.2 | OHT4 | Supplements Pending MDUFA Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT4" &
             data$performance_metric == "Supplements Pending MDUFA Decision" &
             data$fy == "2023"],
  "0"
)
# Table 3.3 | CDRH | Mean Industry Days for Submissions that ... | FY 2023
expect_equal(
  data$value[data$table_number == "3.3" &
             data$organization == "CDRH" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0.00"
)
# Table 8.4 | OHT5 | Rate of Granted Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT5" &
             data$performance_metric == "Rate of Granted Decision" &
             data$fy == "2023"],
  "33.33%"
)
# Table 3.3 | OHT8 | Mean FDA Days for Submissions that Misse... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2023"]
))
# Table 2.2 | OHT5 | Non-MDUFA Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT5" &
             data$performance_metric == "Non-MDUFA Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.11 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT4" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2023"]
))
# Table 6.5 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2023
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2023"],
  "315"
)
# Table 2.2 | OHT6 | MDUFA Decision Goal Met... | FY 2023
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT6" &
             data$performance_metric == "MDUFA Decision Goal Met" &
             data$fy == "2023"],
  "6"
)
# Table 6.5 | OHT8 | 40th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "40th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "107"
)
# Table 1.11 | OHT5 | Number of Submissions that Missed the Go... | FY 2023
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0"
)
# Table 6.1 | OHT2 | Rate of Submissions Not Accepted for Rev... | FY 2023
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Rate of Submissions Not Accepted for Review or Failed TS on First Cycle ²" &
             data$fy == "2023"],
  "10.24%"
)
# Table 8.5 | CBER | Number of Submissions that Missed the Go... | FY 2023
expect_equal(
  data$value[data$table_number == "8.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0"
)
# Table 6.5 | OHT7 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT7" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "59"
)
# Table 9.3 | OHT5 | Number with Written Feedback Sent... | FY 2023
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2023"],
  "384"
)
# Table 7.2 | CDRH | Currently Under Review... | FY 2023
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Currently Under Review" &
             data$fy == "2023"],
  "2"
)
# Table 1.11 | OHT2 | Mean Industry Days for Submissions that ... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT2" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2023"]
))
# Table 1.14 | OHT7 | PMAs Pending MDUFA Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "OHT7" &
             data$performance_metric == "PMAs Pending MDUFA Decision" &
             data$fy == "2023"],
  "0"
)
# Table 3.1 | OHT2 | Current Performance Percent Goal Met... | FY 2023
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2023"],
  "100.00%"
)
# Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2023
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT1" &
             data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
             data$fy == "2023"],
  "3"
)
# Table 1.5 | CBER | Number of PMAs Filed... | FY 2023
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of PMAs Filed" &
             data$fy == "2023"],
  "3"
)
# Table 9.3 | OHT8 | Number with Written Feedback Sent... | FY 2023
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2023"],
  "261"
)
# Table 1.2 | OHT3 | Number Received... | FY 2023
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Received" &
             data$fy == "2023"],
  "3"
)
# Table 2.2 | CBER | Current Performance Percent Goal Met... | FY 2023
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2023"],
  "75.00%"
)
# Table 2.2 | OHT1 | Performance Metric... | FY 2023
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT1" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2023"],
  "95% Within 180 FDA Days"
)
# Table 11.1 | CDRH | Denial without SI... | FY 2023
expect_equal(
  data$value[data$table_number == "11.1" &
             data$organization == "CDRH" &
             data$performance_metric == "Denial without SI" &
             data$fy == "2023"],
  "0"
)
# Table 1.2 | OHT8 | Number Not Filed... | FY 2023
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT8" &
             data$performance_metric == "Number Not Filed" &
             data$fy == "2023"],
  "0"
)
# Table 8.4 | OHT1 | Number of Deleted... | FY 2023
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT1" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2023"],
  "4"
)
# Table 1.6 | OHT1 | PMAs Pending MDUFA Decision Past Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "OHT1" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2023"],
  "0"
)
# Table 3.3 | OHT5 | Mean Industry Days for Submissions that ... | FY 2023
expect_equal(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0.00"
)
# Table 8.3 | OHT7 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2023
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT7" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
             data$fy == "2023"],
  "149"
)
# Table 6.1 | OHT6 | Closed Before First RTA or TS Action ¹... | FY 2023
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT6" &
             data$performance_metric == "Closed Before First RTA or TS Action ¹" &
             data$fy == "2023"],
  "6"
)
# Table 2.1 | OHT2 | SI Pending Within Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "OHT2" &
             data$performance_metric == "SI Pending Within Goal" &
             data$fy == "2023"],
  "0"
)
# Table 8.6 | OHT4 | MDUFA Decision Within 150 FDA Days... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "8.6" &
             data$organization == "OHT4" &
             data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
             data$fy == "2023"]
))
# Table 7.2 | CBER | Number With MDUFA V Decision After Trimm... | FY 2023
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CBER" &
             data$performance_metric == "Number With MDUFA V Decision After Trimming the Upper and Lower 2%" &
             data$fy == "2023"],
  "36"
)
# Table 1.2 | CDRH | Completed RTF... | FY 2025
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Completed RTF" &
             data$fy == "2025"],
  "77"
)
# Table 1.9 | OHT3 | Number Filed... | FY 2024
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Filed" &
             data$fy == "2024"],
  "6"
)
# Table 1.8 | OHT4 | Maximum Industry Days to MDUFA Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "OHT4" &
             data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
             data$fy == "2025"],
  "0"
)
# Table 9.5 | OHT5 | Meeting Minutes Past 15 Days of Meeting... | FY 2023
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT5" &
             data$performance_metric == "Meeting Minutes Past 15 Days of Meeting" &
             data$fy == "2023"],
  "64"
)
# Table 6.2 | OHT4 | SI Pending Within 60 FDA Days... | FY 2025
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT4" &
             data$performance_metric == "SI Pending Within 60 FDA Days" &
             data$fy == "2025"],
  "89"
)
# Table 6.6 | OHT3 | Number of NSE Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT3" &
             data$performance_metric == "Number of NSE Decision" &
             data$fy == "2025"],
  "3"
)
# Table 6.2 | OHT6 | Substantive Interaction (SI) Goal... | FY 2026
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT6" &
             data$performance_metric == "Substantive Interaction (SI) Goal" &
             data$fy == "2026"],
  "95% SI Within 60 FDA Days"
)
# Table 6.1 | OHT6 | Closed Before First RTA or TS Action ¹... | FY 2024
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT6" &
             data$performance_metric == "Closed Before First RTA or TS Action ¹" &
             data$fy == "2024"],
  "4"
)
# Table 9.2 | OHT3 | Written Feedback Provided Within Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Written Feedback Provided Within Goal" &
             data$fy == "2024"],
  "485"
)
# Table 6.8 | OHT7 | MDUFA V Decision Within 90 FDA Days... | FY 2025
expect_equal(
  data$value[data$table_number == "6.8" &
             data$organization == "OHT7" &
             data$performance_metric == "MDUFA V Decision Within 90 FDA Days" &
             data$fy == "2025"],
  "0"
)
# Table 1.1 | OHT3 | Number Closed Before First RTA Action... | FY 2023
expect_equal(
  data$value[data$table_number == "1.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Closed Before First RTA Action" &
             data$fy == "2023"],
  "0"
)
# Table 2.1 | CDRH | SI Pending Past Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "CDRH" &
             data$performance_metric == "SI Pending Past Goal" &
             data$fy == "2023"],
  "0"
)
# Table 1.5 | CDRH | PMAs Pending MDUFA Decision Past Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CDRH" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2025"],
  "0"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA D... | FY 2023
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT4" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2023"],
  "181"
)
# Table 7.1 | CBER | Average Number of Days to Accept/Refuse ... | FY 2023
expect_equal(
  data$value[data$table_number == "7.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Days to Accept/Refuse to Accept" &
             data$fy == "2023"],
  "13.27"
)
# Table 9.5 | OHT6 | Meeting Minutes Submitted Within 15 Days... | FY 2023
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT6" &
             data$performance_metric == "Meeting Minutes Submitted Within 15 Days of Meeting" &
             data$fy == "2023"],
  "91"
)
# Table 1.7 | OHT5 | 20th Percentile Industry Days to MDUFA D... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "OHT5" &
             data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2024"],
  "36"
)
# Table 8.3 | OHT2 | 80th Percentile Total Days to MDUFA Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT2" &
             data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
             data$fy == "2025"],
  "242"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2023
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Withdrawal" &
             data$fy == "2023"],
  "10"
)
# Table 3.1 | OHT3 | Performance Metric... | FY 2026
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2026"],
  "95% Within 90 FDA Days"
)
# Table 6.5 | OHT8 | 60th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "128"
)
# Table 1.2 | CDRH | Completed RTF... | FY 2023
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Completed RTF" &
             data$fy == "2023"],
  "72"
)
# Table 1.2 | CDRH | Completed RTF... | FY 2024
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Completed RTF" &
             data$fy == "2024"],
  "68"
)
# Table 1.5 | CDRH | PMAs Pending MDUFA Decision Past Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CDRH" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2023"],
  "0"
)
# Table 1.5 | CDRH | PMAs Pending MDUFA Decision Past Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CDRH" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2024"],
  "0"
)
# Table 1.6 | OHT1 | PMAs Pending MDUFA Decision Past Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "OHT1" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2024"],
  "0"
)
# Table 1.6 | OHT1 | PMAs Pending MDUFA Decision Past Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "OHT1" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2025"],
  "0"
)
# Table 1.11 | OHT2 | Mean Industry Days for Submissions that ... | FY 2024
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT2" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 1.1 | OHT3 | Number Closed Before First RTA Action... | FY 2024
expect_equal(
  data$value[data$table_number == "1.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Closed Before First RTA Action" &
             data$fy == "2024"],
  "0"
)
# Table 1.1 | OHT3 | Number Closed Before First RTA Action... | FY 2025
expect_equal(
  data$value[data$table_number == "1.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Closed Before First RTA Action" &
             data$fy == "2025"],
  "0"
)
# Table 1.2 | OHT3 | Number Received... | FY 2024
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Received" &
             data$fy == "2024"],
  "7"
)
# Table 1.2 | OHT3 | Number Received... | FY 2025
expect_equal(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Received" &
             data$fy == "2025"],
  "5"
)
# Table 1.9 | OHT3 | Number Filed... | FY 2023
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Filed" &
             data$fy == "2023"],
  "3"
)
# Table 1.9 | OHT3 | Number Filed... | FY 2025
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Filed" &
             data$fy == "2025"],
  "5"
)
# Table 1.8 | OHT4 | Maximum Industry Days to MDUFA Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "OHT4" &
             data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | OHT4 | Maximum Industry Days to MDUFA Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "OHT4" &
             data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | OHT5 | 20th Percentile Industry Days to MDUFA D... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "OHT5" &
             data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.7 | OHT5 | 20th Percentile Industry Days to MDUFA D... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "OHT5" &
             data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2025"],
  "88"
)
# Table 1.11 | OHT5 | Number of Submissions that Missed the Go... | FY 2024
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0"
)
# Table 1.11 | OHT5 | Number of Submissions that Missed the Go... | FY 2025
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0"
)
# Table 1.14 | OHT7 | PMAs Pending MDUFA Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "OHT7" &
             data$performance_metric == "PMAs Pending MDUFA Decision" &
             data$fy == "2024"],
  "2"
)
# Table 1.14 | OHT7 | PMAs Pending MDUFA Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "OHT7" &
             data$performance_metric == "PMAs Pending MDUFA Decision" &
             data$fy == "2025"],
  "6"
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
# Table 2.1 | CDRH | SI Pending Past Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "CDRH" &
             data$performance_metric == "SI Pending Past Goal" &
             data$fy == "2024"],
  "0"
)
# Table 2.1 | CDRH | SI Pending Past Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "CDRH" &
             data$performance_metric == "SI Pending Past Goal" &
             data$fy == "2025"],
  "1"
)
# Table 2.2 | OHT1 | Performance Metric... | FY 2024
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT1" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2024"],
  "95% Within 180 FDA Days"
)
# Table 2.2 | OHT1 | Performance Metric... | FY 2025
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT1" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2025"],
  "95% Within 180 FDA Days"
)
# Table 2.2 | OHT1 | Performance Metric... | FY 2026
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT1" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2026"],
  "95% Within 180 FDA Days"
)
# Table 2.2 | OHT1 | Performance Metric... | FY 2027
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT1" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2027"],
  "95% Within 180 FDA Days"
)
# Table 2.1 | OHT2 | SI Pending Within Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "OHT2" &
             data$performance_metric == "SI Pending Within Goal" &
             data$fy == "2024"],
  "0"
)
# Table 2.1 | OHT2 | SI Pending Within Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "2.1" &
             data$organization == "OHT2" &
             data$performance_metric == "SI Pending Within Goal" &
             data$fy == "2025"],
  "30"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT4" &
             data$performance_metric == "Supplements Pending MDUFA Decision" &
             data$fy == "2024"],
  "0"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT4" &
             data$performance_metric == "Supplements Pending MDUFA Decision" &
             data$fy == "2025"],
  "9"
)
# Table 2.2 | OHT5 | Non-MDUFA Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT5" &
             data$performance_metric == "Non-MDUFA Decision" &
             data$fy == "2024"],
  "0"
)
# Table 2.2 | OHT5 | Non-MDUFA Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT5" &
             data$performance_metric == "Non-MDUFA Decision" &
             data$fy == "2025"],
  "0"
)
# Table 2.2 | OHT6 | MDUFA Decision Goal Met... | FY 2024
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT6" &
             data$performance_metric == "MDUFA Decision Goal Met" &
             data$fy == "2024"],
  "5"
)
# Table 2.2 | OHT6 | MDUFA Decision Goal Met... | FY 2025
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT6" &
             data$performance_metric == "MDUFA Decision Goal Met" &
             data$fy == "2025"],
  "3"
)
# Table 3.3 | CDRH | Mean Industry Days for Submissions that ... | FY 2024
expect_equal(
  data$value[data$table_number == "3.3" &
             data$organization == "CDRH" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 3.3 | CDRH | Mean Industry Days for Submissions that ... | FY 2025
expect_equal(
  data$value[data$table_number == "3.3" &
             data$organization == "CDRH" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0.00"
)
# Table 3.1 | OHT2 | Current Performance Percent Goal Met... | FY 2024
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2024"],
  "100.00%"
)
# Table 3.1 | OHT2 | Current Performance Percent Goal Met... | FY 2025
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2025"],
  "100.00%"
)
# Table 3.1 | OHT3 | Performance Metric... | FY 2023
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2023"],
  "95% Within 90 FDA Days"
)
# Table 3.1 | OHT3 | Performance Metric... | FY 2024
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2024"],
  "95% Within 90 FDA Days"
)
# Table 3.1 | OHT3 | Performance Metric... | FY 2025
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2025"],
  "95% Within 90 FDA Days"
)
# Table 3.1 | OHT3 | Performance Metric... | FY 2027
expect_equal(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Performance Metric" &
             data$fy == "2027"],
  "95% Within 90 FDA Days"
)
# Table 3.3 | OHT5 | Mean Industry Days for Submissions that ... | FY 2024
expect_equal(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2024
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT1" &
             data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
             data$fy == "2024"],
  "4"
)
# Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2025
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT1" &
             data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
             data$fy == "2025"],
  "2"
)
# Table 6.1 | OHT2 | Rate of Submissions Not Accepted for Rev... | FY 2024
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Rate of Submissions Not Accepted for Review or Failed TS on First Cycle ²" &
             data$fy == "2024"],
  "3.23%"
)
# Table 6.1 | OHT2 | Rate of Submissions Not Accepted for Rev... | FY 2025
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Rate of Submissions Not Accepted for Review or Failed TS on First Cycle ²" &
             data$fy == "2025"],
  "9.56%"
)
# Table 6.6 | OHT3 | Number of NSE Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT3" &
             data$performance_metric == "Number of NSE Decision" &
             data$fy == "2023"],
  "27"
)
# Table 6.6 | OHT3 | Number of NSE Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT3" &
             data$performance_metric == "Number of NSE Decision" &
             data$fy == "2024"],
  "28"
)
# Table 6.2 | OHT4 | SI Pending Within 60 FDA Days... | FY 2023
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT4" &
             data$performance_metric == "SI Pending Within 60 FDA Days" &
             data$fy == "2023"],
  "0"
)
# Table 6.2 | OHT4 | SI Pending Within 60 FDA Days... | FY 2024
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT4" &
             data$performance_metric == "SI Pending Within 60 FDA Days" &
             data$fy == "2024"],
  "0"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2024
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Withdrawal" &
             data$fy == "2024"],
  "10"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2025
expect_equal(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Withdrawal" &
             data$fy == "2025"],
  "6"
)
# Table 6.1 | OHT6 | Closed Before First RTA or TS Action ¹... | FY 2025
expect_equal(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT6" &
             data$performance_metric == "Closed Before First RTA or TS Action ¹" &
             data$fy == "2025"],
  "7"
)
# Table 6.2 | OHT6 | Substantive Interaction (SI) Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT6" &
             data$performance_metric == "Substantive Interaction (SI) Goal" &
             data$fy == "2023"],
  "95% SI Within 60 FDA Days"
)
# Table 6.2 | OHT6 | Substantive Interaction (SI) Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT6" &
             data$performance_metric == "Substantive Interaction (SI) Goal" &
             data$fy == "2024"],
  "95% SI Within 60 FDA Days"
)
# Table 6.2 | OHT6 | Substantive Interaction (SI) Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT6" &
             data$performance_metric == "Substantive Interaction (SI) Goal" &
             data$fy == "2025"],
  "95% SI Within 60 FDA Days"
)
# Table 6.2 | OHT6 | Substantive Interaction (SI) Goal... | FY 2027
expect_equal(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT6" &
             data$performance_metric == "Substantive Interaction (SI) Goal" &
             data$fy == "2027"],
  "95% SI Within 60 FDA Days"
)
# Table 6.5 | OHT7 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT7" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "53"
)
# Table 6.5 | OHT7 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT7" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "30"
)
# Table 6.8 | OHT7 | MDUFA V Decision Within 90 FDA Days... | FY 2023
expect_equal(
  data$value[data$table_number == "6.8" &
             data$organization == "OHT7" &
             data$performance_metric == "MDUFA V Decision Within 90 FDA Days" &
             data$fy == "2023"],
  "2"
)
# Table 6.8 | OHT7 | MDUFA V Decision Within 90 FDA Days... | FY 2024
expect_equal(
  data$value[data$table_number == "6.8" &
             data$organization == "OHT7" &
             data$performance_metric == "MDUFA V Decision Within 90 FDA Days" &
             data$fy == "2024"],
  "4"
)
# Table 6.5 | OHT8 | 40th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "40th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "114"
)
# Table 6.5 | OHT8 | 40th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "40th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "102"
)
# Table 6.5 | OHT8 | 60th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "146"
)
# Table 6.5 | OHT8 | 60th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "156"
)
# Table 7.2 | CDRH | Currently Under Review... | FY 2024
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Currently Under Review" &
             data$fy == "2024"],
  "31"
)
# Table 7.2 | CDRH | Currently Under Review... | FY 2025
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Currently Under Review" &
             data$fy == "2025"],
  "1,526"
)
# Table 8.4 | OHT1 | Number of Deleted... | FY 2024
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT1" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2024"],
  "2"
)
# Table 8.4 | OHT1 | Number of Deleted... | FY 2025
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT1" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2025"],
  "0"
)
# Table 8.3 | OHT2 | 80th Percentile Total Days to MDUFA Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT2" &
             data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
             data$fy == "2023"],
  "328"
)
# Table 8.3 | OHT2 | 80th Percentile Total Days to MDUFA Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT2" &
             data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
             data$fy == "2024"],
  "328"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA D... | FY 2024
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT4" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2024"],
  "179"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA D... | FY 2025
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT4" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2025"],
  "189"
)
# Table 8.4 | OHT5 | Rate of Granted Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT5" &
             data$performance_metric == "Rate of Granted Decision" &
             data$fy == "2024"],
  "25.00%"
)
# Table 8.4 | OHT5 | Rate of Granted Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT5" &
             data$performance_metric == "Rate of Granted Decision" &
             data$fy == "2025"],
  "80.00%"
)
# Table 8.3 | OHT7 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2024
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT7" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
             data$fy == "2024"],
  "148"
)
# Table 8.3 | OHT7 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2025
expect_equal(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT7" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
             data$fy == "2025"],
  "149"
)
# Table 9.2 | OHT3 | Written Feedback Provided Within Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Written Feedback Provided Within Goal" &
             data$fy == "2023"],
  "440"
)
# Table 9.2 | OHT3 | Written Feedback Provided Within Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Written Feedback Provided Within Goal" &
             data$fy == "2025"],
  "439"
)
# Table 9.3 | OHT5 | Number with Written Feedback Sent... | FY 2024
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2024"],
  "416"
)
# Table 9.3 | OHT5 | Number with Written Feedback Sent... | FY 2025
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2025"],
  "353"
)
# Table 9.5 | OHT5 | Meeting Minutes Past 15 Days of Meeting... | FY 2024
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT5" &
             data$performance_metric == "Meeting Minutes Past 15 Days of Meeting" &
             data$fy == "2024"],
  "45"
)
# Table 9.5 | OHT5 | Meeting Minutes Past 15 Days of Meeting... | FY 2025
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT5" &
             data$performance_metric == "Meeting Minutes Past 15 Days of Meeting" &
             data$fy == "2025"],
  "32"
)
# Table 9.5 | OHT6 | Meeting Minutes Submitted Within 15 Days... | FY 2024
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT6" &
             data$performance_metric == "Meeting Minutes Submitted Within 15 Days of Meeting" &
             data$fy == "2024"],
  "103"
)
# Table 9.5 | OHT6 | Meeting Minutes Submitted Within 15 Days... | FY 2025
expect_equal(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT6" &
             data$performance_metric == "Meeting Minutes Submitted Within 15 Days of Meeting" &
             data$fy == "2025"],
  "88"
)
# Table 9.3 | OHT8 | Number with Written Feedback Sent... | FY 2024
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2024"],
  "285"
)
# Table 9.3 | OHT8 | Number with Written Feedback Sent... | FY 2025
expect_equal(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2025"],
  "266"
)
# Table 11.1 | CDRH | Denial without SI... | FY 2024
expect_equal(
  data$value[data$table_number == "11.1" &
             data$organization == "CDRH" &
             data$performance_metric == "Denial without SI" &
             data$fy == "2024"],
  "0 (0)"
)
# Table 11.1 | CDRH | Denial without SI... | FY 2025
expect_equal(
  data$value[data$table_number == "11.1" &
             data$organization == "CDRH" &
             data$performance_metric == "Denial without SI" &
             data$fy == "2025"],
  "0"
)
# Table 1.5 | CBER | Number of PMAs Filed... | FY 2024
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of PMAs Filed" &
             data$fy == "2024"],
  "0"
)
# Table 1.5 | CBER | Number of PMAs Filed... | FY 2025
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of PMAs Filed" &
             data$fy == "2025"],
  "1"
)
# Table 2.2 | CBER | Current Performance Percent Goal Met... | FY 2024
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2024"],
  "100.00%"
)
# Table 2.2 | CBER | Current Performance Percent Goal Met... | FY 2025
expect_equal(
  data$value[data$table_number == "2.2" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2025"],
  "100.00%"
)
# Table 6.5 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2024
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2024"],
  "207"
)
# Table 6.5 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2025
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2025"],
  "193"
)
# Table 7.1 | CBER | Average Number of Days to Accept/Refuse ... | FY 2024
expect_equal(
  data$value[data$table_number == "7.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Days to Accept/Refuse to Accept" &
             data$fy == "2024"],
  "12.45"
)
# Table 7.1 | CBER | Average Number of Days to Accept/Refuse ... | FY 2025
expect_equal(
  data$value[data$table_number == "7.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Days to Accept/Refuse to Accept" &
             data$fy == "2025"],
  "12.02"
)
# Table 7.2 | CBER | Number With MDUFA V Decision After Trimm... | FY 2024
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CBER" &
             data$performance_metric == "Number With MDUFA V Decision After Trimming the Upper and Lower 2%" &
             data$fy == "2024"],
  "26"
)
# Table 7.2 | CBER | Number With MDUFA V Decision After Trimm... | FY 2025
expect_equal(
  data$value[data$table_number == "7.2" &
             data$organization == "CBER" &
             data$performance_metric == "Number With MDUFA V Decision After Trimming the Upper and Lower 2%" &
             data$fy == "2025"],
  "19"
)
# Table 8.5 | CBER | Number of Submissions that Missed the Go... | FY 2024
expect_equal(
  data$value[data$table_number == "8.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0"
)
# Table 8.5 | CBER | Number of Submissions that Missed the Go... | FY 2025
expect_equal(
  data$value[data$table_number == "8.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0"
)
# Table 6.5 | OHT6 | 20th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT6" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "30"
)
# Table 6.5 | OHT6 | 20th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT6" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "30"
)
# Table 6.5 | OHT6 | 20th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT6" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "28"
)
# Table 1.5 | CBER | Current Performance Percent Goal Met... | FY 2023
expect_equal(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2023"],
  "100.00%"
)
# Table 1.6 | CBER | Non-MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.6 | CBER | Non-MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.6 | CBER | Non-MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.6" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | Number with MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2023"],
  "3"
)
# Table 1.7 | CBER | Number with MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | Number with MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile FDA Days to MDUFA V Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "180"
)
# Table 1.7 | CBER | 80th Percentile FDA Days to MDUFA V Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile FDA Days to MDUFA V Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.7 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile Industry Days to MDUFA V... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile Industry Days to MDUFA V... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile Industry Days to MDUFA V... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | 20th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "175"
)
# Table 1.7 | CBER | 20th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | 20th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "180"
)
# Table 1.7 | CBER | 80th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.7 | CBER | 80th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Number with MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | Number with MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | Number with MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Average FDA Days to MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0.00"
)
# Table 1.8 | CBER | Average FDA Days to MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0.00"
)
# Table 1.8 | CBER | Average FDA Days to MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.8 | CBER | 20th Percentile FDA Days to MDUFA V Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | 20th Percentile FDA Days to MDUFA V Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | 20th Percentile FDA Days to MDUFA V Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | 60th Percentile FDA Days to MDUFA V Deci... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | 60th Percentile FDA Days to MDUFA V Deci... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | 60th Percentile FDA Days to MDUFA V Deci... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Maximum FDA Days to MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | Maximum FDA Days to MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | Maximum FDA Days to MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Average Total Days to MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | Average Total Days to MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | Average Total Days to MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.8 | CBER | 60th Percentile Total Days to MDUFA V De... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | 60th Percentile Total Days to MDUFA V De... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | 60th Percentile Total Days to MDUFA V De... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.8 | CBER | Maximum Total Days to MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Total Days to MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.8 | CBER | Maximum Total Days to MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Total Days to MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.8 | CBER | Maximum Total Days to MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Total Days to MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.9 | CBER | Number of Deleted... | FY 2023
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2023"],
  "0"
)
# Table 1.9 | CBER | Number of Deleted... | FY 2024
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2024"],
  "0"
)
# Table 1.9 | CBER | Number of Deleted... | FY 2025
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2025"],
  "0"
)
# Table 1.9 | CBER | Rate of Not Approvable... | FY 2023
expect_equal(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2023"],
  "0.00%"
)
# Table 1.10 | CBER | Number of Not Approvable... | FY 2023
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Not Approvable" &
             data$fy == "2023"],
  "0"
)
# Table 1.10 | CBER | Number of Not Approvable... | FY 2024
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Not Approvable" &
             data$fy == "2024"],
  "0"
)
# Table 1.10 | CBER | Number of Not Approvable... | FY 2025
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Not Approvable" &
             data$fy == "2025"],
  "0"
)
# Table 1.10 | CBER | Number of Deleted... | FY 2023
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2023"],
  "0"
)
# Table 1.10 | CBER | Number of Deleted... | FY 2024
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2024"],
  "0"
)
# Table 1.10 | CBER | Number of Deleted... | FY 2025
expect_equal(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2025"],
  "0"
)
# Table 1.11 | CBER | Mean FDA Days for Submissions that Misse... | FY 2023
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0.00"
)
# Table 1.11 | CBER | Mean FDA Days for Submissions that Misse... | FY 2024
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 1.11 | CBER | Mean FDA Days for Submissions that Misse... | FY 2025
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.11 | CBER | Mean Industry Days for Submissions that ... | FY 2023
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0.00"
)
# Table 1.11 | CBER | Mean Industry Days for Submissions that ... | FY 2024
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 1.11 | CBER | Mean Industry Days for Submissions that ... | FY 2025
expect_equal(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.12 | CBER | Mean Industry Days for Submissions that ... | FY 2023
expect_equal(
  data$value[data$table_number == "1.12" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2023"],
  "0.00"
)
# Table 1.12 | CBER | Mean Industry Days for Submissions that ... | FY 2024
expect_equal(
  data$value[data$table_number == "1.12" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2024"],
  "0.00"
)
# Table 1.12 | CBER | Mean Industry Days for Submissions that ... | FY 2025
expect_equal(
  data$value[data$table_number == "1.12" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.13 | CBER | Non-MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.13 | CBER | Non-MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.13 | CBER | Non-MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.13 | CBER | MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.13 | CBER | MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.13 | CBER | MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.13 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2023"],
  "0"
)
# Table 1.13 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2024"],
  "0"
)
# Table 1.13 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2025"],
  "0"
)
# Table 1.14 | CBER | Non-MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.14 | CBER | Non-MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.14 | CBER | Non-MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.14 | CBER | MDUFA V Decision... | FY 2023
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2023"],
  "0"
)
# Table 1.14 | CBER | MDUFA V Decision... | FY 2024
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2024"],
  "0"
)
# Table 1.14 | CBER | MDUFA V Decision... | FY 2025
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2025"],
  "0"
)
# Table 1.14 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2023
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2023"],
  "0"
)
# Table 1.14 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2024
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2024"],
  "0"
)
# Table 1.14 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2025
expect_equal(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2025"],
  "0"
)
# Table 10.1 | CBER | Average Number of Amendments Prior to ID... | FY 2023
expect_equal(
  data$value[data$table_number == "10.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
             data$fy == "2023"],
  "0.07"
)
# Table 10.1 | CBER | Average Number of Amendments Prior to ID... | FY 2024
expect_equal(
  data$value[data$table_number == "10.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
             data$fy == "2024"],
  "0.06"
)
# Table 10.1 | CBER | Average Number of Amendments Prior to ID... | FY 2025
expect_equal(
  data$value[data$table_number == "10.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
             data$fy == "2025"],
  "0.00"
)
# Table 1.2 | CDRH | Completed RTF... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Completed RTF" &
             data$fy == "2026"]
))
# Table 1.2 | CDRH | Completed RTF... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Completed RTF" &
             data$fy == "2027"]
))
# Table 1.5 | CDRH | PMAs Pending MDUFA Decision Past Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CDRH" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2026"]
))
# Table 1.5 | CDRH | PMAs Pending MDUFA Decision Past Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CDRH" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2027"]
))
# Table 1.6 | OHT1 | PMAs Pending MDUFA Decision Past Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.6" &
             data$organization == "OHT1" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2026"]
))
# Table 1.6 | OHT1 | PMAs Pending MDUFA Decision Past Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.6" &
             data$organization == "OHT1" &
             data$performance_metric == "PMAs Pending MDUFA Decision Past Goal" &
             data$fy == "2027"]
))
# Table 1.11 | OHT2 | Mean Industry Days for Submissions that ... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT2" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2025"]
))
# Table 1.11 | OHT2 | Mean Industry Days for Submissions that ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT2" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.11 | OHT2 | Mean Industry Days for Submissions that ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT2" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.1 | OHT3 | Number Closed Before First RTA Action... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Closed Before First RTA Action" &
             data$fy == "2026"]
))
# Table 1.1 | OHT3 | Number Closed Before First RTA Action... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.1" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Closed Before First RTA Action" &
             data$fy == "2027"]
))
# Table 1.2 | OHT3 | Number Received... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Received" &
             data$fy == "2026"]
))
# Table 1.2 | OHT3 | Number Received... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Received" &
             data$fy == "2027"]
))
# Table 1.9 | OHT3 | Number Filed... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Filed" &
             data$fy == "2026"]
))
# Table 1.9 | OHT3 | Number Filed... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "OHT3" &
             data$performance_metric == "Number Filed" &
             data$fy == "2027"]
))
# Table 1.8 | OHT4 | Maximum Industry Days to MDUFA Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "OHT4" &
             data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
             data$fy == "2026"]
))
# Table 1.8 | OHT4 | Maximum Industry Days to MDUFA Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "OHT4" &
             data$performance_metric == "Maximum Industry Days to MDUFA Decision" &
             data$fy == "2027"]
))
# Table 1.11 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT4" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2024"]
))
# Table 1.11 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT4" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2025"]
))
# Table 1.11 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT4" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.11 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT4" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.7 | OHT5 | 20th Percentile Industry Days to MDUFA D... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "OHT5" &
             data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2026"]
))
# Table 1.7 | OHT5 | 20th Percentile Industry Days to MDUFA D... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "OHT5" &
             data$performance_metric == "20th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2027"]
))
# Table 1.11 | OHT5 | Number of Submissions that Missed the Go... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.11 | OHT5 | Number of Submissions that Missed the Go... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.14 | OHT7 | PMAs Pending MDUFA Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "OHT7" &
             data$performance_metric == "PMAs Pending MDUFA Decision" &
             data$fy == "2026"]
))
# Table 1.14 | OHT7 | PMAs Pending MDUFA Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "OHT7" &
             data$performance_metric == "PMAs Pending MDUFA Decision" &
             data$fy == "2027"]
))
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
# Table 2.1 | CDRH | SI Pending Past Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.1" &
             data$organization == "CDRH" &
             data$performance_metric == "SI Pending Past Goal" &
             data$fy == "2026"]
))
# Table 2.1 | CDRH | SI Pending Past Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.1" &
             data$organization == "CDRH" &
             data$performance_metric == "SI Pending Past Goal" &
             data$fy == "2027"]
))
# Table 2.1 | OHT2 | SI Pending Within Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.1" &
             data$organization == "OHT2" &
             data$performance_metric == "SI Pending Within Goal" &
             data$fy == "2026"]
))
# Table 2.1 | OHT2 | SI Pending Within Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.1" &
             data$organization == "OHT2" &
             data$performance_metric == "SI Pending Within Goal" &
             data$fy == "2027"]
))
# Table 2.2 | OHT4 | Supplements Pending MDUFA Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT4" &
             data$performance_metric == "Supplements Pending MDUFA Decision" &
             data$fy == "2026"]
))
# Table 2.2 | OHT4 | Supplements Pending MDUFA Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT4" &
             data$performance_metric == "Supplements Pending MDUFA Decision" &
             data$fy == "2027"]
))
# Table 2.2 | OHT5 | Non-MDUFA Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT5" &
             data$performance_metric == "Non-MDUFA Decision" &
             data$fy == "2026"]
))
# Table 2.2 | OHT5 | Non-MDUFA Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT5" &
             data$performance_metric == "Non-MDUFA Decision" &
             data$fy == "2027"]
))
# Table 2.2 | OHT6 | MDUFA Decision Goal Met... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT6" &
             data$performance_metric == "MDUFA Decision Goal Met" &
             data$fy == "2026"]
))
# Table 2.2 | OHT6 | MDUFA Decision Goal Met... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "OHT6" &
             data$performance_metric == "MDUFA Decision Goal Met" &
             data$fy == "2027"]
))
# Table 3.3 | CDRH | Mean Industry Days for Submissions that ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "CDRH" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 3.3 | CDRH | Mean Industry Days for Submissions that ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "CDRH" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 3.1 | OHT2 | Current Performance Percent Goal Met... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2026"]
))
# Table 3.1 | OHT2 | Current Performance Percent Goal Met... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "3.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2027"]
))
# Table 3.3 | OHT5 | Mean Industry Days for Submissions that ... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2025"]
))
# Table 3.3 | OHT5 | Mean Industry Days for Submissions that ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 3.3 | OHT5 | Mean Industry Days for Submissions that ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 3.3 | OHT8 | Mean FDA Days for Submissions that Misse... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2024"]
))
# Table 3.3 | OHT8 | Mean FDA Days for Submissions that Misse... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2025"]
))
# Table 3.3 | OHT8 | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 3.3 | OHT8 | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "3.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT1" &
             data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
             data$fy == "2026"]
))
# Table 6.1 | OHT1 | Number Without a RTA or TS Review and > ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT1" &
             data$performance_metric == "Number Without a RTA or TS Review and > 15 Days Since Date Received ³" &
             data$fy == "2027"]
))
# Table 6.1 | OHT2 | Rate of Submissions Not Accepted for Rev... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Rate of Submissions Not Accepted for Review or Failed TS on First Cycle ²" &
             data$fy == "2026"]
))
# Table 6.1 | OHT2 | Rate of Submissions Not Accepted for Rev... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT2" &
             data$performance_metric == "Rate of Submissions Not Accepted for Review or Failed TS on First Cycle ²" &
             data$fy == "2027"]
))
# Table 6.6 | OHT3 | Number of NSE Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT3" &
             data$performance_metric == "Number of NSE Decision" &
             data$fy == "2026"]
))
# Table 6.6 | OHT3 | Number of NSE Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT3" &
             data$performance_metric == "Number of NSE Decision" &
             data$fy == "2027"]
))
# Table 6.2 | OHT4 | SI Pending Within 60 FDA Days... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT4" &
             data$performance_metric == "SI Pending Within 60 FDA Days" &
             data$fy == "2026"]
))
# Table 6.2 | OHT4 | SI Pending Within 60 FDA Days... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.2" &
             data$organization == "OHT4" &
             data$performance_metric == "SI Pending Within 60 FDA Days" &
             data$fy == "2027"]
))
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Withdrawal" &
             data$fy == "2026"]
))
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.6" &
             data$organization == "OHT5" &
             data$performance_metric == "Number of Withdrawal" &
             data$fy == "2027"]
))
# Table 6.1 | OHT6 | Closed Before First RTA or TS Action ¹... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT6" &
             data$performance_metric == "Closed Before First RTA or TS Action ¹" &
             data$fy == "2026"]
))
# Table 6.1 | OHT6 | Closed Before First RTA or TS Action ¹... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.1" &
             data$organization == "OHT6" &
             data$performance_metric == "Closed Before First RTA or TS Action ¹" &
             data$fy == "2027"]
))
# Table 6.5 | OHT7 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT7" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 6.5 | OHT7 | 20th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT7" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 6.8 | OHT7 | MDUFA V Decision Within 90 FDA Days... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.8" &
             data$organization == "OHT7" &
             data$performance_metric == "MDUFA V Decision Within 90 FDA Days" &
             data$fy == "2026"]
))
# Table 6.8 | OHT7 | MDUFA V Decision Within 90 FDA Days... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.8" &
             data$organization == "OHT7" &
             data$performance_metric == "MDUFA V Decision Within 90 FDA Days" &
             data$fy == "2027"]
))
# Table 6.5 | OHT8 | 40th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "40th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 6.5 | OHT8 | 40th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "40th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 6.5 | OHT8 | 60th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 6.5 | OHT8 | 60th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT8" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 7.2 | CDRH | Currently Under Review... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "7.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Currently Under Review" &
             data$fy == "2026"]
))
# Table 7.2 | CDRH | Currently Under Review... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "7.2" &
             data$organization == "CDRH" &
             data$performance_metric == "Currently Under Review" &
             data$fy == "2027"]
))
# Table 8.4 | OHT1 | Number of Deleted... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT1" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2026"]
))
# Table 8.4 | OHT1 | Number of Deleted... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT1" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2027"]
))
# Table 8.3 | OHT2 | 80th Percentile Total Days to MDUFA Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT2" &
             data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
             data$fy == "2026"]
))
# Table 8.3 | OHT2 | 80th Percentile Total Days to MDUFA Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT2" &
             data$performance_metric == "80th Percentile Total Days to MDUFA Decision" &
             data$fy == "2027"]
))
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA D... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT4" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2026"]
))
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA D... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT4" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA Decision" &
             data$fy == "2027"]
))
# Table 8.6 | OHT4 | MDUFA Decision Within 150 FDA Days... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "8.6" &
             data$organization == "OHT4" &
             data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
             data$fy == "2024"]
))
# Table 8.6 | OHT4 | MDUFA Decision Within 150 FDA Days... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "8.6" &
             data$organization == "OHT4" &
             data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
             data$fy == "2025"]
))
# Table 8.6 | OHT4 | MDUFA Decision Within 150 FDA Days... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.6" &
             data$organization == "OHT4" &
             data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
             data$fy == "2026"]
))
# Table 8.6 | OHT4 | MDUFA Decision Within 150 FDA Days... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.6" &
             data$organization == "OHT4" &
             data$performance_metric == "MDUFA Decision Within 150 FDA Days" &
             data$fy == "2027"]
))
# Table 8.4 | OHT5 | Rate of Granted Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT5" &
             data$performance_metric == "Rate of Granted Decision" &
             data$fy == "2026"]
))
# Table 8.4 | OHT5 | Rate of Granted Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.4" &
             data$organization == "OHT5" &
             data$performance_metric == "Rate of Granted Decision" &
             data$fy == "2027"]
))
# Table 8.3 | OHT7 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT7" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
             data$fy == "2026"]
))
# Table 8.3 | OHT7 | 60th Percentile FDA Days to MDUFA Decisi... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.3" &
             data$organization == "OHT7" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA Decision" &
             data$fy == "2027"]
))
# Table 9.2 | OHT3 | Written Feedback Provided Within Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Written Feedback Provided Within Goal" &
             data$fy == "2026"]
))
# Table 9.2 | OHT3 | Written Feedback Provided Within Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT3" &
             data$performance_metric == "Written Feedback Provided Within Goal" &
             data$fy == "2027"]
))
# Table 9.3 | OHT5 | Number with Written Feedback Sent... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2026"]
))
# Table 9.3 | OHT5 | Number with Written Feedback Sent... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT5" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2027"]
))
# Table 9.5 | OHT5 | Meeting Minutes Past 15 Days of Meeting... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT5" &
             data$performance_metric == "Meeting Minutes Past 15 Days of Meeting" &
             data$fy == "2026"]
))
# Table 9.5 | OHT5 | Meeting Minutes Past 15 Days of Meeting... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT5" &
             data$performance_metric == "Meeting Minutes Past 15 Days of Meeting" &
             data$fy == "2027"]
))
# Table 9.2 | OHT6 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT6" &
             data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
             data$fy == "2023"]
))
# Table 9.2 | OHT6 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT6" &
             data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
             data$fy == "2024"]
))
# Table 9.2 | OHT6 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT6" &
             data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
             data$fy == "2025"]
))
# Table 9.2 | OHT6 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT6" &
             data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
             data$fy == "2026"]
))
# Table 9.2 | OHT6 | FYs 2025, 2026, and 2027. If the Pre-Sub... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.2" &
             data$organization == "OHT6" &
             data$performance_metric == "FYs 2025, 2026, and 2027. If the Pre-Sub MDUFA goal is met for FY 2024, the maximum number of submissions subject to the goal will" &
             data$fy == "2027"]
))
# Table 9.5 | OHT6 | Meeting Minutes Submitted Within 15 Days... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT6" &
             data$performance_metric == "Meeting Minutes Submitted Within 15 Days of Meeting" &
             data$fy == "2026"]
))
# Table 9.5 | OHT6 | Meeting Minutes Submitted Within 15 Days... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.5" &
             data$organization == "OHT6" &
             data$performance_metric == "Meeting Minutes Submitted Within 15 Days of Meeting" &
             data$fy == "2027"]
))
# Table 9.3 | OHT8 | Number with Written Feedback Sent... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2026"]
))
# Table 9.3 | OHT8 | Number with Written Feedback Sent... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "9.3" &
             data$organization == "OHT8" &
             data$performance_metric == "Number with Written Feedback Sent" &
             data$fy == "2027"]
))
# Table 11.1 | CDRH | Denial without SI... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "11.1" &
             data$organization == "CDRH" &
             data$performance_metric == "Denial without SI" &
             data$fy == "2026"]
))
# Table 11.1 | CDRH | Denial without SI... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "11.1" &
             data$organization == "CDRH" &
             data$performance_metric == "Denial without SI" &
             data$fy == "2027"]
))
# Table 1.5 | CBER | Number of PMAs Filed... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of PMAs Filed" &
             data$fy == "2026"]
))
# Table 1.5 | CBER | Number of PMAs Filed... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of PMAs Filed" &
             data$fy == "2027"]
))
# Table 2.2 | CBER | Current Performance Percent Goal Met... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2026"]
))
# Table 2.2 | CBER | Current Performance Percent Goal Met... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "2.2" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2027"]
))
# Table 6.5 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 6.5 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 7.1 | CBER | Average Number of Days to Accept/Refuse ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "7.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Days to Accept/Refuse to Accept" &
             data$fy == "2026"]
))
# Table 7.1 | CBER | Average Number of Days to Accept/Refuse ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "7.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Days to Accept/Refuse to Accept" &
             data$fy == "2027"]
))
# Table 7.2 | CBER | Number With MDUFA V Decision After Trimm... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "7.2" &
             data$organization == "CBER" &
             data$performance_metric == "Number With MDUFA V Decision After Trimming the Upper and Lower 2%" &
             data$fy == "2026"]
))
# Table 7.2 | CBER | Number With MDUFA V Decision After Trimm... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "7.2" &
             data$organization == "CBER" &
             data$performance_metric == "Number With MDUFA V Decision After Trimming the Upper and Lower 2%" &
             data$fy == "2027"]
))
# Table 8.5 | CBER | Number of Submissions that Missed the Go... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "8.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 8.5 | CBER | Number of Submissions that Missed the Go... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "8.5" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 6.5 | OHT6 | 20th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT6" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 6.5 | OHT6 | 20th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "6.5" &
             data$organization == "OHT6" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.5 | CBER | Current Performance Percent Goal Met... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2024"]
))
# Table 1.5 | CBER | Current Performance Percent Goal Met... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2025"]
))
# Table 1.5 | CBER | Current Performance Percent Goal Met... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2026"]
))
# Table 1.5 | CBER | Current Performance Percent Goal Met... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.5" &
             data$organization == "CBER" &
             data$performance_metric == "Current Performance Percent Goal Met" &
             data$fy == "2027"]
))
# Table 1.6 | CBER | Non-MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.6" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.6 | CBER | Non-MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.6" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | Number with MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | Number with MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | 80th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | 80th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | 80th Percentile Industry Days to MDUFA V... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | 80th Percentile Industry Days to MDUFA V... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | 20th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | 20th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.7 | CBER | 80th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.7 | CBER | 80th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.7" &
             data$organization == "CBER" &
             data$performance_metric == "80th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Number with MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Number with MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Number with MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Average FDA Days to MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Average FDA Days to MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | 20th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | 20th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "20th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | 40th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | 60th Percentile FDA Days to MDUFA V Deci... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | 60th Percentile FDA Days to MDUFA V Deci... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Maximum FDA Days to MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Maximum FDA Days to MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum FDA Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | 40th Percentile Industry Days to MDUFA V... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "40th Percentile Industry Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Maximum Industry Days to MDUFA V Decisio... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Industry Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Average Total Days to MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Average Total Days to MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Average Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | 60th Percentile Total Days to MDUFA V De... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | 60th Percentile Total Days to MDUFA V De... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "60th Percentile Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.8 | CBER | Maximum Total Days to MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Total Days to MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.8 | CBER | Maximum Total Days to MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.8" &
             data$organization == "CBER" &
             data$performance_metric == "Maximum Total Days to MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.9 | CBER | Number of Deleted... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2026"]
))
# Table 1.9 | CBER | Number of Deleted... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2027"]
))
# Table 1.9 | CBER | Rate of Not Approvable... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2024"]
))
# Table 1.9 | CBER | Rate of Not Approvable... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2025"]
))
# Table 1.9 | CBER | Rate of Not Approvable... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2026"]
))
# Table 1.9 | CBER | Rate of Not Approvable... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.9" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2027"]
))
# Table 1.10 | CBER | Number of Not Approvable... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Not Approvable" &
             data$fy == "2026"]
))
# Table 1.10 | CBER | Number of Not Approvable... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Not Approvable" &
             data$fy == "2027"]
))
# Table 1.10 | CBER | Number of Deleted... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2026"]
))
# Table 1.10 | CBER | Number of Deleted... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Number of Deleted" &
             data$fy == "2027"]
))
# Table 1.10 | CBER | Rate of Not Approvable... | FY 2023 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2023"]
))
# Table 1.10 | CBER | Rate of Not Approvable... | FY 2024 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2024"]
))
# Table 1.10 | CBER | Rate of Not Approvable... | FY 2025 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2025"]
))
# Table 1.10 | CBER | Rate of Not Approvable... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2026"]
))
# Table 1.10 | CBER | Rate of Not Approvable... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.10" &
             data$organization == "CBER" &
             data$performance_metric == "Rate of Not Approvable" &
             data$fy == "2027"]
))
# Table 1.11 | CBER | Mean FDA Days for Submissions that Misse... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.11 | CBER | Mean FDA Days for Submissions that Misse... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.11 | CBER | Mean Industry Days for Submissions that ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.11 | CBER | Mean Industry Days for Submissions that ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.11" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.12 | CBER | Mean Industry Days for Submissions that ... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.12" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2026"]
))
# Table 1.12 | CBER | Mean Industry Days for Submissions that ... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.12" &
             data$organization == "CBER" &
             data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
             data$fy == "2027"]
))
# Table 1.13 | CBER | Non-MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.13 | CBER | Non-MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.13 | CBER | MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.13 | CBER | MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.13 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2026"]
))
# Table 1.13 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.13" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2027"]
))
# Table 1.14 | CBER | Non-MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.14 | CBER | Non-MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "Non-MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.14 | CBER | MDUFA V Decision... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2026"]
))
# Table 1.14 | CBER | MDUFA V Decision... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "MDUFA V Decision" &
             data$fy == "2027"]
))
# Table 1.14 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2026"]
))
# Table 1.14 | CBER | PMAs Pending MDUFA V Decision Past Goal... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "1.14" &
             data$organization == "CBER" &
             data$performance_metric == "PMAs Pending MDUFA V Decision Past Goal" &
             data$fy == "2027"]
))
# Table 10.1 | CBER | Average Number of Amendments Prior to ID... | FY 2026 = NA
expect_true(is.na(
  data$value[data$table_number == "10.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
             data$fy == "2026"]
))
# Table 10.1 | CBER | Average Number of Amendments Prior to ID... | FY 2027 = NA
expect_true(is.na(
  data$value[data$table_number == "10.1" &
             data$organization == "CBER" &
             data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
             data$fy == "2027"]
))})
# nolint end
