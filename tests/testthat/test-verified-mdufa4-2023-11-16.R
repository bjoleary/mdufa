# nolint start
# Verified extraction tests for MDUFA IV 2023-11-16 report
# Generated: 2025-12-14
# Verifier: Brendan O'Leary
# Sample size: 74 metrics, 370 values
# Statistical basis: LB of 95% CI > 90% (Wilson score)

# Helper function to find local PDF (works from testthat directory)
find_local_pdf <- function(pattern) {
  # testthat runs from tests/testthat, so go up two levels
  pdf_dir <- testthat::test_path("..", "..", "data-raw", "pdf_reports")
  files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[1]
}

test_that("MDUFA IV 2023-11-16 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-4_2023-11-16")
  skip_if(is.null(pdf_path), "MDUFA IV PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA IV"))
# Table 1.5 | CDRH | MDUFA IV Decision Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2018")],
  "65"
)
# Table 1.5 | CDRH | MDUFA IV Decision Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2019")],
  "48"
)
# Table 1.5 | CDRH | MDUFA IV Decision Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2020")],
  "64"
)
# Table 1.5 | CDRH | MDUFA IV Decision Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2021")],
  "51"
)
# Table 1.5 | CDRH | MDUFA IV Decision Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2022")],
  "31"
)
# Table 1.8 | CDRH | 40th Percentile Total Days to MDUFA IV D... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "CDRH" &
                   data$performance_metric == "40th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "297"
)
# Table 1.8 | CDRH | 40th Percentile Total Days to MDUFA IV D... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "CDRH" &
                   data$performance_metric == "40th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "548"
)
# Table 1.8 | CDRH | 40th Percentile Total Days to MDUFA IV D... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "CDRH" &
                   data$performance_metric == "40th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "430"
)
# Table 1.8 | CDRH | 40th Percentile Total Days to MDUFA IV D... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "CDRH" &
                   data$performance_metric == "40th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "161"
)
# Table 1.8 | CDRH | 40th Percentile Total Days to MDUFA IV D... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "CDRH" &
                   data$performance_metric == "40th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "0"
)
# Table 1.7 | OHT1 | Maximum FDA Days to MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Maximum FDA Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "180"
)
# Table 1.7 | OHT1 | Maximum FDA Days to MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Maximum FDA Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "180"
)
# Table 1.7 | OHT1 | Maximum FDA Days to MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Maximum FDA Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "180"
)
# Table 1.7 | OHT1 | Maximum FDA Days to MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Maximum FDA Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "201"
)
# Table 1.7 | OHT1 | Maximum FDA Days to MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Maximum FDA Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "180"
)
# Table 1.13 | OHT2 | Performance Metric... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.13" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2018")],
  "90% Within 180 FDA Days"
)
# Table 1.13 | OHT2 | Performance Metric... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.13" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2019")],
  "90% Within 180 FDA Days"
)
# Table 1.13 | OHT2 | Performance Metric... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.13" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2020")],
  "90% Within 180 FDA Days"
)
# Table 1.13 | OHT2 | Performance Metric... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.13" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2021")],
  "90% Within 180 FDA Days"
)
# Table 1.13 | OHT2 | Performance Metric... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.13" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2022")],
  "90% Within 180 FDA Days"
)
# Table 1.14 | OHT2 | Performance Metric... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2018")],
  "90% Within 320 FDA Days"
)
# Table 1.14 | OHT2 | Performance Metric... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2019")],
  "90% Within 320 FDA Days"
)
# Table 1.14 | OHT2 | Performance Metric... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2020")],
  "90% Within 320 FDA Days"
)
# Table 1.14 | OHT2 | Performance Metric... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2021")],
  "90% Within 320 FDA Days"
)
# Table 1.14 | OHT2 | Performance Metric... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2022")],
  "90% Within 320 FDA Days"
)
# Table 1.7 | OHT4 | 80th Percentile FDA Days to MDUFA IV Dec... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "213"
)
# Table 1.7 | OHT4 | 80th Percentile FDA Days to MDUFA IV Dec... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "183"
)
# Table 1.7 | OHT4 | 80th Percentile FDA Days to MDUFA IV Dec... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "209"
)
# Table 1.7 | OHT4 | 80th Percentile FDA Days to MDUFA IV Dec... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "230"
)
# Table 1.7 | OHT4 | 80th Percentile FDA Days to MDUFA IV Dec... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "366"
)
# Table 1.1 | OHT5 | Number Without a RTA Review and <= 15 Da... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2018")],
  "0"
)
# Table 1.1 | OHT5 | Number Without a RTA Review and <= 15 Da... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2019")],
  "0"
)
# Table 1.1 | OHT5 | Number Without a RTA Review and <= 15 Da... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2020")],
  "0"
)
# Table 1.1 | OHT5 | Number Without a RTA Review and <= 15 Da... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2021")],
  "0"
)
# Table 1.1 | OHT5 | Number Without a RTA Review and <= 15 Da... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2022")],
  "0"
)
# Table 1.7 | OHT6 | 20th Percentile FDA Days to MDUFA IV Dec... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "20th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "180"
)
# Table 1.7 | OHT6 | 20th Percentile FDA Days to MDUFA IV Dec... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "20th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "121"
)
# Table 1.7 | OHT6 | 20th Percentile FDA Days to MDUFA IV Dec... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "20th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "178"
)
# Table 1.7 | OHT6 | 20th Percentile FDA Days to MDUFA IV Dec... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "20th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "130"
)
# Table 1.7 | OHT6 | 20th Percentile FDA Days to MDUFA IV Dec... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "20th Percentile FDA Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "174"
)
# Table 1.10 | OHT7 | Number of Withdrawal... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2018")],
  "0"
)
# Table 1.10 | OHT7 | Number of Withdrawal... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2019")],
  "0"
)
# Table 1.10 | OHT7 | Number of Withdrawal... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2020")],
  "0"
)
# Table 1.10 | OHT7 | Number of Withdrawal... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2021")],
  "0"
)
# Table 1.10 | OHT7 | Number of Withdrawal... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2022")],
  "0"
)
# Table 1.14 | OHT7 | Non-MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 1.14 | OHT7 | Non-MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 1.14 | OHT7 | Non-MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 1.14 | OHT7 | Non-MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2021")],
  "1"
)
# Table 1.14 | OHT7 | Non-MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2022")],
  "0"
)
# Table 1.14 | OHT7 | Current Performance Percent Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2018")],
  "100.00%"
)
# Table 1.14 | OHT7 | Current Performance Percent Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2019")],
  "76.47%"
)
# Table 1.14 | OHT7 | Current Performance Percent Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2020")],
  "85.71%"
)
# Table 1.14 | OHT7 | Current Performance Percent Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2021")],
  "42.11%"
)
# Table 1.14 | OHT7 | Current Performance Percent Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.14" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2022")],
  "75.00%"
)
# Table 1.3 | OHT8 | SI Goal Not Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2018")],
  "0"
)
# Table 1.3 | OHT8 | SI Goal Not Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2019")],
  "0"
)
# Table 1.3 | OHT8 | SI Goal Not Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2020")],
  "0"
)
# Table 1.3 | OHT8 | SI Goal Not Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2021")],
  "0"
)
# Table 1.3 | OHT8 | SI Goal Not Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2022")],
  "0"
)
# Table 1.3 | OHT8 | Closed Without SI... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Closed Without SI" &
                   data$fy == "2018")],
  "0"
)
# Table 1.3 | OHT8 | Closed Without SI... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Closed Without SI" &
                   data$fy == "2019")],
  "0"
)
# Table 1.3 | OHT8 | Closed Without SI... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Closed Without SI" &
                   data$fy == "2020")],
  "0"
)
# Table 1.3 | OHT8 | Closed Without SI... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Closed Without SI" &
                   data$fy == "2021")],
  "0"
)
# Table 1.3 | OHT8 | Closed Without SI... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Closed Without SI" &
                   data$fy == "2022")],
  "0"
)
# Table 1.6 | OHT8 | Performance Metric... | FY 2018
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2018")],
  "90% Within 320 FDA Days"
)
# Table 1.6 | OHT8 | Performance Metric... | FY 2019
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2019")],
  "90% Within 320 FDA Days"
)
# Table 1.6 | OHT8 | Performance Metric... | FY 2020
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2020")],
  "90% Within 320 FDA Days"
)
# Table 1.6 | OHT8 | Performance Metric... | FY 2021
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2021")],
  "90% Within 320 FDA Days"
)
# Table 1.6 | OHT8 | Performance Metric... | FY 2022
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Performance Metric" &
                   data$fy == "2022")],
  "90% Within 320 FDA Days"
)
# Table 2.1 | CDRH | Current SI Performance Percent Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2018")],
  "98.38%"
)
# Table 2.1 | CDRH | Current SI Performance Percent Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2019")],
  "98.95%"
)
# Table 2.1 | CDRH | Current SI Performance Percent Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2020")],
  "93.06%"
)
# Table 2.1 | CDRH | Current SI Performance Percent Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2021")],
  "81.77%"
)
# Table 2.1 | CDRH | Current SI Performance Percent Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2022")],
  "78.57%"
)
# Table 2.2 | CDRH | Supplements Pending MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 2.2 | CDRH | Supplements Pending MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 2.2 | CDRH | Supplements Pending MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 2.2 | CDRH | Supplements Pending MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2021")],
  "1"
)
# Table 2.2 | CDRH | Supplements Pending MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2022")],
  "7"
)
# Table 2.3 | CDRH | Number with MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with MDUFA IV Decision" &
                   data$fy == "2018")],
  "180"
)
# Table 2.3 | CDRH | Number with MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with MDUFA IV Decision" &
                   data$fy == "2019")],
  "185"
)
# Table 2.3 | CDRH | Number with MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with MDUFA IV Decision" &
                   data$fy == "2020")],
  "164"
)
# Table 2.3 | CDRH | Number with MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with MDUFA IV Decision" &
                   data$fy == "2021")],
  "175"
)
# Table 2.3 | CDRH | Number with MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with MDUFA IV Decision" &
                   data$fy == "2022")],
  "129"
)
# Table 2.3 | CDRH | Number of Not Approvable... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Not Approvable" &
                   data$fy == "2018")],
  "13"
)
# Table 2.3 | CDRH | Number of Not Approvable... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Not Approvable" &
                   data$fy == "2019")],
  "10"
)
# Table 2.3 | CDRH | Number of Not Approvable... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Not Approvable" &
                   data$fy == "2020")],
  "9"
)
# Table 2.3 | CDRH | Number of Not Approvable... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Not Approvable" &
                   data$fy == "2021")],
  "13"
)
# Table 2.3 | CDRH | Number of Not Approvable... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Not Approvable" &
                   data$fy == "2022")],
  "7"
)
# Table 2.2 | OHT1 | MDUFA IV Decision Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2018")],
  "20"
)
# Table 2.2 | OHT1 | MDUFA IV Decision Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2019")],
  "35"
)
# Table 2.2 | OHT1 | MDUFA IV Decision Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2020")],
  "28"
)
# Table 2.2 | OHT1 | MDUFA IV Decision Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2021")],
  "14"
)
# Table 2.2 | OHT1 | MDUFA IV Decision Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "MDUFA IV Decision Goal Met" &
                   data$fy == "2022")],
  "18"
)
# Table 2.2 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 2.2 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 2.2 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 2.2 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2021")],
  "0"
)
# Table 2.2 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2022")],
  "1"
)
# Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2018")],
  "93.33%"
)
# Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2019")],
  "93.75%"
)
# Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2020")],
  "100.00%"
)
# Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2021")],
  "94.74%"
)
# Table 2.1 | OHT3 | Current SI Performance Percent Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2022")],
  "100.00%"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA IV Decision Pa... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2018")],
  "0"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA IV Decision Pa... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2019")],
  "0"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA IV Decision Pa... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2020")],
  "0"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA IV Decision Pa... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2021")],
  "1"
)
# Table 2.2 | OHT4 | Supplements Pending MDUFA IV Decision Pa... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2022")],
  "0"
)
# Table 2.2 | OHT4 | Current Performance Percent Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2018")],
  "100.00%"
)
# Table 2.2 | OHT4 | Current Performance Percent Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2019")],
  "55.56%"
)
# Table 2.2 | OHT4 | Current Performance Percent Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2020")],
  "85.71%"
)
# Table 2.2 | OHT4 | Current Performance Percent Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2021")],
  "60.00%"
)
# Table 2.2 | OHT4 | Current Performance Percent Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2022")],
  "60.00%"
)
# Table 2.2 | OHT7 | Non-MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2018")],
  "2"
)
# Table 2.2 | OHT7 | Non-MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 2.2 | OHT7 | Non-MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 2.2 | OHT7 | Non-MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2021")],
  "1"
)
# Table 2.2 | OHT7 | Non-MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2022")],
  "2"
)
# Table 3.1 | CDRH | Supplements Pending MDUFA IV Decision Pa... | FY 2018
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2018")],
  "0"
)
# Table 3.1 | CDRH | Supplements Pending MDUFA IV Decision Pa... | FY 2019
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2019")],
  "0"
)
# Table 3.1 | CDRH | Supplements Pending MDUFA IV Decision Pa... | FY 2020
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2020")],
  "0"
)
# Table 3.1 | CDRH | Supplements Pending MDUFA IV Decision Pa... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2021")],
  "0"
)
# Table 3.1 | CDRH | Supplements Pending MDUFA IV Decision Pa... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision Past Goal" &
                   data$fy == "2022")],
  "0"
)
# Table 3.1 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 3.1 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 3.1 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 3.1 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2021")],
  "0"
)
# Table 3.1 | OHT1 | Supplements Pending MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Supplements Pending MDUFA IV Decision" &
                   data$fy == "2022")],
  "0"
)
# Table 3.1 | OHT3 | MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2018")],
  "20"
)
# Table 3.1 | OHT3 | MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2019")],
  "38"
)
# Table 3.1 | OHT3 | MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2020")],
  "36"
)
# Table 3.1 | OHT3 | MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2021")],
  "14"
)
# Table 3.1 | OHT3 | MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2022")],
  "27"
)
# Table 3.3 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2018 = NA
expect_true(is.na(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
                   data$fy == "2018")]
))
# Table 3.3 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2019 = NA
expect_true(is.na(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
                   data$fy == "2019")]
))
# Table 3.3 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2020 = NA
expect_true(is.na(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
                   data$fy == "2020")]
))
# Table 3.3 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
                   data$fy == "2021")],
  "147.00"
)
# Table 3.3 | OHT4 | Mean FDA Days for Submissions that Misse... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Mean FDA Days for Submissions that Missed the Goal" &
                   data$fy == "2022")],
  "257.00"
)
# Table 3.1 | OHT6 | Supplements Received... | FY 2018
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Supplements Received" &
                   data$fy == "2018")],
  "18"
)
# Table 3.1 | OHT6 | Supplements Received... | FY 2019
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Supplements Received" &
                   data$fy == "2019")],
  "25"
)
# Table 3.1 | OHT6 | Supplements Received... | FY 2020
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Supplements Received" &
                   data$fy == "2020")],
  "10"
)
# Table 3.1 | OHT6 | Supplements Received... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Supplements Received" &
                   data$fy == "2021")],
  "4"
)
# Table 3.1 | OHT6 | Supplements Received... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Supplements Received" &
                   data$fy == "2022")],
  "9"
)
# Table 3.1 | OHT8 | MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2018")],
  "2"
)
# Table 3.1 | OHT8 | MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2019")],
  "1"
)
# Table 3.1 | OHT8 | MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2020")],
  "1"
)
# Table 3.1 | OHT8 | MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2021")],
  "1"
)
# Table 3.1 | OHT8 | MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "MDUFA IV Decision" &
                   data$fy == "2022")],
  "0"
)
# Table 5.1 | CDRH | Original PMAs (No Panel) - Breakthrough ... | FY 2018
expect_equal(
  data$value[which(data$table_number == "5.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Original PMAs (No Panel) - Breakthrough Device" &
                   data$fy == "2018")],
  "3"
)
# Table 5.1 | CDRH | Original PMAs (No Panel) - Breakthrough ... | FY 2019
expect_equal(
  data$value[which(data$table_number == "5.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Original PMAs (No Panel) - Breakthrough Device" &
                   data$fy == "2019")],
  "2"
)
# Table 5.1 | CDRH | Original PMAs (No Panel) - Breakthrough ... | FY 2020
expect_equal(
  data$value[which(data$table_number == "5.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Original PMAs (No Panel) - Breakthrough Device" &
                   data$fy == "2020")],
  "5"
)
# Table 5.1 | CDRH | Original PMAs (No Panel) - Breakthrough ... | FY 2021
expect_equal(
  data$value[which(data$table_number == "5.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Original PMAs (No Panel) - Breakthrough Device" &
                   data$fy == "2021")],
  "7"
)
# Table 5.1 | CDRH | Original PMAs (No Panel) - Breakthrough ... | FY 2022
expect_equal(
  data$value[which(data$table_number == "5.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Original PMAs (No Panel) - Breakthrough Device" &
                   data$fy == "2022")],
  "1"
)
# Table 6.5 | CDRH | 80th Percentile Industry Days to MDUFA I... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "127"
)
# Table 6.5 | CDRH | 80th Percentile Industry Days to MDUFA I... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "139"
)
# Table 6.5 | CDRH | 80th Percentile Industry Days to MDUFA I... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "147"
)
# Table 6.5 | CDRH | 80th Percentile Industry Days to MDUFA I... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "158"
)
# Table 6.5 | CDRH | 80th Percentile Industry Days to MDUFA I... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "149"
)
# Table 6.6 | CDRH | Number With MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number With MDUFA IV Decision" &
                   data$fy == "2018")],
  "2,926"
)
# Table 6.6 | CDRH | Number With MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number With MDUFA IV Decision" &
                   data$fy == "2019")],
  "3,039"
)
# Table 6.6 | CDRH | Number With MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number With MDUFA IV Decision" &
                   data$fy == "2020")],
  "3,109"
)
# Table 6.6 | CDRH | Number With MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number With MDUFA IV Decision" &
                   data$fy == "2021")],
  "3,260"
)
# Table 6.6 | CDRH | Number With MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number With MDUFA IV Decision" &
                   data$fy == "2022")],
  "3,043"
)
# Table 6.3 | OHT1 | 80th Percentile FDA Days to Substantive ... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
                   data$fy == "2018")],
  "60"
)
# Table 6.3 | OHT1 | 80th Percentile FDA Days to Substantive ... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
                   data$fy == "2019")],
  "60"
)
# Table 6.3 | OHT1 | 80th Percentile FDA Days to Substantive ... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
                   data$fy == "2020")],
  "60"
)
# Table 6.3 | OHT1 | 80th Percentile FDA Days to Substantive ... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
                   data$fy == "2021")],
  "60"
)
# Table 6.3 | OHT1 | 80th Percentile FDA Days to Substantive ... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "80th Percentile FDA Days to Substantive Interaction" &
                   data$fy == "2022")],
  "60"
)
# Table 6.8 | OHT1 | Non-MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.8" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 6.8 | OHT1 | Non-MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.8" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 6.8 | OHT1 | Non-MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.8" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 6.8 | OHT1 | Non-MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.8" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2021")],
  "0"
)
# Table 6.8 | OHT1 | Non-MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.8" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Non-MDUFA IV Decision" &
                   data$fy == "2022")],
  "0"
)
# Table 6.5 | OHT2 | Average Review Cycles... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2018")],
  "1.71"
)
# Table 6.5 | OHT2 | Average Review Cycles... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2019")],
  "1.70"
)
# Table 6.5 | OHT2 | Average Review Cycles... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2020")],
  "1.82"
)
# Table 6.5 | OHT2 | Average Review Cycles... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2021")],
  "1.81"
)
# Table 6.5 | OHT2 | Average Review Cycles... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2022")],
  "1.83"
)
# Table 6.2 | OHT3 | Current SI Performance Percent Within 60... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Within 60 FDA Days" &
                   data$fy == "2018")],
  "97.94%"
)
# Table 6.2 | OHT3 | Current SI Performance Percent Within 60... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Within 60 FDA Days" &
                   data$fy == "2019")],
  "98.90%"
)
# Table 6.2 | OHT3 | Current SI Performance Percent Within 60... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Within 60 FDA Days" &
                   data$fy == "2020")],
  "96.88%"
)
# Table 6.2 | OHT3 | Current SI Performance Percent Within 60... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Within 60 FDA Days" &
                   data$fy == "2021")],
  "93.16%"
)
# Table 6.2 | OHT3 | Current SI Performance Percent Within 60... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Current SI Performance Percent Within 60 FDA Days" &
                   data$fy == "2022")],
  "95.57%"
)
# Table 6.4 | OHT3 | 510(k)s Pending MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "510(k)s Pending MDUFA IV Decision" &
                   data$fy == "2018")],
  "0"
)
# Table 6.4 | OHT3 | 510(k)s Pending MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "510(k)s Pending MDUFA IV Decision" &
                   data$fy == "2019")],
  "0"
)
# Table 6.4 | OHT3 | 510(k)s Pending MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "510(k)s Pending MDUFA IV Decision" &
                   data$fy == "2020")],
  "0"
)
# Table 6.4 | OHT3 | 510(k)s Pending MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "510(k)s Pending MDUFA IV Decision" &
                   data$fy == "2021")],
  "9"
)
# Table 6.4 | OHT3 | 510(k)s Pending MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "510(k)s Pending MDUFA IV Decision" &
                   data$fy == "2022")],
  "13"
)
# Table 6.6 | OHT3 | Rate of Withdrawal... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Withdrawal" &
                   data$fy == "2018")],
  "4.59%"
)
# Table 6.6 | OHT3 | Rate of Withdrawal... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Withdrawal" &
                   data$fy == "2019")],
  "6.83%"
)
# Table 6.6 | OHT3 | Rate of Withdrawal... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Withdrawal" &
                   data$fy == "2020")],
  "6.70%"
)
# Table 6.6 | OHT3 | Rate of Withdrawal... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Withdrawal" &
                   data$fy == "2021")],
  "5.10%"
)
# Table 6.6 | OHT3 | Rate of Withdrawal... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Withdrawal" &
                   data$fy == "2022")],
  "6.76%"
)
# Table 6.9 | OHT4 | MDUFA IV Decision (SE/NSE)... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.9" &
                   data$organization == "OHT4" &
                   data$performance_metric == "MDUFA IV Decision (SE/NSE)" &
                   data$fy == "2018")],
  "0"
)
# Table 6.9 | OHT4 | MDUFA IV Decision (SE/NSE)... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.9" &
                   data$organization == "OHT4" &
                   data$performance_metric == "MDUFA IV Decision (SE/NSE)" &
                   data$fy == "2019")],
  "0"
)
# Table 6.9 | OHT4 | MDUFA IV Decision (SE/NSE)... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.9" &
                   data$organization == "OHT4" &
                   data$performance_metric == "MDUFA IV Decision (SE/NSE)" &
                   data$fy == "2020")],
  "0"
)
# Table 6.9 | OHT4 | MDUFA IV Decision (SE/NSE)... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.9" &
                   data$organization == "OHT4" &
                   data$performance_metric == "MDUFA IV Decision (SE/NSE)" &
                   data$fy == "2021")],
  "0"
)
# Table 6.9 | OHT4 | MDUFA IV Decision (SE/NSE)... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.9" &
                   data$organization == "OHT4" &
                   data$performance_metric == "MDUFA IV Decision (SE/NSE)" &
                   data$fy == "2022")],
  "0"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2018")],
  "17"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2019")],
  "16"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2020")],
  "13"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2021")],
  "18"
)
# Table 6.6 | OHT5 | Number of Withdrawal... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.6" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Number of Withdrawal" &
                   data$fy == "2022")],
  "12"
)
# Table 6.5 | OHT6 | Average Number of Industry Days to MDUFA... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Average Number of Industry Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "48.84"
)
# Table 6.5 | OHT6 | Average Number of Industry Days to MDUFA... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Average Number of Industry Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "50.98"
)
# Table 6.5 | OHT6 | Average Number of Industry Days to MDUFA... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Average Number of Industry Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "54.52"
)
# Table 6.5 | OHT6 | Average Number of Industry Days to MDUFA... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Average Number of Industry Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "59.82"
)
# Table 6.5 | OHT6 | Average Number of Industry Days to MDUFA... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT6" &
                   data$performance_metric == "Average Number of Industry Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "54.54"
)
# Table 6.1 | OHT7 | Number Without a RTA Review and <= 15 Da... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2018")],
  "0"
)
# Table 6.1 | OHT7 | Number Without a RTA Review and <= 15 Da... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2019")],
  "0"
)
# Table 6.1 | OHT7 | Number Without a RTA Review and <= 15 Da... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2020")],
  "0"
)
# Table 6.1 | OHT7 | Number Without a RTA Review and <= 15 Da... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2021")],
  "0"
)
# Table 6.1 | OHT7 | Number Without a RTA Review and <= 15 Da... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2022")],
  "0"
)
# Table 6.2 | OHT7 | SI Within 60 FDA Days... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "SI Within 60 FDA Days" &
                   data$fy == "2018")],
  "271"
)
# Table 6.2 | OHT7 | SI Within 60 FDA Days... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "SI Within 60 FDA Days" &
                   data$fy == "2019")],
  "273"
)
# Table 6.2 | OHT7 | SI Within 60 FDA Days... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "SI Within 60 FDA Days" &
                   data$fy == "2020")],
  "204"
)
# Table 6.2 | OHT7 | SI Within 60 FDA Days... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "SI Within 60 FDA Days" &
                   data$fy == "2021")],
  "19"
)
# Table 6.2 | OHT7 | SI Within 60 FDA Days... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "OHT7" &
                   data$performance_metric == "SI Within 60 FDA Days" &
                   data$fy == "2022")],
  "16"
)
# Table 6.5 | OHT8 | 20th Percentile Total Days to MDUFA IV D... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT8" &
                   data$performance_metric == "20th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "30"
)
# Table 6.5 | OHT8 | 20th Percentile Total Days to MDUFA IV D... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT8" &
                   data$performance_metric == "20th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "28"
)
# Table 6.5 | OHT8 | 20th Percentile Total Days to MDUFA IV D... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT8" &
                   data$performance_metric == "20th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "29"
)
# Table 6.5 | OHT8 | 20th Percentile Total Days to MDUFA IV D... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT8" &
                   data$performance_metric == "20th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "48"
)
# Table 6.5 | OHT8 | 20th Percentile Total Days to MDUFA IV D... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OHT8" &
                   data$performance_metric == "20th Percentile Total Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "51"
)
# Table 7.3 | CDRH | Number of Third Party Submissions... | FY 2018
expect_equal(
  data$value[which(data$table_number == "7.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Third Party Submissions" &
                   data$fy == "2018")],
  "75"
)
# Table 7.3 | CDRH | Number of Third Party Submissions... | FY 2019
expect_equal(
  data$value[which(data$table_number == "7.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Third Party Submissions" &
                   data$fy == "2019")],
  "78"
)
# Table 7.3 | CDRH | Number of Third Party Submissions... | FY 2020
expect_equal(
  data$value[which(data$table_number == "7.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Third Party Submissions" &
                   data$fy == "2020")],
  "85"
)
# Table 7.3 | CDRH | Number of Third Party Submissions... | FY 2021
expect_equal(
  data$value[which(data$table_number == "7.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Third Party Submissions" &
                   data$fy == "2021")],
  "90"
)
# Table 7.3 | CDRH | Number of Third Party Submissions... | FY 2022
expect_equal(
  data$value[which(data$table_number == "7.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of Third Party Submissions" &
                   data$fy == "2022")],
  "77"
)
# Table 8.3 | OHT1 | 60th Percentile Industry Days to MDUFA I... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "60th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "75"
)
# Table 8.3 | OHT1 | 60th Percentile Industry Days to MDUFA I... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "60th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "199"
)
# Table 8.3 | OHT1 | 60th Percentile Industry Days to MDUFA I... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "60th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "276"
)
# Table 8.3 | OHT1 | 60th Percentile Industry Days to MDUFA I... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "60th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "346"
)
# Table 8.3 | OHT1 | 60th Percentile Industry Days to MDUFA I... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT1" &
                   data$performance_metric == "60th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "183"
)
# Table 8.5 | OHT1 | Number of Submissions that Missed the Go... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Number of Submissions that Missed the Goal" &
                   data$fy == "2018")],
  "3"
)
# Table 8.5 | OHT1 | Number of Submissions that Missed the Go... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Number of Submissions that Missed the Goal" &
                   data$fy == "2019")],
  "1"
)
# Table 8.5 | OHT1 | Number of Submissions that Missed the Go... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Number of Submissions that Missed the Goal" &
                   data$fy == "2020")],
  "3"
)
# Table 8.5 | OHT1 | Number of Submissions that Missed the Go... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Number of Submissions that Missed the Goal" &
                   data$fy == "2021")],
  "1"
)
# Table 8.5 | OHT1 | Number of Submissions that Missed the Go... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT1" &
                   data$performance_metric == "Number of Submissions that Missed the Goal" &
                   data$fy == "2022")],
  "1"
)
# Table 8.2 | OHT3 | Non-MDUFA IV Decisions... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Non-MDUFA IV Decisions" &
                   data$fy == "2018")],
  "0"
)
# Table 8.2 | OHT3 | Non-MDUFA IV Decisions... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Non-MDUFA IV Decisions" &
                   data$fy == "2019")],
  "0"
)
# Table 8.2 | OHT3 | Non-MDUFA IV Decisions... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Non-MDUFA IV Decisions" &
                   data$fy == "2020")],
  "0"
)
# Table 8.2 | OHT3 | Non-MDUFA IV Decisions... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Non-MDUFA IV Decisions" &
                   data$fy == "2021")],
  "0"
)
# Table 8.2 | OHT3 | Non-MDUFA IV Decisions... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.2" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Non-MDUFA IV Decisions" &
                   data$fy == "2022")],
  "0"
)
# Table 8.3 | OHT3 | Average FDA Days to MDUFA IV Decision... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Average FDA Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "100.00"
)
# Table 8.3 | OHT3 | Average FDA Days to MDUFA IV Decision... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Average FDA Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "186.55"
)
# Table 8.3 | OHT3 | Average FDA Days to MDUFA IV Decision... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Average FDA Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "161.50"
)
# Table 8.3 | OHT3 | Average FDA Days to MDUFA IV Decision... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Average FDA Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "148.67"
)
# Table 8.3 | OHT3 | Average FDA Days to MDUFA IV Decision... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Average FDA Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "127.86"
)
# Table 8.4 | OHT3 | Number With Granted Decisions... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Number With Granted Decisions" &
                   data$fy == "2018")],
  "0"
)
# Table 8.4 | OHT3 | Number With Granted Decisions... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Number With Granted Decisions" &
                   data$fy == "2019")],
  "8"
)
# Table 8.4 | OHT3 | Number With Granted Decisions... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Number With Granted Decisions" &
                   data$fy == "2020")],
  "3"
)
# Table 8.4 | OHT3 | Number With Granted Decisions... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Number With Granted Decisions" &
                   data$fy == "2021")],
  "4"
)
# Table 8.4 | OHT3 | Number With Granted Decisions... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Number With Granted Decisions" &
                   data$fy == "2022")],
  "4"
)
# Table 8.3 | OHT4 | Average Review Cycles... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2018")],
  "1.80"
)
# Table 8.3 | OHT4 | Average Review Cycles... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2019")],
  "1.50"
)
# Table 8.3 | OHT4 | Average Review Cycles... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2020")],
  "1.57"
)
# Table 8.3 | OHT4 | Average Review Cycles... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2021")],
  "1.67"
)
# Table 8.3 | OHT4 | Average Review Cycles... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Average Review Cycles" &
                   data$fy == "2022")],
  "1.75"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA I... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "165"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA I... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "195"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA I... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "331"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA I... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "364"
)
# Table 8.3 | OHT4 | 80th Percentile Industry Days to MDUFA I... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT4" &
                   data$performance_metric == "80th Percentile Industry Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "248"
)
# Table 8.5 | OHT5 | Mean Industry Days for Submissions that ... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
                   data$fy == "2018")],
  "82.75"
)
# Table 8.5 | OHT5 | Mean Industry Days for Submissions that ... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
                   data$fy == "2019")],
  "0.00"
)
# Table 8.5 | OHT5 | Mean Industry Days for Submissions that ... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
                   data$fy == "2020")],
  "169.00"
)
# Table 8.5 | OHT5 | Mean Industry Days for Submissions that ... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
                   data$fy == "2021")],
  "166.00"
)
# Table 8.5 | OHT5 | Mean Industry Days for Submissions that ... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Mean Industry Days for Submissions that Missed the Goal" &
                   data$fy == "2022")],
  "0.00"
)
# Table 8.7 | OHT5 | MDUFA IV Decisions Within 150 FDA Days... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT5" &
                   data$performance_metric == "MDUFA IV Decisions Within 150 FDA Days" &
                   data$fy == "2018")],
  "0"
)
# Table 8.7 | OHT5 | MDUFA IV Decisions Within 150 FDA Days... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT5" &
                   data$performance_metric == "MDUFA IV Decisions Within 150 FDA Days" &
                   data$fy == "2019")],
  "0"
)
# Table 8.7 | OHT5 | MDUFA IV Decisions Within 150 FDA Days... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT5" &
                   data$performance_metric == "MDUFA IV Decisions Within 150 FDA Days" &
                   data$fy == "2020")],
  "0"
)
# Table 8.7 | OHT5 | MDUFA IV Decisions Within 150 FDA Days... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT5" &
                   data$performance_metric == "MDUFA IV Decisions Within 150 FDA Days" &
                   data$fy == "2021")],
  "0"
)
# Table 8.7 | OHT5 | MDUFA IV Decisions Within 150 FDA Days... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT5" &
                   data$performance_metric == "MDUFA IV Decisions Within 150 FDA Days" &
                   data$fy == "2022")],
  "0"
)
# Table 8.7 | OHT6 | De Novos Accepted... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "De Novos Accepted" &
                   data$fy == "2018")],
  "0"
)
# Table 8.7 | OHT6 | De Novos Accepted... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "De Novos Accepted" &
                   data$fy == "2019")],
  "0"
)
# Table 8.7 | OHT6 | De Novos Accepted... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "De Novos Accepted" &
                   data$fy == "2020")],
  "0"
)
# Table 8.7 | OHT6 | De Novos Accepted... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "De Novos Accepted" &
                   data$fy == "2021")],
  "0"
)
# Table 8.7 | OHT6 | De Novos Accepted... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.7" &
                   data$organization == "OHT6" &
                   data$performance_metric == "De Novos Accepted" &
                   data$fy == "2022")],
  "0"
)
# Table 8.3 | OHT8 | Average Industry Days to MDUFA IV Decisi... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Average Industry Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "0.00"
)
# Table 8.3 | OHT8 | Average Industry Days to MDUFA IV Decisi... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Average Industry Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "15.00"
)
# Table 8.3 | OHT8 | Average Industry Days to MDUFA IV Decisi... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Average Industry Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "360.00"
)
# Table 8.3 | OHT8 | Average Industry Days to MDUFA IV Decisi... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Average Industry Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "267.50"
)
# Table 8.3 | OHT8 | Average Industry Days to MDUFA IV Decisi... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.3" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Average Industry Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "104.83"
)
# Table 8.4 | OHT8 | Number With MDUFA IV Decisions... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Number With MDUFA IV Decisions" &
                   data$fy == "2018")],
  "1"
)
# Table 8.4 | OHT8 | Number With MDUFA IV Decisions... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Number With MDUFA IV Decisions" &
                   data$fy == "2019")],
  "1"
)
# Table 8.4 | OHT8 | Number With MDUFA IV Decisions... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Number With MDUFA IV Decisions" &
                   data$fy == "2020")],
  "1"
)
# Table 8.4 | OHT8 | Number With MDUFA IV Decisions... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Number With MDUFA IV Decisions" &
                   data$fy == "2021")],
  "2"
)
# Table 8.4 | OHT8 | Number With MDUFA IV Decisions... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Number With MDUFA IV Decisions" &
                   data$fy == "2022")],
  "6"
)
# Table 9.3 | OHT2 | Average FDA Days to Written Feedback... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average FDA Days to Written Feedback" &
                   data$fy == "2018")],
  "53.02"
)
# Table 9.3 | OHT2 | Average FDA Days to Written Feedback... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average FDA Days to Written Feedback" &
                   data$fy == "2019")],
  "55.46"
)
# Table 9.3 | OHT2 | Average FDA Days to Written Feedback... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average FDA Days to Written Feedback" &
                   data$fy == "2020")],
  "56.14"
)
# Table 9.3 | OHT2 | Average FDA Days to Written Feedback... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average FDA Days to Written Feedback" &
                   data$fy == "2021")],
  "58.80"
)
# Table 9.3 | OHT2 | Average FDA Days to Written Feedback... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "OHT2" &
                   data$performance_metric == "Average FDA Days to Written Feedback" &
                   data$fy == "2022")],
  "59.11"
)
# Table 9.1 | OHT3 | Rate of Submissions Not Accepted for Rev... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2018")],
  "3.34%"
)
# Table 9.1 | OHT3 | Rate of Submissions Not Accepted for Rev... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2019")],
  "2.39%"
)
# Table 9.1 | OHT3 | Rate of Submissions Not Accepted for Rev... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2020")],
  "2.30%"
)
# Table 9.1 | OHT3 | Rate of Submissions Not Accepted for Rev... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2021")],
  "1.44%"
)
# Table 9.1 | OHT3 | Rate of Submissions Not Accepted for Rev... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2022")],
  "2.42%"
)
# Table 9.5 | OHT3 | Meeting Minutes Not Submitted and <= 15 ... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
                   data$fy == "2018")],
  "0"
)
# Table 9.5 | OHT3 | Meeting Minutes Not Submitted and <= 15 ... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
                   data$fy == "2019")],
  "0"
)
# Table 9.5 | OHT3 | Meeting Minutes Not Submitted and <= 15 ... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
                   data$fy == "2020")],
  "0"
)
# Table 9.5 | OHT3 | Meeting Minutes Not Submitted and <= 15 ... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
                   data$fy == "2021")],
  "0"
)
# Table 9.5 | OHT3 | Meeting Minutes Not Submitted and <= 15 ... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT3" &
                   data$performance_metric == "Meeting Minutes Not Submitted and <= 15 Days Since Meeting Date" &
                   data$fy == "2022")],
  "0"
)
# Table 9.1 | OHT4 | Number Without a RTA Review and <= 15 Da... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2018")],
  "0"
)
# Table 9.1 | OHT4 | Number Without a RTA Review and <= 15 Da... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2019")],
  "0"
)
# Table 9.1 | OHT4 | Number Without a RTA Review and <= 15 Da... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2020")],
  "0"
)
# Table 9.1 | OHT4 | Number Without a RTA Review and <= 15 Da... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2021")],
  "0"
)
# Table 9.1 | OHT4 | Number Without a RTA Review and <= 15 Da... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT4" &
                   data$performance_metric == "Number Without a RTA Review and <= 15 Days Since Date Received" &
                   data$fy == "2022")],
  "0"
)
# Table 9.5 | OHT5 | Meeting Minutes Not Submitted and >15 Da... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Meeting Minutes Not Submitted and >15 Days Since Meeting" &
                   data$fy == "2018")],
  "7"
)
# Table 9.5 | OHT5 | Meeting Minutes Not Submitted and >15 Da... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Meeting Minutes Not Submitted and >15 Days Since Meeting" &
                   data$fy == "2019")],
  "12"
)
# Table 9.5 | OHT5 | Meeting Minutes Not Submitted and >15 Da... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Meeting Minutes Not Submitted and >15 Days Since Meeting" &
                   data$fy == "2020")],
  "7"
)
# Table 9.5 | OHT5 | Meeting Minutes Not Submitted and >15 Da... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Meeting Minutes Not Submitted and >15 Days Since Meeting" &
                   data$fy == "2021")],
  "10"
)
# Table 9.5 | OHT5 | Meeting Minutes Not Submitted and >15 Da... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.5" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Meeting Minutes Not Submitted and >15 Days Since Meeting" &
                   data$fy == "2022")],
  "8"
)
# Table 9.1 | OHT7 | Number Received... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2018")],
  "759"
)
# Table 9.1 | OHT7 | Number Received... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2019")],
  "906"
)
# Table 9.1 | OHT7 | Number Received... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2020")],
  "765"
)
# Table 9.1 | OHT7 | Number Received... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2021")],
  "434"
)
# Table 9.1 | OHT7 | Number Received... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT7" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2022")],
  "533"
)
# Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2018")],
  "3.82%"
)
# Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2019")],
  "2.66%"
)
# Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2020")],
  "0.96%"
)
# Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2021")],
  "0.48%"
)
# Table 9.1 | OHT8 | Rate of Submissions Not Accepted for Rev... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OHT8" &
                   data$performance_metric == "Rate of Submissions Not Accepted for Review" &
                   data$fy == "2022")],
  "0.84%"
)
# Table 10.1 | OHT5 | Average Number of Amendments Prior to ID... | FY 2018
expect_equal(
  data$value[which(data$table_number == "10.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
                   data$fy == "2018")],
  "0.16"
)
# Table 10.1 | OHT5 | Average Number of Amendments Prior to ID... | FY 2019
expect_equal(
  data$value[which(data$table_number == "10.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
                   data$fy == "2019")],
  "0.47"
)
# Table 10.1 | OHT5 | Average Number of Amendments Prior to ID... | FY 2020
expect_equal(
  data$value[which(data$table_number == "10.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
                   data$fy == "2020")],
  "0.17"
)
# Table 10.1 | OHT5 | Average Number of Amendments Prior to ID... | FY 2021
expect_equal(
  data$value[which(data$table_number == "10.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
                   data$fy == "2021")],
  "0.14"
)
# Table 10.1 | OHT5 | Average Number of Amendments Prior to ID... | FY 2022
expect_equal(
  data$value[which(data$table_number == "10.1" &
                   data$organization == "OHT5" &
                   data$performance_metric == "Average Number of Amendments Prior to IDE Approval or Conditional Approval" &
                   data$fy == "2022")],
  "0.16"
)
# Table 11.3 | CDRH | MDUFA IV Decisions... | FY 2018
expect_equal(
  data$value[which(data$table_number == "11.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decisions" &
                   data$fy == "2018")],
  "4"
)
# Table 11.3 | CDRH | MDUFA IV Decisions... | FY 2019
expect_equal(
  data$value[which(data$table_number == "11.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decisions" &
                   data$fy == "2019")],
  "8 (12)"
)
# Table 11.3 | CDRH | MDUFA IV Decisions... | FY 2020
expect_equal(
  data$value[which(data$table_number == "11.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decisions" &
                   data$fy == "2020")],
  "1"
)
# Table 11.3 | CDRH | MDUFA IV Decisions... | FY 2021
expect_equal(
  data$value[which(data$table_number == "11.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decisions" &
                   data$fy == "2021")],
  "3 (4)"
)
# Table 11.3 | CDRH | MDUFA IV Decisions... | FY 2022
expect_equal(
  data$value[which(data$table_number == "11.3" &
                   data$organization == "CDRH" &
                   data$performance_metric == "MDUFA IV Decisions" &
                   data$fy == "2022")],
  "1 (5)"
)
# Table 12.1 | CDRH | SI over 90 FDA days... | FY 2018
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "SI over 90 FDA days" &
                   data$fy == "2018")],
  "0"
)
# Table 12.1 | CDRH | SI over 90 FDA days... | FY 2019
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "SI over 90 FDA days" &
                   data$fy == "2019")],
  "0"
)
# Table 12.1 | CDRH | SI over 90 FDA days... | FY 2020
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "SI over 90 FDA days" &
                   data$fy == "2020")],
  "0"
)
# Table 12.1 | CDRH | SI over 90 FDA days... | FY 2021
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "SI over 90 FDA days" &
                   data$fy == "2021")],
  "4"
)
# Table 12.1 | CDRH | SI over 90 FDA days... | FY 2022
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "SI over 90 FDA days" &
                   data$fy == "2022")],
  "9 (13)"
)
# Table 12.1 | CDRH | Current SI Performance Percent within 90... | FY 2018
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent within 90 FDA days*" &
                   data$fy == "2018")],
  "100.00%"
)
# Table 12.1 | CDRH | Current SI Performance Percent within 90... | FY 2019
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent within 90 FDA days*" &
                   data$fy == "2019")],
  "N/A*"
)
# Table 12.1 | CDRH | Current SI Performance Percent within 90... | FY 2020
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent within 90 FDA days*" &
                   data$fy == "2020")],
  "100.00%"
)
# Table 12.1 | CDRH | Current SI Performance Percent within 90... | FY 2021
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent within 90 FDA days*" &
                   data$fy == "2021")],
  "N/A*"
)
# Table 12.1 | CDRH | Current SI Performance Percent within 90... | FY 2022
expect_equal(
  data$value[which(data$table_number == "12.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Current SI Performance Percent within 90 FDA days*" &
                   data$fy == "2022")],
  "0.00%"
)
# Table 2.1 | CBER | SI Pending Within Goal... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "SI Pending Within Goal" &
                   data$fy == "2018")],
  "0"
)
# Table 2.1 | CBER | SI Pending Within Goal... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "SI Pending Within Goal" &
                   data$fy == "2019")],
  "0"
)
# Table 2.1 | CBER | SI Pending Within Goal... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "SI Pending Within Goal" &
                   data$fy == "2020")],
  "0"
)
# Table 2.1 | CBER | SI Pending Within Goal... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "SI Pending Within Goal" &
                   data$fy == "2021")],
  "0"
)
# Table 2.1 | CBER | SI Pending Within Goal... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "SI Pending Within Goal" &
                   data$fy == "2022")],
  "0"
)
# Table 2.1 | CBER | Current SI Performance Percent Goal Met... | FY 2018
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2018")],
  "100.00%"
)
# Table 2.1 | CBER | Current SI Performance Percent Goal Met... | FY 2019
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2019")],
  "100.00%"
)
# Table 2.1 | CBER | Current SI Performance Percent Goal Met... | FY 2020
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2020")],
  "100.00%"
)
# Table 2.1 | CBER | Current SI Performance Percent Goal Met... | FY 2021
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2021")],
  "100.00%"
)
# Table 2.1 | CBER | Current SI Performance Percent Goal Met... | FY 2022
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Current SI Performance Percent Goal Met" &
                   data$fy == "2022")],
  "85.71%"
)
# Table 6.5 | CBER | Average Number of Total Days to MDUFA IV... | FY 2018
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "Average Number of Total Days to MDUFA IV Decision" &
                   data$fy == "2018")],
  "100.84"
)
# Table 6.5 | CBER | Average Number of Total Days to MDUFA IV... | FY 2019
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "Average Number of Total Days to MDUFA IV Decision" &
                   data$fy == "2019")],
  "143.24"
)
# Table 6.5 | CBER | Average Number of Total Days to MDUFA IV... | FY 2020
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "Average Number of Total Days to MDUFA IV Decision" &
                   data$fy == "2020")],
  "81.05"
)
# Table 6.5 | CBER | Average Number of Total Days to MDUFA IV... | FY 2021
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "Average Number of Total Days to MDUFA IV Decision" &
                   data$fy == "2021")],
  "127.70"
)
# Table 6.5 | CBER | Average Number of Total Days to MDUFA IV... | FY 2022
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "Average Number of Total Days to MDUFA IV Decision" &
                   data$fy == "2022")],
  "130.72"
)
# Table 8.4 | CBER | Number Deleted... | FY 2018
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number Deleted" &
                   data$fy == "2018")],
  "0"
)
# Table 8.4 | CBER | Number Deleted... | FY 2019
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number Deleted" &
                   data$fy == "2019")],
  "0"
)
# Table 8.4 | CBER | Number Deleted... | FY 2020
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number Deleted" &
                   data$fy == "2020")],
  "0"
)
# Table 8.4 | CBER | Number Deleted... | FY 2021
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number Deleted" &
                   data$fy == "2021")],
  "0"
)
# Table 8.4 | CBER | Number Deleted... | FY 2022
expect_equal(
  data$value[which(data$table_number == "8.4" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number Deleted" &
                   data$fy == "2022")],
  "1"
)
# Table 9.3 | CBER | 80th Percentile FDA Days to Written Feed... | FY 2018
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "CBER" &
                   data$performance_metric == "80th Percentile FDA Days to Written Feedback" &
                   data$fy == "2018")],
  "67"
)
# Table 9.3 | CBER | 80th Percentile FDA Days to Written Feed... | FY 2019
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "CBER" &
                   data$performance_metric == "80th Percentile FDA Days to Written Feedback" &
                   data$fy == "2019")],
  "68"
)
# Table 9.3 | CBER | 80th Percentile FDA Days to Written Feed... | FY 2020
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "CBER" &
                   data$performance_metric == "80th Percentile FDA Days to Written Feedback" &
                   data$fy == "2020")],
  "68"
)
# Table 9.3 | CBER | 80th Percentile FDA Days to Written Feed... | FY 2021
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "CBER" &
                   data$performance_metric == "80th Percentile FDA Days to Written Feedback" &
                   data$fy == "2021")],
  "66"
)
# Table 9.3 | CBER | 80th Percentile FDA Days to Written Feed... | FY 2022
expect_equal(
  data$value[which(data$table_number == "9.3" &
                   data$organization == "CBER" &
                   data$performance_metric == "80th Percentile FDA Days to Written Feedback" &
                   data$fy == "2022")],
  "69"
)})
# nolint end

