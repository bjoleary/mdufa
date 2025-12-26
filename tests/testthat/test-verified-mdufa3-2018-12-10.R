# nolint start
# Verified extraction tests for MDUFA III 2018-12-10 report
# Generated: 2025-12-14
# Verifier: Brendan O'Leary
# Sample size: 54 metrics, 270 values
# Statistical basis: LB of 95% CI > 90% (Wilson score)

test_that("MDUFA III 2018-12-10 extraction is accurate", {
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("mdufa-3_2018-12-10")
  skip_if(is.null(pdf_path), "MDUFA III PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "MDUFA III"))
# Table 1.4 | ODE | 80th Percentile FDA days to Substantive ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2013")],
  "90"
)
# Table 1.4 | ODE | 80th Percentile FDA days to Substantive ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2014")],
  "90"
)
# Table 1.4 | ODE | 80th Percentile FDA days to Substantive ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "90"
)
# Table 1.4 | ODE | 80th Percentile FDA days to Substantive ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2016")],
  "90"
)
# Table 1.4 | ODE | 80th Percentile FDA days to Substantive ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "90"
)
# Table 1.8 | DAGRID | 20th Percentile FDA days to MDUFA III de... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "20th Percentile FDA days to MDUFA III decision" &
                   data$fy == "2013")],
  "320"
)
# Table 1.8 | DAGRID | 20th Percentile FDA days to MDUFA III de... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "20th Percentile FDA days to MDUFA III decision" &
                   data$fy == "2014")],
  "0"
)
# Table 1.8 | DAGRID | 20th Percentile FDA days to MDUFA III de... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "20th Percentile FDA days to MDUFA III decision" &
                   data$fy == "2015")],
  "0"
)
# Table 1.8 | DAGRID | 20th Percentile FDA days to MDUFA III de... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "20th Percentile FDA days to MDUFA III decision" &
                   data$fy == "2016")],
  "0"
)
# Table 1.8 | DAGRID | 20th Percentile FDA days to MDUFA III de... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "20th Percentile FDA days to MDUFA III decision" &
                   data$fy == "2017")],
  "320"
)
# Table 1.7 | DNPMD | 40th Percentile Industry days to MDUFA I... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DNPMD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "0"
)
# Table 1.7 | DNPMD | 40th Percentile Industry days to MDUFA I... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DNPMD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "69"
)
# Table 1.7 | DNPMD | 40th Percentile Industry days to MDUFA I... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DNPMD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "36"
)
# Table 1.7 | DNPMD | 40th Percentile Industry days to MDUFA I... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DNPMD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "84"
)
# Table 1.7 | DNPMD | 40th Percentile Industry days to MDUFA I... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DNPMD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "32"
)
# Table 1.4 | DOD | 20th Percentile FDA days to Substantive ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DOD" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2013")],
  "86"
)
# Table 1.4 | DOD | 20th Percentile FDA days to Substantive ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DOD" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2014")],
  "89"
)
# Table 1.4 | DOD | 20th Percentile FDA days to Substantive ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DOD" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "88"
)
# Table 1.4 | DOD | 20th Percentile FDA days to Substantive ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DOD" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2016")],
  "88"
)
# Table 1.4 | DOD | 20th Percentile FDA days to Substantive ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DOD" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "87"
)
# Table 1.6 | DOD | Number of PMAs filed... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DOD" &
                   data$performance_metric == "Number of PMAs filed" &
                   data$fy == "2013")],
  "1"
)
# Table 1.6 | DOD | Number of PMAs filed... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DOD" &
                   data$performance_metric == "Number of PMAs filed" &
                   data$fy == "2014")],
  "2"
)
# Table 1.6 | DOD | Number of PMAs filed... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DOD" &
                   data$performance_metric == "Number of PMAs filed" &
                   data$fy == "2015")],
  "1"
)
# Table 1.6 | DOD | Number of PMAs filed... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DOD" &
                   data$performance_metric == "Number of PMAs filed" &
                   data$fy == "2016")],
  "0"
)
# Table 1.6 | DOD | Number of PMAs filed... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DOD" &
                   data$performance_metric == "Number of PMAs filed" &
                   data$fy == "2017")],
  "1"
)
# Table 1.8 | DOD | 40th Percentile Total days to MDUFA III ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DOD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2013")],
  "405"
)
# Table 1.8 | DOD | 40th Percentile Total days to MDUFA III ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DOD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2014")],
  "543"
)
# Table 1.8 | DOD | 40th Percentile Total days to MDUFA III ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DOD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2015")],
  "427"
)
# Table 1.8 | DOD | 40th Percentile Total days to MDUFA III ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DOD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2016")],
  "0"
)
# Table 1.8 | DOD | 40th Percentile Total days to MDUFA III ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DOD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2017")],
  "469"
)
# Table 1.10 | DOED | Number Filed... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DOED" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2013")],
  "4"
)
# Table 1.10 | DOED | Number Filed... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DOED" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2014")],
  "0"
)
# Table 1.10 | DOED | Number Filed... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DOED" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2015")],
  "0"
)
# Table 1.10 | DOED | Number Filed... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DOED" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2016")],
  "0"
)
# Table 1.10 | DOED | Number Filed... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DOED" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2017")],
  "0"
)
# Table 1.4 | DRGUD | Average number of FDA days to Substantiv... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2013")],
  "88.50"
)
# Table 1.4 | DRGUD | Average number of FDA days to Substantiv... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2014")],
  "97.80"
)
# Table 1.4 | DRGUD | Average number of FDA days to Substantiv... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "122.00"
)
# Table 1.4 | DRGUD | Average number of FDA days to Substantiv... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2016")],
  "88.50"
)
# Table 1.4 | DRGUD | Average number of FDA days to Substantiv... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "88.00"
)
# Table 1.11 | DRGUD | Mean FDA days for submissions that misse... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.11" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean FDA days for submissions that missed goal" &
                   data$fy == "2013")],
  "0"
)
# Table 1.11 | DRGUD | Mean FDA days for submissions that misse... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.11" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean FDA days for submissions that missed goal" &
                   data$fy == "2014")],
  "0"
)
# Table 1.11 | DRGUD | Mean FDA days for submissions that misse... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.11" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean FDA days for submissions that missed goal" &
                   data$fy == "2015")],
  "0"
)
# Table 1.11 | DRGUD | Mean FDA days for submissions that misse... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.11" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean FDA days for submissions that missed goal" &
                   data$fy == "2016")],
  "0"
)
# Table 1.11 | DRGUD | Mean FDA days for submissions that misse... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.11" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean FDA days for submissions that missed goal" &
                   data$fy == "2017")],
  "0"
)
# Table 1.6 | DSD | MDUFA III Decisions... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DSD" &
                   data$performance_metric == "MDUFA III Decisions" &
                   data$fy == "2013")],
  "0"
)
# Table 1.6 | DSD | MDUFA III Decisions... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DSD" &
                   data$performance_metric == "MDUFA III Decisions" &
                   data$fy == "2014")],
  "2"
)
# Table 1.6 | DSD | MDUFA III Decisions... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DSD" &
                   data$performance_metric == "MDUFA III Decisions" &
                   data$fy == "2015")],
  "0"
)
# Table 1.6 | DSD | MDUFA III Decisions... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DSD" &
                   data$performance_metric == "MDUFA III Decisions" &
                   data$fy == "2016")],
  "0"
)
# Table 1.6 | DSD | MDUFA III Decisions... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DSD" &
                   data$performance_metric == "MDUFA III Decisions" &
                   data$fy == "2017")],
  "0"
)
# Table 1.10 | DSD | Number Filed... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2013")],
  "0"
)
# Table 1.10 | DSD | Number Filed... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2014")],
  "2"
)
# Table 1.10 | DSD | Number Filed... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2015")],
  "0"
)
# Table 1.10 | DSD | Number Filed... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2016")],
  "0"
)
# Table 1.10 | DSD | Number Filed... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.10" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number Filed" &
                   data$fy == "2017")],
  "0"
)
# Table 1.5 | DCTD | Current Performance Percent Goal Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2013")],
  "100%"
)
# Table 1.5 | DCTD | Current Performance Percent Goal Met... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2014")],
  "100%"
)
# Table 1.5 | DCTD | Current Performance Percent Goal Met... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2015")],
  "100%"
)
# Table 1.5 | DCTD | Current Performance Percent Goal Met... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2016")],
  "100%"
)
# Table 1.5 | DCTD | Current Performance Percent Goal Met... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2017")],
  "100%"
)
# Table 1.4 | DIHD | Maximum FDA days to Substantive Interact... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DIHD" &
                   data$performance_metric == "Maximum FDA days to Substantive Interaction" &
                   data$fy == "2013")],
  "0"
)
# Table 1.4 | DIHD | Maximum FDA days to Substantive Interact... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DIHD" &
                   data$performance_metric == "Maximum FDA days to Substantive Interaction" &
                   data$fy == "2014")],
  "0"
)
# Table 1.4 | DIHD | Maximum FDA days to Substantive Interact... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DIHD" &
                   data$performance_metric == "Maximum FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "0"
)
# Table 1.4 | DIHD | Maximum FDA days to Substantive Interact... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DIHD" &
                   data$performance_metric == "Maximum FDA days to Substantive Interaction" &
                   data$fy == "2016")],
  "0"
)
# Table 1.4 | DIHD | Maximum FDA days to Substantive Interact... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.4" &
                   data$organization == "DIHD" &
                   data$performance_metric == "Maximum FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "0"
)
# Table 1.6 | DMD | Current Performance Percent Goal Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DMD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2013")],
  "100%"
)
# Table 1.6 | DMD | Current Performance Percent Goal Met... | FY 2014 = NA
expect_true(is.na(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DMD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2014")]
))
# Table 1.6 | DMD | Current Performance Percent Goal Met... | FY 2015 = NA
expect_true(is.na(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DMD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2015")]
))
# Table 1.6 | DMD | Current Performance Percent Goal Met... | FY 2016 = NA
expect_true(is.na(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DMD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2016")]
))
# Table 1.6 | DMD | Current Performance Percent Goal Met... | FY 2017 = NA
expect_true(is.na(
  data$value[which(data$table_number == "1.6" &
                   data$organization == "DMD" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2017")]
))
# Table 1.7 | DMGP | Number with MDUFA III decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2013")],
  "2"
)
# Table 1.7 | DMGP | Number with MDUFA III decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2014")],
  "1"
)
# Table 1.7 | DMGP | Number with MDUFA III decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2015")],
  "6"
)
# Table 1.7 | DMGP | Number with MDUFA III decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2016")],
  "10"
)
# Table 1.7 | DMGP | Number with MDUFA III decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2017")],
  "10"
)
# Table 1.8 | DRH | 40th Percentile Industry days to MDUFA I... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DRH" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "0"
)
# Table 1.8 | DRH | 40th Percentile Industry days to MDUFA I... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DRH" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "0"
)
# Table 1.8 | DRH | 40th Percentile Industry days to MDUFA I... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DRH" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "0"
)
# Table 1.8 | DRH | 40th Percentile Industry days to MDUFA I... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DRH" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "0"
)
# Table 1.8 | DRH | 40th Percentile Industry days to MDUFA I... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.8" &
                   data$organization == "DRH" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "0"
)
# Table 2.4 | OIR | Number of submissions that missed the go... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2013")],
  "0"
)
# Table 2.4 | OIR | Number of submissions that missed the go... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2014")],
  "0"
)
# Table 2.4 | OIR | Number of submissions that missed the go... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2015")],
  "0"
)
# Table 2.4 | OIR | Number of submissions that missed the go... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2016")],
  "0"
)
# Table 2.4 | OIR | Number of submissions that missed the go... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2017")],
  "1"
)
# Table 2.1 | DAGRID | SI Goal Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2013")],
  "8"
)
# Table 2.1 | DAGRID | SI Goal Met... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2014")],
  "7"
)
# Table 2.1 | DAGRID | SI Goal Met... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2015")],
  "6"
)
# Table 2.1 | DAGRID | SI Goal Met... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2016")],
  "5"
)
# Table 2.1 | DAGRID | SI Goal Met... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2017")],
  "7"
)
# Table 2.2 | DOD | Supplements pending MDUFA III Decision P... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOD" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2013")],
  "0"
)
# Table 2.2 | DOD | Supplements pending MDUFA III Decision P... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOD" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2014")],
  "0"
)
# Table 2.2 | DOD | Supplements pending MDUFA III Decision P... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOD" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2015")],
  "0"
)
# Table 2.2 | DOD | Supplements pending MDUFA III Decision P... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOD" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2016")],
  "0"
)
# Table 2.2 | DOD | Supplements pending MDUFA III Decision P... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOD" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2017")],
  "0"
)
# Table 2.2 | DOED | Supplements pending MDUFA III Decision P... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOED" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2013")],
  "0"
)
# Table 2.2 | DOED | Supplements pending MDUFA III Decision P... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOED" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2014")],
  "0"
)
# Table 2.2 | DOED | Supplements pending MDUFA III Decision P... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOED" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2015")],
  "0"
)
# Table 2.2 | DOED | Supplements pending MDUFA III Decision P... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOED" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2016")],
  "0"
)
# Table 2.2 | DOED | Supplements pending MDUFA III Decision P... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.2" &
                   data$organization == "DOED" &
                   data$performance_metric == "Supplements pending MDUFA III Decision Past Goal" &
                   data$fy == "2017")],
  "0"
)
# Table 2.3 | DRGUD | Number Received... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2013")],
  "10"
)
# Table 2.3 | DRGUD | Number Received... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2014")],
  "7"
)
# Table 2.3 | DRGUD | Number Received... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2015")],
  "8"
)
# Table 2.3 | DRGUD | Number Received... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2016")],
  "11"
)
# Table 2.3 | DRGUD | Number Received... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Number Received" &
                   data$fy == "2017")],
  "8"
)
# Table 2.1 | DCTD | SI Goal Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DCTD" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2013")],
  "3"
)
# Table 2.1 | DCTD | SI Goal Met... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DCTD" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2014")],
  "3"
)
# Table 2.1 | DCTD | SI Goal Met... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DCTD" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2015")],
  "3"
)
# Table 2.1 | DCTD | SI Goal Met... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DCTD" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2016")],
  "3"
)
# Table 2.1 | DCTD | SI Goal Met... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DCTD" &
                   data$performance_metric == "SI Goal Met" &
                   data$fy == "2017")],
  "6"
)
# Table 2.1 | DMGP | SI Goal Not Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DMGP" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2013")],
  "0"
)
# Table 2.1 | DMGP | SI Goal Not Met... | FY 2014
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DMGP" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2014")],
  "0"
)
# Table 2.1 | DMGP | SI Goal Not Met... | FY 2015
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DMGP" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2015")],
  "0"
)
# Table 2.1 | DMGP | SI Goal Not Met... | FY 2016
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DMGP" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2016")],
  "0"
)
# Table 2.1 | DMGP | SI Goal Not Met... | FY 2017
expect_equal(
  data$value[which(data$table_number == "2.1" &
                   data$organization == "DMGP" &
                   data$performance_metric == "SI Goal Not Met" &
                   data$fy == "2017")],
  "0"
)
# Table 3.2 | CDRH | Rate of Not Approvable... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2013")],
  "6.71%"
)
# Table 3.2 | CDRH | Rate of Not Approvable... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2014")],
  "0.91%"
)
# Table 3.2 | CDRH | Rate of Not Approvable... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2015")],
  "6.25%"
)
# Table 3.2 | CDRH | Rate of Not Approvable... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2016")],
  "1.87%"
)
# Table 3.2 | CDRH | Rate of Not Approvable... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2017")],
  "3.36%"
)
# Table 3.1 | ODE | Current Performance Percent Goal Met... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2013")],
  "99.23%"
)
# Table 3.1 | ODE | Current Performance Percent Goal Met... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2014")],
  "98.56%"
)
# Table 3.1 | ODE | Current Performance Percent Goal Met... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2015")],
  "98.14%"
)
# Table 3.1 | ODE | Current Performance Percent Goal Met... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2016")],
  "99.64%"
)
# Table 3.1 | ODE | Current Performance Percent Goal Met... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "Current Performance Percent Goal Met" &
                   data$fy == "2017")],
  "98.92%"
)
# Table 3.3 | DRGUD | Mean Industry days for submissions that ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean Industry days for submissions that missed goal" &
                   data$fy == "2013")],
  "0"
)
# Table 3.3 | DRGUD | Mean Industry days for submissions that ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean Industry days for submissions that missed goal" &
                   data$fy == "2014")],
  "0"
)
# Table 3.3 | DRGUD | Mean Industry days for submissions that ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean Industry days for submissions that missed goal" &
                   data$fy == "2015")],
  "0"
)
# Table 3.3 | DRGUD | Mean Industry days for submissions that ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean Industry days for submissions that missed goal" &
                   data$fy == "2016")],
  "0"
)
# Table 3.3 | DRGUD | Mean Industry days for submissions that ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DRGUD" &
                   data$performance_metric == "Mean Industry days for submissions that missed goal" &
                   data$fy == "2017")],
  "0"
)
# Table 3.2 | DSD | Number with MDUFA decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number with MDUFA decision" &
                   data$fy == "2013")],
  "13"
)
# Table 3.2 | DSD | Number with MDUFA decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number with MDUFA decision" &
                   data$fy == "2014")],
  "11"
)
# Table 3.2 | DSD | Number with MDUFA decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number with MDUFA decision" &
                   data$fy == "2015")],
  "7"
)
# Table 3.2 | DSD | Number with MDUFA decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number with MDUFA decision" &
                   data$fy == "2016")],
  "9"
)
# Table 3.2 | DSD | Number with MDUFA decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DSD" &
                   data$performance_metric == "Number with MDUFA decision" &
                   data$fy == "2017")],
  "10"
)
# Table 3.2 | DMD | Rate of Not Approvable... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMD" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2013")],
  "23.08%"
)
# Table 3.2 | DMD | Rate of Not Approvable... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMD" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2014")],
  "0%"
)
# Table 3.2 | DMD | Rate of Not Approvable... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMD" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2015")],
  "0%"
)
# Table 3.2 | DMD | Rate of Not Approvable... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMD" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2016")],
  "11.11%"
)
# Table 3.2 | DMD | Rate of Not Approvable... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMD" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2017")],
  "0%"
)
# Table 3.3 | DMD | Number of submissions that missed the go... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DMD" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2013")],
  "0"
)
# Table 3.3 | DMD | Number of submissions that missed the go... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DMD" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2014")],
  "0"
)
# Table 3.3 | DMD | Number of submissions that missed the go... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DMD" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2015")],
  "0"
)
# Table 3.3 | DMD | Number of submissions that missed the go... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DMD" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2016")],
  "0"
)
# Table 3.3 | DMD | Number of submissions that missed the go... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.3" &
                   data$organization == "DMD" &
                   data$performance_metric == "Number of submissions that missed the goal" &
                   data$fy == "2017")],
  "0"
)
# Table 3.2 | DMGP | Rate of Not Approvable... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2013")],
  "0%"
)
# Table 3.2 | DMGP | Rate of Not Approvable... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2014")],
  "0%"
)
# Table 3.2 | DMGP | Rate of Not Approvable... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2015")],
  "16.67%"
)
# Table 3.2 | DMGP | Rate of Not Approvable... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2016")],
  "0%"
)
# Table 3.2 | DMGP | Rate of Not Approvable... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.2" &
                   data$organization == "DMGP" &
                   data$performance_metric == "Rate of Not Approvable" &
                   data$fy == "2017")],
  "0%"
)
# Table 3.1 | DRH | Non-MDUFA III Decisions... | FY 2013
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Non-MDUFA III Decisions" &
                   data$fy == "2013")],
  "0"
)
# Table 3.1 | DRH | Non-MDUFA III Decisions... | FY 2014
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Non-MDUFA III Decisions" &
                   data$fy == "2014")],
  "0"
)
# Table 3.1 | DRH | Non-MDUFA III Decisions... | FY 2015
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Non-MDUFA III Decisions" &
                   data$fy == "2015")],
  "1"
)
# Table 3.1 | DRH | Non-MDUFA III Decisions... | FY 2016
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Non-MDUFA III Decisions" &
                   data$fy == "2016")],
  "0"
)
# Table 3.1 | DRH | Non-MDUFA III Decisions... | FY 2017
expect_equal(
  data$value[which(data$table_number == "3.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Non-MDUFA III Decisions" &
                   data$fy == "2017")],
  "1"
)
# Table 5.2 | CDRH | Number with a decision (MDUFA or Non- MD... | FY 2013
expect_equal(
  data$value[which(data$table_number == "5.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with a decision (MDUFA or Non- MDUFA)" &
                   data$fy == "2013")],
  "44"
)
# Table 5.2 | CDRH | Number with a decision (MDUFA or Non- MD... | FY 2014
expect_equal(
  data$value[which(data$table_number == "5.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with a decision (MDUFA or Non- MDUFA)" &
                   data$fy == "2014")],
  "42"
)
# Table 5.2 | CDRH | Number with a decision (MDUFA or Non- MD... | FY 2015
expect_equal(
  data$value[which(data$table_number == "5.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with a decision (MDUFA or Non- MDUFA)" &
                   data$fy == "2015")],
  "68"
)
# Table 5.2 | CDRH | Number with a decision (MDUFA or Non- MD... | FY 2016
expect_equal(
  data$value[which(data$table_number == "5.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with a decision (MDUFA or Non- MDUFA)" &
                   data$fy == "2016")],
  "69"
)
# Table 5.2 | CDRH | Number with a decision (MDUFA or Non- MD... | FY 2017
expect_equal(
  data$value[which(data$table_number == "5.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number with a decision (MDUFA or Non- MDUFA)" &
                   data$fy == "2017")],
  "60"
)
# Table 6.5 | CDRH | Average review cycles... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average review cycles" &
                   data$fy == "2013")],
  "1.74"
)
# Table 6.5 | CDRH | Average review cycles... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average review cycles" &
                   data$fy == "2014")],
  "1.72"
)
# Table 6.5 | CDRH | Average review cycles... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average review cycles" &
                   data$fy == "2015")],
  "1.73"
)
# Table 6.5 | CDRH | Average review cycles... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average review cycles" &
                   data$fy == "2016")],
  "1.80"
)
# Table 6.5 | CDRH | Average review cycles... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average review cycles" &
                   data$fy == "2017")],
  "1.70"
)
# Table 6.5 | ODE | 80th Percentile Industry days to MDUFA I... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "116"
)
# Table 6.5 | ODE | 80th Percentile Industry days to MDUFA I... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "118"
)
# Table 6.5 | ODE | 80th Percentile Industry days to MDUFA I... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "140"
)
# Table 6.5 | ODE | 80th Percentile Industry days to MDUFA I... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "147"
)
# Table 6.5 | ODE | 80th Percentile Industry days to MDUFA I... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "ODE" &
                   data$performance_metric == "80th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "144"
)
# Table 6.4 | OIR | 510(k)s pending MDUFA III Decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "510(k)s pending MDUFA III Decision" &
                   data$fy == "2013")],
  "0"
)
# Table 6.4 | OIR | 510(k)s pending MDUFA III Decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "510(k)s pending MDUFA III Decision" &
                   data$fy == "2014")],
  "0"
)
# Table 6.4 | OIR | 510(k)s pending MDUFA III Decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "510(k)s pending MDUFA III Decision" &
                   data$fy == "2015")],
  "0"
)
# Table 6.4 | OIR | 510(k)s pending MDUFA III Decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "510(k)s pending MDUFA III Decision" &
                   data$fy == "2016")],
  "0"
)
# Table 6.4 | OIR | 510(k)s pending MDUFA III Decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.4" &
                   data$organization == "OIR" &
                   data$performance_metric == "510(k)s pending MDUFA III Decision" &
                   data$fy == "2017")],
  "9"
)
# Table 6.5 | OIR | 80th Percentile Total days to MDUFA III ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OIR" &
                   data$performance_metric == "80th Percentile Total days to MDUFA III decision" &
                   data$fy == "2013")],
  "194"
)
# Table 6.5 | OIR | 80th Percentile Total days to MDUFA III ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OIR" &
                   data$performance_metric == "80th Percentile Total days to MDUFA III decision" &
                   data$fy == "2014")],
  "189"
)
# Table 6.5 | OIR | 80th Percentile Total days to MDUFA III ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OIR" &
                   data$performance_metric == "80th Percentile Total days to MDUFA III decision" &
                   data$fy == "2015")],
  "207"
)
# Table 6.5 | OIR | 80th Percentile Total days to MDUFA III ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OIR" &
                   data$performance_metric == "80th Percentile Total days to MDUFA III decision" &
                   data$fy == "2016")],
  "212"
)
# Table 6.5 | OIR | 80th Percentile Total days to MDUFA III ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "OIR" &
                   data$performance_metric == "80th Percentile Total days to MDUFA III decision" &
                   data$fy == "2017")],
  "218"
)
# Table 6.5 | DAGRID | 60th Percentile Total days to MDUFA III ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "60th Percentile Total days to MDUFA III decision" &
                   data$fy == "2013")],
  "140"
)
# Table 6.5 | DAGRID | 60th Percentile Total days to MDUFA III ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "60th Percentile Total days to MDUFA III decision" &
                   data$fy == "2014")],
  "157"
)
# Table 6.5 | DAGRID | 60th Percentile Total days to MDUFA III ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "60th Percentile Total days to MDUFA III decision" &
                   data$fy == "2015")],
  "178"
)
# Table 6.5 | DAGRID | 60th Percentile Total days to MDUFA III ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "60th Percentile Total days to MDUFA III decision" &
                   data$fy == "2016")],
  "183"
)
# Table 6.5 | DAGRID | 60th Percentile Total days to MDUFA III ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DAGRID" &
                   data$performance_metric == "60th Percentile Total days to MDUFA III decision" &
                   data$fy == "2017")],
  "183"
)
# Table 6.2 | DCD | Current SI Performance Percent within 60... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "DCD" &
                   data$performance_metric == "Current SI Performance Percent within 60 FDA days" &
                   data$fy == "2013")],
  "91.32%"
)
# Table 6.2 | DCD | Current SI Performance Percent within 60... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "DCD" &
                   data$performance_metric == "Current SI Performance Percent within 60 FDA days" &
                   data$fy == "2014")],
  "95.26%"
)
# Table 6.2 | DCD | Current SI Performance Percent within 60... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "DCD" &
                   data$performance_metric == "Current SI Performance Percent within 60 FDA days" &
                   data$fy == "2015")],
  "96.01%"
)
# Table 6.2 | DCD | Current SI Performance Percent within 60... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "DCD" &
                   data$performance_metric == "Current SI Performance Percent within 60 FDA days" &
                   data$fy == "2016")],
  "94.83%"
)
# Table 6.2 | DCD | Current SI Performance Percent within 60... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.2" &
                   data$organization == "DCD" &
                   data$performance_metric == "Current SI Performance Percent within 60 FDA days" &
                   data$fy == "2017")],
  "97.53%"
)
# Table 6.5 | DCD | 40th Percentile Industry days to MDUFA I... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "14"
)
# Table 6.5 | DCD | 40th Percentile Industry days to MDUFA I... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "10"
)
# Table 6.5 | DCD | 40th Percentile Industry days to MDUFA I... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "13"
)
# Table 6.5 | DCD | 40th Percentile Industry days to MDUFA I... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "24"
)
# Table 6.5 | DCD | 40th Percentile Industry days to MDUFA I... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "40th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "22"
)
# Table 6.5 | DCD | Average Total days to MDUFA III decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "Average Total days to MDUFA III decision" &
                   data$fy == "2013")],
  "116.54"
)
# Table 6.5 | DCD | Average Total days to MDUFA III decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "Average Total days to MDUFA III decision" &
                   data$fy == "2014")],
  "117.76"
)
# Table 6.5 | DCD | Average Total days to MDUFA III decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "Average Total days to MDUFA III decision" &
                   data$fy == "2015")],
  "118.68"
)
# Table 6.5 | DCD | Average Total days to MDUFA III decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "Average Total days to MDUFA III decision" &
                   data$fy == "2016")],
  "141.02"
)
# Table 6.5 | DCD | Average Total days to MDUFA III decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCD" &
                   data$performance_metric == "Average Total days to MDUFA III decision" &
                   data$fy == "2017")],
  "138.42"
)
# Table 6.5 | DOED | Maximum Industry days to MDUFA III decis... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DOED" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "250"
)
# Table 6.5 | DOED | Maximum Industry days to MDUFA III decis... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DOED" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "333"
)
# Table 6.5 | DOED | Maximum Industry days to MDUFA III decis... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DOED" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "180"
)
# Table 6.5 | DOED | Maximum Industry days to MDUFA III decis... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DOED" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "189"
)
# Table 6.5 | DOED | Maximum Industry days to MDUFA III decis... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DOED" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "181"
)
# Table 6.1 | DSD | Rate of submissions not accepted for fil... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "DSD" &
                   data$performance_metric == "Rate of submissions not accepted for filing review" &
                   data$fy == "2013")],
  "62.31%"
)
# Table 6.1 | DSD | Rate of submissions not accepted for fil... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "DSD" &
                   data$performance_metric == "Rate of submissions not accepted for filing review" &
                   data$fy == "2014")],
  "54.97%"
)
# Table 6.1 | DSD | Rate of submissions not accepted for fil... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "DSD" &
                   data$performance_metric == "Rate of submissions not accepted for filing review" &
                   data$fy == "2015")],
  "37.63%"
)
# Table 6.1 | DSD | Rate of submissions not accepted for fil... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "DSD" &
                   data$performance_metric == "Rate of submissions not accepted for filing review" &
                   data$fy == "2016")],
  "24.00%"
)
# Table 6.1 | DSD | Rate of submissions not accepted for fil... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.1" &
                   data$organization == "DSD" &
                   data$performance_metric == "Rate of submissions not accepted for filing review" &
                   data$fy == "2017")],
  "28.26%"
)
# Table 6.5 | DSD | Maximum Industry days to MDUFA III decis... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DSD" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "438"
)
# Table 6.5 | DSD | Maximum Industry days to MDUFA III decis... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DSD" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "408"
)
# Table 6.5 | DSD | Maximum Industry days to MDUFA III decis... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DSD" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "342"
)
# Table 6.5 | DSD | Maximum Industry days to MDUFA III decis... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DSD" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "354"
)
# Table 6.5 | DSD | Maximum Industry days to MDUFA III decis... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DSD" &
                   data$performance_metric == "Maximum Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "182"
)
# Table 6.5 | DCTD | 40th Percentile Total days to MDUFA III ... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2013")],
  "87"
)
# Table 6.5 | DCTD | 40th Percentile Total days to MDUFA III ... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2014")],
  "84"
)
# Table 6.5 | DCTD | 40th Percentile Total days to MDUFA III ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2015")],
  "85"
)
# Table 6.5 | DCTD | 40th Percentile Total days to MDUFA III ... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2016")],
  "143"
)
# Table 6.5 | DCTD | 40th Percentile Total days to MDUFA III ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "DCTD" &
                   data$performance_metric == "40th Percentile Total days to MDUFA III decision" &
                   data$fy == "2017")],
  "141"
)
# Table 8.1 | CDRH | Number of De Novo Requests with Decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of De Novo Requests with Decision Pending" &
                   data$fy == "2013")],
  "0"
)
# Table 8.1 | CDRH | Number of De Novo Requests with Decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of De Novo Requests with Decision Pending" &
                   data$fy == "2014")],
  "0"
)
# Table 8.1 | CDRH | Number of De Novo Requests with Decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of De Novo Requests with Decision Pending" &
                   data$fy == "2015")],
  "0"
)
# Table 8.1 | CDRH | Number of De Novo Requests with Decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of De Novo Requests with Decision Pending" &
                   data$fy == "2016")],
  "0"
)
# Table 8.1 | CDRH | Number of De Novo Requests with Decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Number of De Novo Requests with Decision Pending" &
                   data$fy == "2017")],
  "11"
)
# Table 9.1 | ODE | 20th Percentile days to meeting... | FY 2013
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "20th Percentile days to meeting" &
                   data$fy == "2013")],
  "40"
)
# Table 9.1 | ODE | 20th Percentile days to meeting... | FY 2014
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "20th Percentile days to meeting" &
                   data$fy == "2014")],
  "49"
)
# Table 9.1 | ODE | 20th Percentile days to meeting... | FY 2015
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "20th Percentile days to meeting" &
                   data$fy == "2015")],
  "52"
)
# Table 9.1 | ODE | 20th Percentile days to meeting... | FY 2016
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "20th Percentile days to meeting" &
                   data$fy == "2016")],
  "54"
)
# Table 9.1 | ODE | 20th Percentile days to meeting... | FY 2017
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "ODE" &
                   data$performance_metric == "20th Percentile days to meeting" &
                   data$fy == "2017")],
  "56"
)
# Table 9.1 | OIR | Maximum days to meeting... | FY 2013
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OIR" &
                   data$performance_metric == "Maximum days to meeting" &
                   data$fy == "2013")],
  "138"
)
# Table 9.1 | OIR | Maximum days to meeting... | FY 2014
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OIR" &
                   data$performance_metric == "Maximum days to meeting" &
                   data$fy == "2014")],
  "115"
)
# Table 9.1 | OIR | Maximum days to meeting... | FY 2015
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OIR" &
                   data$performance_metric == "Maximum days to meeting" &
                   data$fy == "2015")],
  "112"
)
# Table 9.1 | OIR | Maximum days to meeting... | FY 2016
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OIR" &
                   data$performance_metric == "Maximum days to meeting" &
                   data$fy == "2016")],
  "203"
)
# Table 9.1 | OIR | Maximum days to meeting... | FY 2017
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "OIR" &
                   data$performance_metric == "Maximum days to meeting" &
                   data$fy == "2017")],
  "146"
)
# Table 9.1 | DOD | 60th Percentile days to meeting... | FY 2013
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DOD" &
                   data$performance_metric == "60th Percentile days to meeting" &
                   data$fy == "2013")],
  "68"
)
# Table 9.1 | DOD | 60th Percentile days to meeting... | FY 2014
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DOD" &
                   data$performance_metric == "60th Percentile days to meeting" &
                   data$fy == "2014")],
  "77"
)
# Table 9.1 | DOD | 60th Percentile days to meeting... | FY 2015
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DOD" &
                   data$performance_metric == "60th Percentile days to meeting" &
                   data$fy == "2015")],
  "77"
)
# Table 9.1 | DOD | 60th Percentile days to meeting... | FY 2016
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DOD" &
                   data$performance_metric == "60th Percentile days to meeting" &
                   data$fy == "2016")],
  "75"
)
# Table 9.1 | DOD | 60th Percentile days to meeting... | FY 2017
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DOD" &
                   data$performance_metric == "60th Percentile days to meeting" &
                   data$fy == "2017")],
  "73"
)
# Table 9.1 | DRH | Number requesting a meeting or teleconfe... | FY 2013
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Number requesting a meeting or teleconference" &
                   data$fy == "2013")],
  "45"
)
# Table 9.1 | DRH | Number requesting a meeting or teleconfe... | FY 2014
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Number requesting a meeting or teleconference" &
                   data$fy == "2014")],
  "37"
)
# Table 9.1 | DRH | Number requesting a meeting or teleconfe... | FY 2015
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Number requesting a meeting or teleconference" &
                   data$fy == "2015")],
  "50"
)
# Table 9.1 | DRH | Number requesting a meeting or teleconfe... | FY 2016
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Number requesting a meeting or teleconference" &
                   data$fy == "2016")],
  "57"
)
# Table 9.1 | DRH | Number requesting a meeting or teleconfe... | FY 2017
expect_equal(
  data$value[which(data$table_number == "9.1" &
                   data$organization == "DRH" &
                   data$performance_metric == "Number requesting a meeting or teleconference" &
                   data$fy == "2017")],
  "58"
)
# Table 12.2 | CDRH | Average number of FDA days to Substantiv... | FY 2013 = NA
expect_true(is.na(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2013")]
))
# Table 12.2 | CDRH | Average number of FDA days to Substantiv... | FY 2014
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2014")],
  "87.0"
)
# Table 12.2 | CDRH | Average number of FDA days to Substantiv... | FY 2015
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "87.0"
)
# Table 12.2 | CDRH | Average number of FDA days to Substantiv... | FY 2016
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2016")],
  "77.0"
)
# Table 12.2 | CDRH | Average number of FDA days to Substantiv... | FY 2017
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "Average number of FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "84.3"
)
# Table 12.2 | CDRH | 20th Percentile FDA days to Substantive ... | FY 2013 = NA
expect_true(is.na(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2013")]
))
# Table 12.2 | CDRH | 20th Percentile FDA days to Substantive ... | FY 2014 = NA
expect_true(is.na(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2014")]
))
# Table 12.2 | CDRH | 20th Percentile FDA days to Substantive ... | FY 2015
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2015")],
  "86"
)
# Table 12.2 | CDRH | 20th Percentile FDA days to Substantive ... | FY 2016 = NA
expect_true(is.na(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2016")]
))
# Table 12.2 | CDRH | 20th Percentile FDA days to Substantive ... | FY 2017
expect_equal(
  data$value[which(data$table_number == "12.2" &
                   data$organization == "CDRH" &
                   data$performance_metric == "20th Percentile FDA days to Substantive Interaction" &
                   data$fy == "2017")],
  "84"
)
# Table 1.7 | CBER | Number with MDUFA III decision... | FY 2013
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2013")],
  "1"
)
# Table 1.7 | CBER | Number with MDUFA III decision... | FY 2014
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2014")],
  "0"
)
# Table 1.7 | CBER | Number with MDUFA III decision... | FY 2015
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2015")],
  "0"
)
# Table 1.7 | CBER | Number with MDUFA III decision... | FY 2016
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2016")],
  "0"
)
# Table 1.7 | CBER | Number with MDUFA III decision... | FY 2017
expect_equal(
  data$value[which(data$table_number == "1.7" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number with MDUFA III decision" &
                   data$fy == "2017")],
  "0"
)
# Table 6.5 | CBER | 60th Percentile Industry days to MDUFA I... | FY 2013
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "60th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2013")],
  "83"
)
# Table 6.5 | CBER | 60th Percentile Industry days to MDUFA I... | FY 2014
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "60th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2014")],
  "44"
)
# Table 6.5 | CBER | 60th Percentile Industry days to MDUFA I... | FY 2015
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "60th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2015")],
  "29"
)
# Table 6.5 | CBER | 60th Percentile Industry days to MDUFA I... | FY 2016
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "60th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2016")],
  "65"
)
# Table 6.5 | CBER | 60th Percentile Industry days to MDUFA I... | FY 2017
expect_equal(
  data$value[which(data$table_number == "6.5" &
                   data$organization == "CBER" &
                   data$performance_metric == "60th Percentile Industry days to MDUFA III decision" &
                   data$fy == "2017")],
  "24"
)
# Table 8.1 | CBER | Number of De Novo Petitions Received... | FY 2013
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number of De Novo Petitions Received" &
                   data$fy == "2013")],
  "2"
)
# Table 8.1 | CBER | Number of De Novo Petitions Received... | FY 2014
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number of De Novo Petitions Received" &
                   data$fy == "2014")],
  "0"
)
# Table 8.1 | CBER | Number of De Novo Petitions Received... | FY 2015
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number of De Novo Petitions Received" &
                   data$fy == "2015")],
  "1"
)
# Table 8.1 | CBER | Number of De Novo Petitions Received... | FY 2016
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number of De Novo Petitions Received" &
                   data$fy == "2016")],
  "0"
)
# Table 8.1 | CBER | Number of De Novo Petitions Received... | FY 2017
expect_equal(
  data$value[which(data$table_number == "8.1" &
                   data$organization == "CBER" &
                   data$performance_metric == "Number of De Novo Petitions Received" &
                   data$fy == "2017")],
  "2"
)})
# nolint end
