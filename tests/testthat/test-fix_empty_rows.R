test_that("empty rows are properly removed", {
  testthat::skip("This one doesn't work yet and we know it.")
  test_data <-
    structure(
      list(
        Performance.Metric =
          c(
            "Average Review Cycles",
            "Number With MDUFA IV Decision",
            "Average Number of FDA Days to MDUFA IV",
            "Decision", "20th Percentile FDA Days to MDUFA IV", "Decision",
            "40th Percentile FDA Days to MDUFA IV", "Decision",
            "60th Percentile FDA Days to MDUFA IV",
            "Decision", "80th Percentile FDA Days to MDUFA IV", "Decision",
            "Maximum FDA Days to MDUFA IV Decision",
            "Average Number of Industry Days to MDUFA",
            "IV Decision", "20th Percentile Industry Days to MDUFA IV",
            "Decision",
            "40th Percentile Industry Days to MDUFA IV", "Decision",
            "60th Percentile Industry Days to MDUFA IV",
            "Decision", "80th Percentile Industry Days to MDUFA IV", "Decision",
            "Maximum Industry Days to MDUFA IV Decision",
            "Average Number of Total Days to MDUFA IV",
            "Decision", "20th Percentile Total Days to MDUFA IV", "Decision",
            "40th Percentile Total Days to MDUFA IV", "Decision",
            "60th Percentile Total Days to MDUFA IV",
            "Decision", "80th Percentile Total Days to MDUFA IV", "Decision",
            "Maximum Total Days to MDUFA IV Decision", ""
          ),
        FY.2018 =
          c(
            "2", "2,926", "", "72.62", "", "54", "", "79", "", "87", "", "89",
            "220", "", "54.69", "", "0", "", "5", "", "44", "", "127", "563",
            "", "127.31", "", "57", "", "89", "", "128", "", "212", "783", ""
          ),
        FY.2019 =
          c(
            "2", "3,023", "", "73.09", "", "55", "", "82", "", "88", "", "90",
            "207", "", "59.54", "", "0", "", "0", "", "49", "", "137", "444",
            "", "132.62", "", "57", "", "90", "", "132", "", "223", "543", ""
          ),
        FY.2020 =
          c(
            "2", "2,656", "", "71.26", "", "49", "", "79", "", "87", "", "89",
            "287", "", "49.16", "", "0", "", "0", "", "35", "", "103", "361",
            "", "120.42", "", "52", "", "88", "", "120", "", "191", "464", ""
          ),
        FY.2021 =
          c(
            1, 385, NA, 54.94, NA, 28, NA, 46, NA, 60, NA, 87, 132, NA, 6.25,
            NA, 0, NA, 0, NA, 0, NA, 5, 82, NA, 61.18, NA, 28, NA, 49, NA, 64,
            NA, 89, 170, NA
          ),
        FY.2022 =
          c(
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA
          )
      ),
      class = "data.frame",
      row.names = c(NA, -36L)
    ) |>
    tibble::as_tibble() |>
    janitor::clean_names()
  expected_output <-
    structure(
      list(
        performance_metric =
          c(
            "Average Review Cycles",
            "Number With MDUFA IV Decision",
            "Average Number of FDA Days to MDUFA IV Decision",
            "20th Percentile FDA Days to MDUFA IV Decision",
            "40th Percentile FDA Days to MDUFA IV Decision",
            "60th Percentile FDA Days to MDUFA IV Decision",
            "80th Percentile FDA Days to MDUFA IV Decision",
            "Maximum FDA Days to MDUFA IV Decision",
            "Average Number of Industry Days to MDUFA IV Decision",
            "20th Percentile Industry Days to MDUFA IV Decision",
            "40th Percentile Industry Days to MDUFA IV Decision",
            "60th Percentile Industry Days to MDUFA IV Decision",
            "80th Percentile Industry Days to MDUFA IV Decision",
            "Maximum Industry Days to MDUFA IV Decision",
            "Average Number of Total Days to MDUFA IV Decision",
            "20th Percentile Total Days to MDUFA IV Decision",
            "40th Percentile Total Days to MDUFA IV Decision",
            "60th Percentile Total Days to MDUFA IV Decision",
            "80th Percentile Total Days to MDUFA IV Decision",
            "Maximum Total Days to MDUFA IV Decision"
          ),
        fy_2018 =
          c(
            "2", "2,926", "72.62", "54", "79", "87", "89", "220", "54.69", "0",
            "5", "44", "127", "563", "127.31", "57", "89", "128", "212",
            "783"
          ),
        fy_2019 =
          c(
            "2", "3,023", "73.09", "55", "82", "88",
            "90", "207", "59.54", "0", "0", "49", "137", "444", "132.62",
            "57", "90", "132", "223", "543"
          ),
        fy_2020 =
          c(
            "2", "2,656", "71.26",
            "49", "79", "87", "89", "287", "49.16", "0", "0", "35", "103",
            "361", "120.42", "52", "88", "120", "191", "464"
          ),
        fy_2021 =
          c(
            1, 385, 54.94, 28, 46, 60, 87, 132, 6.25, 0, 0, 0, 5, 82, 61.18,
            28, 49, 64, 89, 170
          ),
        fy_2022 =
          c(
            NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          )
      ),
      row.names = c(NA, -20L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  expect_equal(
    expected_output,
    fix_empty_rows(test_data, name_column = "performance_metric")
  )
})
