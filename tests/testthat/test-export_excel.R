test_that("export_excel works", {
  # TODO: Lots of improvements to make here, but the underlying function
  # needs enhancement first.
  suppressWarnings(
    expect_equal(
      object =
        export_excel(
          data = mdufa::mdufa4,
          filepath = tempfile(fileext = ".xlsx")
        ),
      expected = 1,
    )
  )
})

test_that("export_excel errors work", {
  expect_error(
    object =
      export_excel(
        data = mdufa::mdufa4,
        filepath = "not_an_xlsx_file.csv"
      )
  )
  expect_error(
    object =
      export_excel(
        data =
          mdufa::mdufa4 %>%
            dplyr::select(-"value"),
        filepath = tempfile(fileext = ".xlsx")
      )
  )
})
