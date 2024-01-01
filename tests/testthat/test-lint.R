if (requireNamespace("lintr", quietly = TRUE)) {
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    # skip("Fails on R CMD Check because of filepath issues. ")
    test_that("package lints", {
      lintr::expect_lint_free()
    })
  }
}
