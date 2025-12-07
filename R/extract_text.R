#' Extract Text from PDF Report
#'
#' @param url_report The URL of the report to read
#'
#' @return The embedded text of the report
#' @export
#'
extract_text <- function(url_report) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    raw_text <-
      pdftools::pdf_text(url_report) |>
      # Remove first 10 pages
      (\(x) x[-seq(1, 10, 1)])() |>
      # Make sure a table title is present
      (\(x) x[stringr::str_detect(x, pattern = table_title_text_pattern())])()

    # Remove everything in the appendices
    start_of_appendix <-
      stringr::str_which(
        string = raw_text,
        pattern = stringr::regex("^Appendix A")
      ) |>
      (\(x) x[[1]])()

    start_of_cber <-
      stringr::str_which(
        string = raw_text,
        pattern = stringr::regex(".*CBER.*")
      ) |>
      (\(x) x[[1]])()

    raw_text <- raw_text[-(start_of_appendix:start_of_cber)]
  } else {
    warning(
      "The \"pdftools\" package is required for this function. Please ",
      "install it and try again."
    )
  }
}
