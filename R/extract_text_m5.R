#' Extract Text from PDF MDUFA 5 Report
#'
#' These reports don't always have page numbers, complicating the procedure.
#'
#' @param url_report The URL of the report to read
#'
#' @return A tibble that includes page numbers and the embedded text of the
#'   report
#' @export
#'
extract_text_m5 <- function(url_report) {
  if (requireNamespace("pdftools", quietly = TRUE)) {
    raw_text <-
      pdftools::pdf_text(url_report) |>
      tibble::enframe(
        name = "page_number",
        value = "raw_text"
      ) |>
      dplyr::filter(
        .data$page_number > 10,
        stringr::str_detect(
          string = .data$raw_text,
          pattern = table_title_text_pattern()
        )
      )

    # Remove everything in the appendices
    start_of_appendix <-
      stringr::str_which(
        string = raw_text$raw_text,
        pattern = stringr::regex("^Appendix A")
      ) |>
      (\(x) x[[1]])()

    start_of_cber <-
      stringr::str_which(
        string = raw_text$raw_text,
        # Match actual CBER table pages, not "(CDRH+CBER)" metric names
        pattern = stringr::regex("Table.*CBER")
      ) |>
      (\(x) x[[1]])()

    raw_text <- raw_text[-(start_of_appendix:start_of_cber), ]
  } else {
    warning(
      "The \"pdftools\" package is required for this function. Please ",
      "install it and try again."
    )
  }
}
