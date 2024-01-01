#' Fix Empty Rows
#'
#' When converting a table from PDF, sometimes the \code{name_column} breaks
#' across multiple lines. Each of these lines can then appear as a separate
#' empty row in the resulting table. This function fixes that by checking for
#' empty rows and then concatenating the \code{name_column} value of the
#' following row with the \code{name_column} value of the previous row.
#'
#' @param data A tibble
#' @param name_column The name of the column that functions as the row names and
#' may break across multiple rows
#'
#' @return A compressed tibble without empty rows and with repaired names
#' @export
#'
fix_empty_rows <- function(data, name_column) {
  data %>%
    # Remove the name column
    dplyr::select(-!!name_column) %>%
    # Check if all columns other than the name column are na, return TRUE if
    # they are
    purrr::pmap(
      .f = ~ is.na(.x),
      .id = NULL
    )  %>%
    # Turn the resulting vector into a tibble with one field labeled "empty"
    unlist() %>%
    tibble::enframe(
      x = .,
      name = NULL,
      value = "empty"
    ) %>%
    # Bind the original data back in
    dplyr::bind_cols(
      data,
      .
    ) %>%
    # Some rows are missing their names because of line breaks in the PDF. Let's
    # fix that.
    dplyr::mutate(
      {{ name_column }} :=
        dplyr::case_when(
          # If it has a name, keep it.
          !(is.na(.data[[name_column]])) ~ .data[[name_column]],
          # This row has data and both the 2 rows above and the 2 rows below
          # are empty
            .data$empty == FALSE &
            dplyr::lag(.data$empty) == TRUE &
            dplyr::lead(.data$empty) == TRUE ~
              # Concatenate the names of the previous row and the following row
              paste(
                dplyr::lag(.data[[name_column]]),
                dplyr::lead(.data[[name_column]])
              ),
            # Otherwise, if only the previous row is empty
            .data$empty == FALSE &
              dplyr::lag(.data$empty) == TRUE ~
              paste(
                dplyr::lag(.data[[name_column]]),
                .data[[name_column]] %>% tidyr::replace_na("")
              ),
            # Otherwise, just use what you've go.
            TRUE ~ .data[[name_column]]
        )
    ) %>%
    # Remove empty rows
    dplyr::filter(.data$empty == FALSE) %>%
    # Remove helper "empty" column
    dplyr::select(-.data$empty)
}
