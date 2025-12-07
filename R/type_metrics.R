#' Type Metrics
#'
#' Return a performance table with a list column for \code{value} so that
#' multiple metric types can be accommodated.
#'
#' @param data A performance table, such as \code{mdufa::quarterly_performance}.
#'
#' @return A performance table with a list column for \code{value}.
#' @export
#'
#' @examples
#' type_metrics(mdufa::mdufa4)
type_metrics <- function(data) {
  metrics <-
    data$metric_type |>
    unique()
  metrics[!is.na(metrics)] |>
    purrr::map_dfr(
      .f =
        ~ filter_metrics(data, .x) |>
          dplyr::mutate(value = as.list(.data$value))
    ) |>
    (\(x) {
      dplyr::bind_rows(
        x,
        data |>
          dplyr::filter(is.na(.data$metric_type)) |>
          dplyr::mutate(value = as.list(.data$value))
      )
    })()
}
