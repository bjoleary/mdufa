#' Filter Metrics
#'
#' Filter a MDUFA metrics table to one metric type, such as all metrics that are
#' a percent.
#'
#' @param data A MDUFA performance table, such as
#'   \code{mdufa::quarterly_performance}.
#' @param metric_type The metric type to filter the table to. Valid options
#'   include \code{"integer"}, \code{"percent"}, \code{"double"}, and
#'   \code{"text"}.
#'
#' @return A filtered performance table, with the \code{value} column in the
#'   proper format.
#' @export
#'
#' @examples
#' mdufa::mdufa4 %>%
#'   filter_metrics(metric_type = "percent")
filter_metrics <- function(data, metric_type) {
  stopifnot(
    metric_type %in% c("integer", "percent", "double", "text")
  )
  filter_to <- metric_type
  convert_data <- function(x, filter_to) {
    if (filter_to == "integer") {
      stringr::str_remove_all(
        string = x,
        pattern = "[^0-9.]"
      ) %>%
      as.integer()
    } else if (filter_to == "percent") {
      stringr::str_remove(
        string = x,
        pattern = "\\%$"
      ) %>%
        as.numeric() / 100
    } else if (filter_to == "double") {
      as.numeric(x)
    } else {
      x
    }
  }
  data %>%
    dplyr::filter(.data$metric_type == filter_to) %>%
    dplyr::mutate(value = convert_data(x = .data$value, filter_to = filter_to))
}
