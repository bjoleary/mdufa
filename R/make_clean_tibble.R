#' Make a Tibble with Correct Names
#'
#' @param data A tibble
#'
#' @return A tibble with names that are harmonized with other tables from the
#'   report.
#' @export
#'
make_clean_tibble <- function(data) {
  name_replacer <- function(x) {
    stringr::str_replace(
      string = x,
      pattern = stringr::regex("performance_metrics|pma_submissions_received"),
      replacement = "performance_metric"
    )
  }
  data %>%
    janitor::clean_names() %>%
    dplyr::rename_with(name_replacer) %>%
    fix_empty_rows(
      data = .,
      name_column = colnames(.) %>% magrittr::extract(1)
    )
}
