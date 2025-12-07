#' Unnecessary Text Pattern
#'
#'
#' @return A regex pattern that defines unnecessary text that can be removed.
#' @export
#'
unnecessary_text_pattern <- function() {
  patterns <-
    c(
      # The word Section followed by digits, may or may not have a line break
      "(Section\\s\\d*.*\\v|(?<=\\v)\\*.*\\v)",
      # An asterisk followed by a warning about RTA implementation
      "(\\*\\sRTA.*\\v)"
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Goal Percent Line Pattern
#'
#' @return A regex pattern that matches to a line that includes
#'   \code{goal_percent_pattern()}.
#' @export
#'
goal_percent_line_pattern <- function() {
  patterns <-
    c(
      # A newline with the some words followed by 5 of the percentage patterns
      # with 0 or more spaces or pipes and followed by a newline.
      paste0(
        "\\v\\D*((",
        goal_percent_pattern(),
        ")(\\|{0,}|\\s{0,})){5}\\v"
      )
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for table header rows with FYs in them
#'
#' The pattern matches a line that includes the string "FY" followed by a space
#' and four digits, repeated five times.
#'
#' @return A regular expression pattern
fy_start_line_pattern <- function() {
  patterns <-
    c(
      # A line that includes 5 copies of FY XXXX, where X is a digit
      "((?<=\\v)\\s*(FY\\s\\d{4}\\s*){5}\\v)"
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for goal percentages
#'
#' This function generates a regular expression pattern for matching goal
#' percentages in text. The pattern matches a string that starts with a
#' percentage followed by one or more words, followed by two or more digits, not
#' followed by a percent. The pattern also matches a string that starts with a
#' percentage followed by one or more words.
#'
#' @return A regular expression pattern
goal_percent_pattern <- function() {
  patterns <-
    c(
      # A percentage followed by 1 or more words, followed by 2 or more digits,
      # not followed by a percent
      "(\\d{2}%(\\s[[:alpha:]]*\\b){1,}(\\d{2,}(?!%)))",
      # A percentage followed by 1 or more words
      "(\\d{2}%(\\s[[:alpha:]]*\\b){1,})"
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for goal days
#'
#' This function generates a regular expression pattern for matching goal days
#' in text. The pattern matches a string that contains zero or more digits,
#' followed by the word "FDA" or the word "Industry", followed by the word
#' "Days".
#'
#' @return A regular expression pattern
goal_days_pattern <- function() {
  patterns <-
    c(
      # Zero or more digits, followed by the word FDA or the word Industry,
      # followed by the word Days
      "(((\\d{2,})\\s){0,}(FDA|Industry)\\sDays\\b)",
      # Zero or more digits, followed by the word FDA or the word Industry
      "(((\\d{2,})\\s){0,}(FDA|Industry))"
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for within days
#'
#' This function generates a regular expression pattern for matching within days
#' in text. The pattern matches a string that starts with "Within", followed by
#' a space and one or more digits.
#'
#' @return A regular expression pattern
within_days_pattern <- function() {
  patterns <-
    c(
      "((Within)\\s{1,}(\\d{0,})\\b)",
      "\\d{2}\\s(SI\\b)",
      "\\d{2}\\s(SI within\\b)"
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for lines of within days in a table
#'
#' This function generates a regular expression pattern for matching lines of
#' within days in a table. The pattern matches a line that starts with a new
#' line, white space, and five repetitions of a pattern for within days
#' (delimited by vertical bars), followed by white space and a new line.
#'
#' @return A regular expression pattern
within_days_line_pattern <- function() {
  patterns <-
    c(
      # A new line, white space, five repeats of the within days pattern,
      # followed by white space and a new line.
      paste0("\\v\\s{0,}(\\|(", within_days_pattern(), ")\\|*){5}\\v")
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for lines of goal days in a table
#'
#' This function generates a regular expression pattern for matching lines of
#' goal days in a table. The pattern matches a line that starts with a new line,
#' white space, and five repetitions of a pattern for goal days (delimited by
#' vertical bars), followed by white space and a new line.
#'
#' @return A regular expression pattern
goal_days_line_pattern <- function() {
  patterns <-
    c(
      # A new line, white space, five repeats of the goal days pattern, followed
      # by white space and a new line.
      paste0("\\v\\s{0,}(\\|(", goal_days_pattern(), ")\\|*){5}\\v"),
      # A new line, Performance Metric, five repeats of the goal days pattern,
      # followed by white space and a new line.
      paste0(
        "\\vPerformance\\sMetric\\:(\\|(",
        goal_days_pattern(),
        ")\\|*){5}\\v"
      )
    ) %>%
    paste(collapse = "|")
  stringr::regex(
    pattern = patterns,
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Generate a regular expression pattern for table titles
#'
#' This function generates a regular expression pattern for matching table
#' titles in text. The pattern matches a string that starts with "Table",
#' followed by a number, a period, and another number, followed by a space and
#' any characters that are not a vertical bar (|).
#'
#' @return A regular expression pattern
table_title_text_pattern <- function() {
  stringr::regex(
    # Note: \s* after optional period handles cases like "Table 12.2.CDRH"
    # where there's no space between the table number and organization name
    pattern = "(Table\\s\\d*\\.\\d*\\.{0,1}\\s*[^\\|]*(?=\\v))",
    ignore_case = FALSE,
    multiline = FALSE
  )
}

#' Substantive Interaction Metric Pattern
#'
#' This function generates a regular expression pattern for matching performance
#' metrics associated with Substantive Interactions in text.
#'
#' This function returns a regular expression pattern that matches three
#' different types of Substantive Interaction metrics: Average Number of
#' FDA/Industry days to Substantive Interaction, Xth Percentile FDA/Industry
#' days to Substantive Interaction, and Maximum FDA/Industry days to
#' Substantive Interaction. The function ignores case and does not match
#' across multiple lines.
#'
#' The final word needs to be followed by a | for this expression to match.
#'
#' @return a regular expression pattern
#'
#' @export
#'
#' @examples
#' testthat::expect_true(
#'   stringr::str_detect(
#'     string = "\n40th Percentile FDA days to Substantive Interaction|",
#'     pattern = si_metric_pattern()
#'   )
#' )
#'
#' testthat::expect_true(
#'   stringr::str_detect(
#'     string = "\nMaximum FDA days to\nSubstantive Interaction|",
#'     pattern = si_metric_pattern()
#'   )
#' )
#'
#' testthat::expect_true(
#'   stringr::str_detect(
#'     string = "\nAverage number of FDA days to\nSubstantive Interaction|",
#'     pattern = si_metric_pattern()
#'   )
#' )
si_metric_pattern <- function() {
  stringr::regex(
    pattern =
      paste0(
        "(",
        paste0(
          "\\vAverage(\\s|\\v)Number(\\s|\\v)of(\\s|\\v)(FDA|Industry)",
          "(\\s|\\v)days(\\s|\\v)",
          "to(\\s|\\v)Substantive(\\s|\\v)Interaction(?=\\|)"
        ),
        ")|(",
        paste0(
          "\\v\\d{2}th(\\s|\\v)Percentile(\\s|\\v)(FDA|Industry)",
          "(\\s|\\v)days(\\s|\\v)",
          "to(\\s|\\v)Substantive(\\s|\\v)Interaction(?=\\|)"
        ),
        ")|(",
        paste0(
          "\\vMaximum(\\s|\\v)(FDA|Industry)",
          "(\\s|\\v)days(\\s|\\v)",
          "to(\\s|\\v)Substantive(\\s|\\v)Interaction(?=\\|)"
        ),
        ")"
      ),
    ignore_case = TRUE,
    multiline = FALSE
  )
}
