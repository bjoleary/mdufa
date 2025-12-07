#' Remove Unnecessary Text
#'
#' Remove unnecessary text, like section headings and asterisked notes about
#' RTA process implementation, from a text string. Uses the patterns defined in
#' \code{unnecessary_text_pattern()}.
#'
#' @param text_string The string to clean
#'
#' @return A string with anything matching \code{unnecessary_text_pattern()}
#' removed.
#' @export
#'
remove_unnecessary_text <- function(text_string) {
  stringr::str_remove_all(
    string = text_string,
    pattern = unnecessary_text_pattern()
  )
}

#' Add Missing Table Headers
#'
#' Sometimes, the table row containing the fiscal year headers (FY 2018,
#' FY 2019...) does not start with the words "Performance Metric", which leaves
#' the first column of the processed text empty. We want that column to say
#' "Performance Metric", so we will add it.
#'
#' @param text_string The string to check against the pattern and replace as
#' needed.
#'
#' @return Either the original \code{text_string} or one prepended with
#' "Performance Metric".
#' @export
#'
add_missing_table_headers <- function(text_string) {
  replacer <- function(text) {
    paste(
      "Performance Metric",
      text,
      sep = "   "
    )
  }
  stringr::str_replace_all(
    string = text_string,
    pattern = fy_start_line_pattern(),
    replacement = replacer
  )
}

#' Insert Delimiter
#'
#' Places the pipe character (\code{|}) between table columns, which are
#' assumed to be separated by at least \code{whitespace} number of space
#' characters.
#'
#' This function only operates on the first element of \code{text_string}.
#'
#' @param text_string The string to process and insert delimiters into as
#' needed.
#' @param whitespace Defaults to \code{2}. The number of white-spaces to look
#' for and replace with the delimiter (\code{|}).
#'
#' @return The \code{text_string} with delimiters inserted as applicable.
#' @export
#'
insert_delim <- function(text_string, whitespace = 2) {
  text_string_element <- text_string[[1]]
  whitespace <- as.integer(whitespace)
  stringr::str_replace_all(
    string = text_string_element,
    pattern =
      paste0(
        "\\h{",
        whitespace, # Replace this many horizontal white-spaces
        ",}" # or more
      ),
    replacement = "|" # With a pipe character
  ) %>%
    # If you have FY XXXX followed by white-space followed by another FY XXXX,
    # that white-space should almost certainly be replaced with a pipe. Let's
    # use some lookaheads and lookbehinds to catch that.
    stringr::str_replace_all(
      string = .,
      pattern =
        stringr::regex(
          "(?<=\\bFY\\s\\d{4})\\s*(?=FY\\s\\d{4})"
        ),
      replacement = "|"
    )
}

#' Collapse Line Breaks
#'
#' Remove duplicate line breaks from the first element of \code{text_string}.
#'
#' @param text_string The string to process. Only the first element is
#' processed.
#' @param number_of_breaks Defaults to \code{2}. The lower bound of the number
#' of breaks to look for and collapse into a single line break. So, by default,
#' this function collapses two or more line breaks into a single line break.
#'
#' @return The first element of \code{text_string} with line breaks collapsed.
#' @export
#'
collapse_line_breaks <- function(text_string, number_of_breaks = 2) {
  # Collapse multiple line breaks as well
  text_string_element <- text_string[[1]]
  number_of_breaks <- as.integer(number_of_breaks)
  stringr::str_replace_all(
    string = text_string_element,
    pattern =
      paste0(
        "\\v{",
        number_of_breaks, # Replace this many vertical whitespaces
        ",}" # or more
      ),
    replacement = "\n" # With a single line break
  )
}

#' Get Page Number
#'
#' Extract the page number from the first element of \code{text_string}, then
#' return the trimmed text string (without the page number) and all other
#' elements of the object, appending the page number.
#'
#' @param text_string The string to process. Only the first element is
#' processed.
#'
#' @return a list including the trimmed string (not including the page number),
#' the page number, and all other elements that were part of the
#' \code{text_string} passed to the function.
#' @export
#'
get_page_number <- function(text_string) {
  text_string_element <- text_string[[1]] %>% collapse_line_breaks()
  page <-
    stringr::str_extract(
      string = text_string_element,
      pattern = "(?<=\\|Page\\h)\\d*(?=\\sof\\h\\d*$)"
    )
  trimmed_string <-
    stringr::str_remove(
      string = text_string_element,
      pattern = "\\|Page\\h\\d*\\hof\\h\\d*$"
    )
  append(
    list(
      trimmed_string = trimmed_string,
      page = page
    ),
    text_string[-1]
  )
}

#' Get Table Title
#'
#' Extract the table title from the first element of \code{text_string}, then
#' return the trimmed text string (without the table title) and all other
#' elements of the object, appending the table title.
#'
#' @param text_string The string to process. Only the first element is
#' processed.
#'
#' @return a list including the trimmed string (not including the table title),
#' the table title, and all other elements that were part of the
#' \code{text_string} passed to the function.
#'
#' @export
#'
get_table_title <- function(text_string) {
  text_string_element <- text_string[[1]] %>% collapse_line_breaks()
  title <-
    stringr::str_extract(
      string = text_string_element,
      pattern = table_title_text_pattern()
    ) %>%
    stringr::str_squish()
  trimmed_string <-
    stringr::str_remove(
      string = text_string_element,
      pattern = table_title_text_pattern()
    )
  append(
    list(
      trimmed_string = trimmed_string,
      title = title
    ),
    text_string[-1]
  )
}

#' Fix Edge Cases
#'
#' When the more general patterns fail, use more specific patterns. This
#' function cleans up most of the remaining mess the others leave behind.
#'
#' @param text_string The string to process. Only the first element is
#'   processed.
#'
#' @return a list including the trimmed string, the table title, and all other
#'   elements that were part of the \code{text_string} passed to the function.
#' @export
#'
brute_force_row_fix <- function(text_string) {
  text_string_element <- text_string[[1]] %>% collapse_line_breaks()
  patterns <-
    c(
      # 1
      stringr::fixed(
        pattern =
          paste0(
            rep("90% Within 320", 5) %>% paste(collapse = " "),
            "\n|",
            rep("FDA Days", 5) %>% paste(collapse = "|")
          )
      ),
      # 2
      stringr::fixed(
        pattern =
          paste0(
            "95% SI Within 95% SI Within 95% SI Within 95% SI Within",
            "|95% SI Within",
            "\n|",
            rep("90 FDA Days", 5) %>% paste(collapse = "|")
          )
      )
    )

  replacements <-
    c(
      # 1
      rep("90% Within 320 FDA Days", 5) %>% paste(collapse = "|"),
      # 2
      rep("95% SI Within 90 FDA Days", 5) %>% paste(collapse = "|")
    )
  trimmed_string <- text_string_element
  for (i in seq_along(patterns)) {
    trimmed_string <-
      stringr::str_replace_all(
        string = trimmed_string,
        pattern = patterns[[i]],
        replacement = replacements[[i]]
      )
  }

  append(
    list(
      trimmed_string = trimmed_string
    ),
    text_string[-1]
  )
}

#' Fix Goal Row
#'
#' Some of the metrics tables include performance goals, like "90% within 180
#' days," or text along those lines. Between line break and delimiter issues,
#' these rows cause problems. This function attempts to fix those problems.
#'
#' This function is also an example of what happens when one attempts to write
#' comments and documentation a long time after the code is written. For shame.
#'
#' @param text_string The string to process.
#'
#' @return a list including the trimmed string, the table title, and all other
#'   elements that were part of the \code{text_string} passed to the function.
#'
#' @export
#'
fix_goal_row <- function(text_string) {
  goal_line_present <-
    stringr::str_detect(
      string = text_string,
      pattern =
        paste0(
          goal_percent_line_pattern(),
          "*",
          goal_days_line_pattern()
        )
    )
  within_days_line_present <-
    stringr::str_detect(
      string = text_string,
      pattern =
        paste0(
          goal_percent_line_pattern(),
          "*",
          within_days_line_pattern(),
          "*",
          goal_days_line_pattern()
        )
    )
  if (all(goal_line_present == FALSE, within_days_line_present == FALSE)) {
    text_string
  } else if (within_days_line_present == TRUE) {
    # Build the string to swap in
    string_to_replace <-
      paste0(
        stringr::str_extract(
          string = text_string,
          pattern = goal_percent_line_pattern()
        ),
        stringr::str_extract(
          string = text_string,
          pattern = within_days_line_pattern()
        ),
        stringr::str_extract(
          string = text_string,
          pattern = goal_days_line_pattern()
        )
      ) %>%
      collapse_line_breaks()
    # First, let's get the name of the goal
    goal_name <-
      stringr::str_extract(
        string = string_to_replace,
        pattern =
          paste0(
            # The start of the string, followed by any quantity of any
            # character other than the delim (|), followed by a look-ahead that
            # includes the delim (|) and the goal percent pattern.
            "\\v[^\\|]*(?=\\|(",
            goal_percent_pattern(),
            "))"
          )
      )
    goal_percent_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_percent_pattern(),
        simplify = TRUE
      ) %>%
      stringr::str_squish()
    goal_within_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = within_days_pattern(),
        simplify = TRUE
      ) %>%
      stringr::str_squish()
    goal_days_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_days_pattern(),
        simplify = TRUE
      ) %>%
      stringr::str_squish()
    goal_columns <-
      paste(goal_percent_cols, goal_within_cols, goal_days_cols) %>%
      paste(collapse = "|")
    replacement_string <-
      paste0(
        goal_name,
        "|",
        goal_columns,
        "\n"
      )
    stringr::str_replace(
      string = text_string,
      pattern = stringr::fixed(string_to_replace),
      replacement = replacement_string
    )
  } else {
    # Build the string to swap in
    string_to_replace <-
      paste0(
        stringr::str_extract(
          string = text_string,
          pattern = goal_percent_line_pattern()
        ),
        stringr::str_extract(
          string = text_string,
          pattern = goal_days_line_pattern()
        )
      ) %>%
      collapse_line_breaks()
    # First, let's get the name of the goal
    goal_name <-
      stringr::str_extract(
        string = string_to_replace,
        pattern =
          paste0(
            # The start of the string, followed by any quantity of any
            # character other than the delim (|), followed by a look-ahead that
            # includes the delim (|) and the goal percent pattern.
            "\\v[^\\|]*(?=\\|(",
            goal_percent_pattern(),
            "))"
          )
      )
    goal_percent_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_percent_pattern(),
        simplify = TRUE
      ) %>%
      stringr::str_squish()
    goal_days_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_days_pattern(),
        simplify = TRUE
      ) %>%
      stringr::str_squish()
    goal_columns <-
      paste(goal_percent_cols, goal_days_cols) %>%
      paste(collapse = "|")
    replacement_string <-
      paste0(
        goal_name,
        "|",
        goal_columns,
        "\n"
      )
    stringr::str_replace(
      string = text_string,
      pattern = stringr::fixed(string_to_replace),
      replacement = replacement_string
    )
  }
}

#' Fix Substantive Interaction Rows
#'
#' This function fixes performance goals for substantive interactions that break
#' across lines. This is an issue in the MDUFA III reports.
#'
#' @param text_string The string to process.
#'
#' @return The string with extraneous line breaks in the substantive interaction
#'   goals removed.
#' @export
#'
#' @examples
#' testthat::expect_equal(
#'   object =
#'     fix_si_row(
#'       # This text has extraneous line breaks
#'       paste0(
#'         "\nAverage number of FDA days to\nSubstantive Interaction|",
#'         "\nMaximum FDA days to\nSubstantive Interaction|",
#'         "\n40th Percentile FDA days to\nSubstantive Interaction|",
#'         "\n20th Percentile\nFDA\ndays to Substantive Interaction|",
#'         sep = "\n"
#'       )
#'     ),
#'   expected =
#'   # This text does not have extraneous line breaks
#'     paste0(
#'       "\nAverage number of FDA days to Substantive Interaction|",
#'       "\nMaximum FDA days to Substantive Interaction|",
#'       "\n40th Percentile FDA days to Substantive Interaction|",
#'       "\n20th Percentile FDA days to Substantive Interaction|",
#'       sep = "\n"
#'     )
#' )
fix_si_row <- function(text_string) {
  if (
    stringr::str_detect(
      string = text_string,
      pattern = si_metric_pattern()
    )
  ) {
    to_replace <-
      stringr::str_extract_all(
        string = text_string,
        pattern = si_metric_pattern()
      ) %>%
      unlist()
    for (i in seq_along(to_replace)) {
      replacement_string <-
        to_replace[[i]] %>%
        collapse_line_breaks() %>%
        # Replace all line breaks with spaces
        stringr::str_replace_all(
          pattern = stringr::regex("\\v{1,}"),
          replacement = " "
        ) %>%
        # Replace leading space with newline
        stringr::str_replace(
          pattern = stringr::regex("^\\s{1,}"),
          replacement = "\n"
        )
      text_string <-
        stringr::str_replace(
          string = text_string,
          pattern = stringr::regex(to_replace[[i]]),
          replacement = replacement_string
        )
    }
  }
  text_string
}

#' Get Table
#'
#' @param text_string The string to process. Only the first element is
#'   processed.
#'
#' @return a list including the current table as the first element followed by
#'   all other elements that were part of the \code{text_string} passed to the
#'   function.
#'
#' @export
#'
get_table <- function(text_string) {
  text_string_element <- text_string[[1]] %>% collapse_line_breaks()
  additional_table_titles <-
    stringr::str_extract_all(
      string = text_string_element,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    )
  text_string_element <-
    text_string_element %>%
    stringr::str_split(
      string = .,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    )
  # Extract the current table, the first element in the split vector resulting
  # from the last section, and append a newline so the readr package will
  # recognize it as literal data rather than a file name.
  current_table <-
    paste0(text_string_element[[1]], "\n") %>%
    fix_goal_row() %>%
    fix_si_row()
  additional_tables <-
    paste0(additional_table_titles, text_string_element[-1]) %>%
    paste0(collapse = "\n")

  result_table <-
    readr::read_delim(
      file = current_table,
      na = c("", "N/A", "NA"),
      col_types = readr::cols(.default = readr::col_character()),
      col_names = TRUE,
      delim = "|"
    ) %>%
    make_clean_tibble() %>%
    dplyr::mutate(
      source = text_string$title,
      page = text_string$page
    ) %>%
    dplyr::select(
      "source",
      "page",
      dplyr::everything()
    )

  append(
    list(
      trimmed_string = additional_tables,
      table = result_table
    ),
    text_string[-1]
  )
}

#' Process a PDF Page
#'
#' @param page_string The character string of text on the page.
#'
#' @return A list of the page's tables.
#' @export
#'
process_page <- function(page_string) {
  page_data <-
    page_string %>%
    remove_unnecessary_text() %>%
    collapse_line_breaks() %>%
    add_missing_table_headers() %>%
    insert_delim() %>%
    # TODO: Why did I comment this? Is this function used at all?
    # brute_force_row_fix() %>%
    get_page_number()

  # How many tables?
  table_count <-
    stringr::str_extract_all(
      string = page_data$trimmed_string,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    ) %>%
    seq_along()

  output <- vector(mode = "list", length = max(table_count))

  for (i in table_count) {
    page_data <-
      page_data %>%
      get_table_title() %>%
      get_table()
    output[[i]] <- page_data$table
  }
  output
}

#' Process a PDF Page
#'
#' These reports don't always have page numbers, complicating the procedure.
#'
#' @param page_string The character string of text on the page.
#' @param page_number The page number based on the pdf index.
#'
#' @return A list of the page's tables.
#' @export
#'
process_page_m5 <- function(page_string, page_number) {
  page_data <-
    page_string %>%
    remove_unnecessary_text() %>%
    collapse_line_breaks() %>%
    add_missing_table_headers() %>%
    insert_delim() %>%
    # TODO: Why did I comment this? Is this function used at all?
    # brute_force_row_fix() %>%
    get_page_number()

  if (is.na(page_data$page)) {
    page_data$page <- as.character(page_number)
  }

  # How many tables?
  table_count <-
    stringr::str_extract_all(
      string = page_data$trimmed_string,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    ) %>%
    seq_along()

  output <- vector(mode = "list", length = max(table_count))

  for (i in table_count) {
    page_data <-
      page_data %>%
      get_table_title() %>%
      get_table()
    output[[i]] <- page_data$table
  }
  output
}
