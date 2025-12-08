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

#' Strip Page Footer
#'
#' Some MDUFA V reports (May 2025, Aug 2025) have bare page numbers as footers
#' (e.g., just "35" right-aligned at the bottom of the page). These get parsed
#' as data values by insert_delim(). This function detects and removes them.
#'
#' @param text_string The raw page text to clean
#'
#' @return The text with any bare page number footer removed
#' @export
#'
strip_page_footer <- function(text_string) {
  # Pattern: end of string, preceded by 20+ whitespace chars, then 1-3 digits

  # This matches right-aligned bare page numbers like "                    35"
  stringr::str_remove(
    string = text_string,
    pattern = "\\s{20,}\\d{1,3}\\s*$"
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
  ) |>
    # If you have FY XXXX followed by white-space followed by another FY XXXX,
    # that white-space should almost certainly be replaced with a pipe. Let's
    # use some lookaheads and lookbehinds to catch that.
    stringr::str_replace_all(
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
  text_string_element <- text_string[[1]] |> collapse_line_breaks()
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
  text_string_element <- text_string[[1]] |> collapse_line_breaks()
  title <-
    stringr::str_extract(
      string = text_string_element,
      pattern = table_title_text_pattern()
    ) |>
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
  text_string_element <- text_string[[1]] |> collapse_line_breaks()
  patterns <-
    c(
      # 1
      stringr::fixed(
        pattern =
          paste0(
            rep("90% Within 320", 5) |> paste(collapse = " "),
            "\n|",
            rep("FDA Days", 5) |> paste(collapse = "|")
          )
      ),
      # 2
      stringr::fixed(
        pattern =
          paste0(
            "95% SI Within 95% SI Within 95% SI Within 95% SI Within",
            "|95% SI Within",
            "\n|",
            rep("90 FDA Days", 5) |> paste(collapse = "|")
          )
      )
    )

  replacements <-
    c(
      # 1
      rep("90% Within 320 FDA Days", 5) |> paste(collapse = "|"),
      # 2
      rep("95% SI Within 90 FDA Days", 5) |> paste(collapse = "|")
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
      ) |>
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
      ) |>
      stringr::str_squish()
    goal_within_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = within_days_pattern(),
        simplify = TRUE
      ) |>
      stringr::str_squish()
    goal_days_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_days_pattern(),
        simplify = TRUE
      ) |>
      stringr::str_squish()
    goal_columns <-
      paste(goal_percent_cols, goal_within_cols, goal_days_cols) |>
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
      ) |>
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
      ) |>
      stringr::str_squish()
    goal_days_cols <-
      stringr::str_extract_all(
        string = string_to_replace,
        pattern = goal_days_pattern(),
        simplify = TRUE
      ) |>
      stringr::str_squish()
    goal_columns <-
      paste(goal_percent_cols, goal_days_cols) |>
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

#' Fix Table 9.2 Header (MDUFA V)
#'
#' Table 9.2 in MDUFA V reports has a complex multi-line header with goal
#' descriptions that confuses the parser. This function removes those extra
#' header lines while preserving the FY columns and data rows.
#'
#' There are two variants of this table:
#'
#' CDRH variant (all FY columns on one line):
#' \preformatted{
#' |MDUFA V Goal (# of Submissions Received During FY with Written
#' |Feedback Provided by Day 70 or 5 Days Prior to Meeting)
#' Performance Metric|FY 2023|FY 2024|FY 2025|FY 2026|FY 2027
#' |90% / 75%|90% / 80%|90% Within|90% Within|90% Within
#' |Within MDUFA|Within MDUFA|MDUFA Goal|MDUFA Goal|MDUFA Goal
#' |Goal 1|Goal 2
#' }
#'
#' CBER variant (FY columns split across lines):
#' \preformatted{
#' |MDUFA V Goal (# of Submissions Received During FY with Written
#' |Feedback Provided by Day 70 or 5 Days Prior to Meeting)
#' Performance Metric|FY 2023|FY 2024
#' |FY 2025|FY 2026|FY 2027
#' |90% / 75%|90% / 80%
#' Performance Metric|90% Within|90% Within|90% Within
#' |Within MDUFA Within MDUFA
#' |MDUFA Goal|MDUFA Goal|MDUFA Goal
#' |Goal 1|Goal 2
#' }
#'
#' This function is called from get_table() when the table title indicates
#' Table 9.2. The text_string at that point does NOT include the title line.
#'
#' @param text_string The table content string (without title) to process.
#'
#' @return The string with Table 9.2 header cleaned up.
#' @export
#'
fix_table_9_2_header <- function(text_string) {
  # Step 1: Remove the "MDUFA V Goal..." description lines
  result <- stringr::str_replace(
    text_string,
    stringr::regex(
      "[|]MDUFA V Goal[^\\n]*\\n[|]Feedback Provided[^\\n]*\\n"
    ),
    ""
  )

  # Step 2: Handle CBER variant where FY columns are split across two lines
  # Merge "Performance Metric|FY 2023|FY 2024\n|FY 2025|FY 2026|FY 2027"
  # into "Performance Metric|FY 2023|FY 2024|FY 2025|FY 2026|FY 2027"
  result <- stringr::str_replace(
    result,
    stringr::regex(
      "(Performance Metric[|]FY [0-9]{4}[|]FY [0-9]{4})\\n[|](FY [0-9]{4})"
    ),
    "\\1|\\2"
  )

  # Step 3: Remove all remaining junk header lines
  # These start with | and contain goal-related text or "Performance Metric"
  # Pattern: lines starting with | containing 90%, Within, Goal, or
  # lines starting with "Performance Metric|90%"
  junk_patterns <- c(
    "\\n[|]90%[^\\n]*", # |90%...
    "\\n[|][^\\n]*Within MDUFA[^\\n]*", # |Within MDUFA...
    "\\n[|][^\\n]*MDUFA Goal[^\\n]*", # |MDUFA Goal...
    "\\n[|]Goal [^\\n]*", # |Goal 1... or |Goal ¹...
    "\\nPerformance Metric[|]90%[^\\n]*", # Performance Metric|90%...
    "\\nPerformance Metric[|]Within MDUFA[^\\n]*" # Perf Metric|Within...
  )

  for (pattern in junk_patterns) {
    result <- stringr::str_replace_all(result, stringr::regex(pattern), "")
  }

  result
}

#' Fix Breakthrough/STeP Multi-Line Metric
#'
#' In Table 9.1 (Pre-Sub Acceptance Review Decision), the metric "Interactions
#' for Breakthrough Designated Products & Products Included in STeP" spans
#' 3 lines in the PDF:
#'
#' \preformatted{
#' Interactions for Breakthrough
#' Designated Products & Products                    24
#' Included in STeP
#' }
#'
#' This function merges those lines into a single row.
#'
#' @param text_string The string to process.
#'
#' @return The string with the Breakthrough metric row properly combined.
#' @export
#'
fix_breakthrough_metric <- function(text_string) {
  # Pattern to match the 3-line metric name with value on middle line
  # After delimiter insertion, it looks like:
  #  Interactions for Breakthrough
  #  Designated Products & Products|24
  #  Included in STeP
  # (lines start with space, not pipe)
  #
  # We need to merge into:
  # Interactions for Breakthrough Designated Products & Products Included in
  # STeP|24

  pattern <- paste0(
    "(\\s*Interactions for Breakthrough)", # Line 1 (may have leading space)
    "\\v", # Line break
    "\\s*Designated Products & Products", # Line 2 start
    "\\|", # Delimiter before value
    "([^\\v]+)", # Value (capture group 2)
    "\\v", # Line break
    "\\s*Included in STeP" # Line 3
  )

  if (stringr::str_detect(text_string, stringr::regex(pattern))) {
    text_string <- stringr::str_replace_all(
      string = text_string,
      pattern = stringr::regex(pattern),
      replacement = paste0(
        "\nInteractions for Breakthrough ",
        "Designated Products & Products ",
        "Included in STeP|\\2"
      )
    )
  }

  text_string
}

#' Fix MDUFA III SI Goals Row
#'
#' This function fixes the Substantive Interaction Goals row in MDUFA III
#' reports, where varying percentages (65%, 75%, 85%, 95%) span multiple lines.
#'
#' @param text_string The string to process.
#'
#' @return The string with SI Goals row properly combined.
#' @export
#'
fix_m3_si_goal_row <- function(text_string) {
  # MDUFA III has two different SI Goals patterns:
  #
  # Pattern A (page 20 - 5 lines with FY17 split differently):
  # |65% SI|75% SI|85% SI|95% SI
  # |95% SI within
  # |Substantive Interaction (SI) Goals:|within 90|within 90|within 90|within 90
  # |90 FDA days
  # |FDA days|FDA days|FDA days|FDA days
  #
  # Pattern B (page 27 and others - 3 lines):
  # |65% SI|75% SI|85% SI|95% SI|95% SI
  # Substantive Interaction (SI) Goals:|within 90|within 90|within 90|...
  # |FDA days|FDA days|FDA days|FDA days|FDA days

  # First check if the base pattern exists
  if (!stringr::str_detect(
    string = text_string,
    pattern = stringr::regex(
      "\\|65% SI\\|75% SI\\|85% SI\\|95% SI",
      ignore_case = FALSE
    )
  )) {
    return(text_string)
  }

  # Build the replacement string (same for both patterns)
  replacement <- paste0(
    "\nSubstantive Interaction (SI) Goals:|",
    "65% SI within 90 FDA days|",
    "75% SI within 90 FDA days|",
    "85% SI within 90 FDA days|",
    "95% SI within 90 FDA days|",
    "95% SI within 90 FDA days"
  )

  # Try Pattern A first (5-line pattern from page 20)
  pattern_a <- paste0(
    "(\\|65% SI\\|75% SI\\|85% SI\\|95% SI)", # Line 1: percentages
    "\\v",
    "(\\|95% SI within)", # Line 2: FY17 partial
    "\\v",
    "(\\|Substantive Interaction \\(SI\\) Goals:\\|", # Line 3: metric name
    "within 90\\|within 90\\|within 90\\|within 90)", # and within values
    "\\v",
    "(\\|90 FDA days)", # Line 4: FY17 days
    "\\v",
    "(\\|FDA days\\|FDA days\\|FDA days\\|FDA days)" # Line 5: FDA days
  )

  if (stringr::str_detect(text_string, stringr::regex(pattern_a))) {
    return(stringr::str_replace(
      string = text_string,
      pattern = stringr::regex(pattern_a),
      replacement = replacement
    ))
  }

  # Try Pattern B (3-line pattern from page 27, 33, etc.)
  pattern_b <- paste0(
    "(\\|65% SI\\|75% SI\\|85% SI\\|95% SI\\|95% SI)", # Line 1: percentages
    "\\v",
    "(\\s*Substantive Interaction \\(SI\\) Goals:\\|", # Line 2: metric
    "within 90\\|within 90\\|within 90\\|within 90\\|within 90)", # within
    "\\v",
    "(\\|FDA days\\|FDA days\\|FDA days\\|FDA days\\|FDA days)" # Line 3
  )

  if (stringr::str_detect(text_string, stringr::regex(pattern_b))) {
    return(stringr::str_replace(
      string = text_string,
      pattern = stringr::regex(pattern_b),
      replacement = replacement
    ))
  }

  text_string
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
      ) |>
      unlist()
    for (i in seq_along(to_replace)) {
      replacement_string <-
        to_replace[[i]] |>
        collapse_line_breaks() |>
        # Replace all line breaks with spaces
        stringr::str_replace_all(
          pattern = stringr::regex("\\v{1,}"),
          replacement = " "
        ) |>
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

#' Fix Wrapped Metric Names
#'
#' This function fixes metric names that wrap across multiple lines in PDF
#' extraction. The pattern is:
#' - Line 1: Metric name start (no pipe at end)
#' - Line 2: Values (starts with |) OR metric continuation + values
#' - Line 3 (optional): Metric name end (no pipes)
#'
#' @param text_string The string to process
#'
#' @return The string with wrapped metric names merged
#' @export
#'
fix_wrapped_metric_names <- function(text_string) {
  lines <- strsplit(text_string, "\n")[[1]]
  result <- character(0)

  i <- 1

  while (i <= length(lines)) {
    line <- lines[i]

    # Check if this line is a metric name start (has text, NO pipes at all,
    # and next line has values)
    is_metric_start <- !stringr::str_detect(line, "\\|") &&
      nchar(trimws(line)) > 0 &&
      !stringr::str_detect(line, "^Table ") &&
      !stringr::str_detect(line, "^Performance Metric")

    if (is_metric_start && i < length(lines)) {
      next_line <- lines[i + 1]
      has_values <- stringr::str_detect(next_line, "\\|")

      if (has_values) {
        # Check if next line is values-only (starts with |)
        values_only <- stringr::str_detect(next_line, "^\\|")

        if (values_only) {
          # MDUFA V pattern 1: metric_start / |values / metric_end (optional)
          merged <- trimws(line)

          # Check if there's a continuation line after values
          if (i + 2 <= length(lines)) {
            cont_line <- lines[i + 2]
            # Continuation has no pipes and is short text
            is_continuation <- !stringr::str_detect(cont_line, "\\|") &&
              nchar(trimws(cont_line)) > 0 &&
              nchar(trimws(cont_line)) < 80 &&
              !stringr::str_detect(cont_line, "^Table ") &&
              !stringr::str_detect(cont_line, "^\\*") &&
              !stringr::str_detect(cont_line, "^\\d+\\.")

            if (is_continuation) {
              merged <- paste(merged, trimws(cont_line))
              merged <- paste0(merged, next_line)
              result <- c(result, merged)
              i <- i + 3
              next
            }
          }
          # No continuation, just merge metric start with values
          merged <- paste0(merged, next_line)
          result <- c(result, merged)
          i <- i + 2
          next
        } else {
          # MDUFA V pattern 2: metric_start / metric_mid+values / metric_end
          # Extract values from next_line
          values <- stringr::str_extract(next_line, "\\|.*$")
          metric_mid <- stringr::str_remove(next_line, "\\|.*$")

          merged <- paste(trimws(line), trimws(metric_mid))

          # Check for continuation after the values line
          if (i + 2 <= length(lines)) {
            cont_line <- lines[i + 2]
            is_continuation <- !stringr::str_detect(cont_line, "\\|") &&
              nchar(trimws(cont_line)) > 0 &&
              nchar(trimws(cont_line)) < 80 &&
              !stringr::str_detect(cont_line, "^Table ") &&
              !stringr::str_detect(cont_line, "^\\*") &&
              !stringr::str_detect(cont_line, "^\\d+\\.")

            if (is_continuation) {
              merged <- paste(merged, trimws(cont_line))
              merged <- paste0(merged, values)
              result <- c(result, merged)
              i <- i + 3
              next
            }
          }
          merged <- paste0(merged, values)
          result <- c(result, merged)
          i <- i + 2
          next
        }
      }
    }

    # Default: keep line as-is
    result <- c(result, line)
    i <- i + 1
  }

  paste(result, collapse = "\n")
}

#' Get Table
#'
#' @param text_string The string to process. Only the first element is
#'   processed.
#' @param fix_wrapped_names Logical. If TRUE, attempts to fix wrapped metric
#'   names that span multiple lines. Only use for MDUFA V reports.
#'
#' @return a list including the current table as the first element followed by
#'   all other elements that were part of the \code{text_string} passed to the
#'   function.
#'
#' @export
#'
get_table <- function(text_string, fix_wrapped_names = FALSE) {
  text_string_element <- text_string[[1]] |> collapse_line_breaks()
  additional_table_titles <-
    stringr::str_extract_all(
      string = text_string_element,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    )
  text_string_element <-
    text_string_element |>
    stringr::str_split(
      pattern = table_title_text_pattern(),
      simplify = TRUE
    )
  # Extract the current table, the first element in the split vector resulting
  # from the last section, and append a newline so the readr package will
  # recognize it as literal data rather than a file name.
  current_table <- paste0(text_string_element[[1]], "\n")

  # Only apply wrapped metric name fix for MDUFA V
  if (fix_wrapped_names) {
    current_table <- fix_wrapped_metric_names(current_table)
  }

  current_table <- current_table |>
    fix_m3_si_goal_row() |>
    fix_goal_row() |>
    fix_si_row() |>
    fix_breakthrough_metric()

  # Apply Table 9.2 fix if this is that table (MDUFA V specific)
  if (!is.null(text_string$title) &&
    stringr::str_detect(text_string$title, "Table 9[.]2")) {
    current_table <- fix_table_9_2_header(current_table)
  }

  additional_tables <-
    paste0(additional_table_titles, text_string_element[-1]) |>
    paste0(collapse = "\n")

  result_table <-
    readr::read_delim(
      file = current_table,
      na = c("", "N/A", "NA"),
      col_types = readr::cols(.default = readr::col_character()),
      col_names = TRUE,
      delim = "|"
    ) |>
    make_clean_tibble() |>
    dplyr::mutate(
      source = text_string$title,
      page = text_string$page
    ) |>
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
    page_string |>
    remove_unnecessary_text() |>
    collapse_line_breaks() |>
    add_missing_table_headers() |>
    insert_delim() |>
    # TODO: Why did I comment this? Is this function used at all?
    # brute_force_row_fix() |>
    get_page_number()

  # How many tables?
  table_count <-
    stringr::str_extract_all(
      string = page_data$trimmed_string,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    ) |>
    seq_along()

  output <- vector(mode = "list", length = max(table_count))

  for (i in table_count) {
    page_data <-
      page_data |>
      get_table_title() |>
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
    page_string |>
    strip_page_footer() |>
    remove_unnecessary_text() |>
    collapse_line_breaks() |>
    add_missing_table_headers() |>
    insert_delim() |>
    # TODO: Why did I comment this? Is this function used at all?
    # brute_force_row_fix() |>
    get_page_number()

  if (is.na(page_data$page)) {
    page_data$page <- as.character(page_number)
  }


  # Remove orphaned content before the first table title
  # This handles pages that start with continuation data from previous page
  first_table_match <- stringr::str_locate(
    string = page_data$trimmed_string,
    pattern = table_title_text_pattern()
  )

  first_match_start <- first_table_match[1, "start"]
  if (!is.na(first_match_start) && first_match_start > 1) {
    page_data$trimmed_string <- stringr::str_sub(
      page_data$trimmed_string,
      start = first_table_match[1, "start"]
    )
  }

  # How many tables?
  table_titles <-
    stringr::str_extract_all(
      string = page_data$trimmed_string,
      pattern = table_title_text_pattern(),
      simplify = TRUE
    )

  # If no table titles found, return NULL (page has no extractable tables)
  if (length(table_titles) == 0 || all(table_titles == "")) {
    return(NULL)
  }

  table_count <- seq_along(table_titles)
  output <- vector(mode = "list", length = max(table_count))

  for (i in table_count) {
    tryCatch({
      page_data <-
        page_data |>
        get_table_title() |>
        get_table(fix_wrapped_names = TRUE)
      output[[i]] <- page_data$table
    }, error = function(e) {
      # Skip tables that fail to parse (e.g., reference pages with no data)
      output[[i]] <<- NULL
    })
  }
  # Remove NULL entries and return
  output[!sapply(output, is.null)]
}
