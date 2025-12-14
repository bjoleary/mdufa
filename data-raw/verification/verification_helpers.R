# verification_helpers.R
# Helper functions for MDUFA extraction verification
# NOT part of the installed package - development use only

#' Find Local PDF by Pattern
#'
#' Searches for a PDF in the local archive matching a pattern.
#'
#' @param pattern Pattern to match (e.g., "mdufa-4_2023-11-16")
#' @param pdf_dir Directory to search
#' @return Path to PDF or NULL if not found
find_local_pdf <- function(pattern, pdf_dir = "data-raw/pdf_reports") {
  files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    return(NULL)
  }
  files[1]
}

#' Generate Row Image
#'
#' Renders a PDF page with the target row highlighted.
#'
#' @param table_number Table number
#' @param organization Organization code
#' @param performance_metric Metric name
#' @param page PDF page number
#' @param pdf_path Path to PDF
#' @param output_path Where to save the image
#' @param dpi Resolution
#' @return Path to generated image
generate_row_image <- function(table_number,
                               organization,
                               performance_metric,
                               page,
                               pdf_path,
                               output_path,
                               dpi = 150) {
  # Render page
  img <- magick::image_read_pdf(pdf_path, pages = page, density = dpi)

  # Get word-level data for highlighting
  page_data <- pdftools::pdf_data(pdf_path)[[page]]
  scale <- dpi / 72

  # Find table boundaries
  table_num_escaped <- gsub("\\.", "\\\\.", table_number)
  table_num_pattern <- paste0("^", table_num_escaped, "([^0-9]|$)")
  table_num_matches <- page_data[grepl(table_num_pattern, page_data$text), ]

  table_start_y <- 0
  table_end_y <- Inf

  if (nrow(table_num_matches) > 0) {
    table_start_y <- min(table_num_matches$y) - 20
    table_words <- page_data[page_data$text == "Table", ]
    if (nrow(table_words) > 0) {
      min_y <- min(table_num_matches$y) + 20
      tables_after_y <- table_words$y[table_words$y > min_y]
      if (length(tables_after_y) > 0) {
        table_end_y <- min(tables_after_y) - 10
      }
    }
  }

  # Find FY header row FIRST (so we can exclude it from metric search)
  fy_pattern <- "^\\d{4}$"
  fy_headers <- page_data[
    grepl(fy_pattern, page_data$text) &
      page_data$y >= table_start_y &
      page_data$y <= table_end_y,
  ]

  fy_header_y <- NULL
  if (nrow(fy_headers) > 0) {
    header_y_candidates <- fy_headers |>
      dplyr::count(.data$y) |>
      dplyr::filter(.data$n >= 3) |>
      dplyr::slice_min(.data$y, n = 1) |>
      dplyr::pull(.data$y)

    if (length(header_y_candidates) > 0) {
      fy_header_y <- header_y_candidates[1]
    }
  }

  # Data rows start below the header row
  data_start_y <- if (!is.null(fy_header_y)) fy_header_y + 5 else table_start_y

  # Find metric row by searching for the metric text
  metric_y <- NULL
  if (!is.na(performance_metric)) {
    y_tolerance <- 5

    # Get unique Y positions in table section
    data_rows <- page_data[
      page_data$y >= data_start_y & page_data$y <= table_end_y,
    ]
    unique_ys <- sort(unique(data_rows$y))

    # Normalize metric text for matching
    metric_normalized <- tolower(gsub("\\s+", " ", trimws(performance_metric)))
    metric_words <- strsplit(metric_normalized, " ")[[1]]
    metric_words <- metric_words[nchar(metric_words) > 2] # Skip short words

    # Step 1: Try EXACT match first (row text equals metric text)
    for (y in unique_ys) {
      row_words <- data_rows[abs(data_rows$y - y) <= y_tolerance, ]
      row_words <- row_words[order(row_words$x), ] # Sort left-to-right
      row_text <- tolower(paste(row_words$text, collapse = " "))
      row_text <- gsub("\\s+", " ", trimws(row_text))

      if (row_text == metric_normalized) {
        metric_y <- y
        break
      }
    }

    # Step 2: Try partial match - prefer rows with fewest EXTRA words
    # This prevents "MDUFA IV Decisions" matching "Eligible for MDUFA IV Decisions"
    if (is.null(metric_y)) {
      best_match_y <- NULL
      best_match_score <- -Inf # Higher is better

      for (y in unique_ys) {
        row_words_df <- data_rows[abs(data_rows$y - y) <= y_tolerance, ]
        row_text <- tolower(paste(row_words_df$text, collapse = " "))

        # Count how many metric words appear in this row
        matches <- sum(sapply(metric_words, function(w) {
          grepl(w, row_text, fixed = TRUE)
        }))

        # Skip if less than 60% of metric words match
        if (matches < length(metric_words) * 0.6) next

        # Count extra words in row not in metric (penalty)
        row_word_list <- strsplit(row_text, "\\s+")[[1]]
        row_word_list <- row_word_list[nchar(row_word_list) > 2]
        extra_words <- sum(!row_word_list %in% metric_words)

        # Score = matches - extra_words (prefer fewer extra words)
        score <- matches - (extra_words * 0.5)

        if (score > best_match_score) {
          best_match_score <- score
          best_match_y <- y
        }
      }

      if (!is.null(best_match_y)) {
        metric_y <- best_match_y
      }
    }

    # Step 3: Fallback to distinctive word matching if partial match failed
    if (is.null(metric_y)) {
      metric_words <- strsplit(performance_metric, "\\s+")[[1]]
      # Only filter truly generic words - keep distinctive words like
      # FDA, Industry, Total, Average, Days which differentiate metrics
      common_words <- c(
        "Number", "of", "the", "a", "an", "to", "for", "and", "or",
        "with", "in", "on", "by", "Rate"
      )
      distinctive <- metric_words[
        !metric_words %in% common_words & nchar(metric_words) > 2
      ]

      # Collect y-positions for distinctive words (only in table section)
      word_y_positions <- list()
      for (word in distinctive) {
        matches <- page_data[
          page_data$text == word &
            page_data$y >= data_start_y &
            page_data$y <= table_end_y,
        ]
        if (nrow(matches) > 0) {
          word_y_positions[[word]] <- matches$y
        }
      }

      # Find y-position where distinctive words co-occur
      # Priority: prefer rows where ALL words match, then most matches
      if (length(word_y_positions) > 0) {
        all_ys <- unlist(word_y_positions)
        unique_ys <- unique(all_ys)
        best_y <- NULL
        best_count <- 0

        # Key differentiator words - these should be required if present
        key_words <- c(
          "FDA", "Industry", "Total", "Average", "Days", "Maximum", "Minimum"
        )
        required_words <- intersect(names(word_y_positions), key_words)

        for (y in unique_ys) {
          # Count how many distinctive words appear at this y
          words_at_y <- names(word_y_positions)[sapply(
            word_y_positions,
            function(ys) any(abs(ys - y) <= y_tolerance)
          )]
          count <- length(words_at_y)

          # Check if all required key words are present at this y
          has_all_required <- all(required_words %in% words_at_y)

          # Prefer: (1) rows with all required words, (2) highest count
          if (has_all_required && count > best_count) {
            best_count <- count
            best_y <- y
          } else if (is.null(best_y) && count > best_count) {
            # Fallback if no row has all required words
            best_count <- count
            best_y <- y
          }
        }

        # If we found a match but it doesn't have all required words,
        # try again prioritizing required words
        if (!is.null(best_y) && length(required_words) > 0) {
          for (y in unique_ys) {
            words_at_y <- names(word_y_positions)[sapply(
              word_y_positions,
              function(ys) any(abs(ys - y) <= y_tolerance)
            )]
            if (all(required_words %in% words_at_y)) {
              best_y <- y
              break
            }
          }
        }

        if (!is.null(best_y)) {
          metric_y <- best_y
        }
      }

      # Fallback: first distinctive word (in table section only)
      if (is.null(metric_y) && length(distinctive) > 0) {
        for (word in distinctive) {
          matches <- page_data[
            page_data$text == word &
              page_data$y >= data_start_y &
              page_data$y <= table_end_y,
          ]
          if (nrow(matches) > 0) {
            metric_y <- matches$y[1]
            break
          }
        }
      }
    }
  }

  # Get FY columns from the header row we already found
  if (!is.null(fy_header_y) && nrow(fy_headers) > 0) {
    fy_columns <- fy_headers[fy_headers$y == fy_header_y, ]
  } else {
    fy_columns <- data.frame()
  }

  # Draw highlight
  if (!is.null(metric_y)) {
    img <- magick::image_draw(img)

    # MDUFA tables have consistent layout:
    # - Metric names start around x=52
    # - FY value columns end around x=565
    # Use fixed positions that work across all tables
    row_left <- 52 * scale
    row_right <- 565 * scale

    # Estimate row height from table structure
    table_ys <- page_data$y[
      page_data$y >= table_start_y & page_data$y <= table_end_y
    ]
    unique_ys <- sort(unique(table_ys))
    base_row_height <- 24  # Default row height in scaled pixels

    # Try to estimate row height from spacing between lines
    if (length(unique_ys) > 2) {
      diffs <- diff(unique_ys)
      # Filter to reasonable row heights (8-30 points)
      reasonable_diffs <- diffs[diffs >= 8 & diffs <= 30]
      if (length(reasonable_diffs) > 0) {
        base_row_height <- median(reasonable_diffs) * scale
      }
    }

    # Adjust row height based on metric name length
    # Longer metrics are more likely to wrap to multiple lines
    metric_len <- nchar(performance_metric)
    if (metric_len > 60) {
      # Very long metrics often wrap to 3 lines
      row_height <- base_row_height * 2.5
    } else if (metric_len > 40) {
      # Medium-long metrics often wrap to 2 lines
      row_height <- base_row_height * 1.8
    } else {
      row_height <- base_row_height
    }

    row_top <- metric_y * scale - 3
    row_bottom <- row_top + max(row_height, 24)

    graphics::rect(
      row_left, row_top, row_right, row_bottom,
      col = grDevices::adjustcolor("orange", alpha.f = 0.4),
      border = "darkorange", lwd = 2
    )

    grDevices::dev.off()
  }

  # Add annotation
  label <- paste0(
    "Table ", table_number, " | ", organization, " | ",
    substr(performance_metric, 1, 50)
  )
  img <- magick::image_annotate(img, label,
    size = 14, color = "black", boxcolor = "white",
    location = "+10+10"
  )

  magick::image_write(img, output_path)

  # Return path and highlight position for dynamic overlay positioning
  list(
    path = output_path,
    highlight_y = if (!is.null(metric_y)) metric_y * scale else NA
  )
}

#' Verify Metric Row
#'
#' Highlights an entire row of values for a performance metric across
#' all fiscal years. Uses key fields (table_number, organization, metric)
#' rather than expected values for location.
#'
#' @param table_number Table number (e.g., "1.1")
#' @param organization Organization code (e.g., "OHT8", "CDRH")
#' @param performance_metric Metric name to highlight
#' @param page PDF page number
#' @param pdf_path Path to PDF
#' @param mdufa_period MDUFA period (used if pdf_path is NULL)
#' @param report_date Report date (used if pdf_path is NULL)
#' @param pdf_dir Directory containing PDFs
#' @param dpi Resolution for rendering
#'
#' @return Invisibly returns path to generated image
verify_metric <- function(table_number,
                          organization,
                          performance_metric,
                          page = NULL,
                          pdf_path = NULL,
                          mdufa_period = NULL,
                          report_date = NULL,
                          pdf_dir = "data-raw/pdf_reports",
                          dpi = 150) {
  # Resolve PDF path if not provided
  if (is.null(pdf_path)) {
    stopifnot(!is.null(mdufa_period), !is.null(report_date))
    mdufa_num <- switch(mdufa_period,
      "MDUFA III" = "3",
      "MDUFA IV" = "4",
      "MDUFA V" = "5"
    )
    pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)
    pdf_files <- list.files(pdf_dir, pattern = pdf_pattern, full.names = TRUE)
    if (length(pdf_files) == 0) {
      stop("No PDF found matching: ", pdf_pattern)
    }
    pdf_path <- pdf_files[1]
  }

  # Find page if not provided
  if (is.null(page)) {
    page <- find_table_page(pdf_path, table_number)
    if (is.na(page)) {
      stop("Table ", table_number, " not found in PDF")
    }
  }

  # Generate image to temp location
  timestamp <- format(Sys.time(), "%H%M%S")
  out_path <- file.path(
    "/tmp",
    paste0(
      "verify_", table_number, "_",
      gsub(" ", "_", substr(performance_metric, 1, 30)),
      "_", timestamp, ".png"
    )
  )

  generate_row_image(
    table_number = table_number,
    organization = organization,
    performance_metric = performance_metric,
    page = page,
    pdf_path = pdf_path,
    output_path = out_path,
    dpi = dpi
  )

  # Open the image
  message("Table: ", table_number, " | Org: ", organization)
  message("Metric: ", performance_metric)
  message("Page: ", page)
  message("Opening: ", out_path)

  system(paste("open", shQuote(out_path)), wait = FALSE)

  invisible(out_path)
}

#' Verify Data Row (v2)
#'
#' Wrapper around verify_metric() that takes a data frame row.
#' Highlights entire metric row, not just single cell.
#'
#' @param row Single row from mdufa dataset
#' @param ... Additional arguments passed to verify_metric()
#' @return Invisibly returns path to generated image
verify_row_v2 <- function(row, ...) {
  if (nrow(row) != 1) {
    stop("row must be a single row")
  }

  verify_metric(
    table_number = as.character(row$table_number),
    organization = as.character(row$organization),
    performance_metric = as.character(row$performance_metric),
    page = as.integer(row$page),
    mdufa_period = as.character(row$report_mdufa_period),
    report_date = as.character(row$report_date),
    ...
  )
}

#' Generate Verification Test Code
#'
#' Creates testthat code for a verified data point that can be
#' copied into a test file. Tests fresh extraction from PDF.
#'
#' @param row Single row from extracted data (with report_date)
#' @param pdf_page Page number where value was verified
#' @param verifier Name of person who verified
#'
#' @return Character string with test code (printed to console)
generate_verification_test <- function(row,
                                       pdf_page,
                                       verifier = "Brendan O'Leary") {
  mdufa_num <- switch(as.character(row$report_mdufa_period),
    "MDUFA III" = "3",
    "MDUFA IV" = "4",
    "MDUFA V" = "5"
  )
  pdf_pattern <- paste0("mdufa-", mdufa_num, "_", row$report_date)

  code <- glue::glue('
test_that("{row$table_number} {row$organization} {substr(row$performance_metric, 1, 30)}...", {{
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("{pdf_pattern}")
  skip_if(is.null(pdf_path), "{row$report_mdufa_period} PDF not available")

  # Verified {Sys.Date()} against PDF page {pdf_page}
  # Verifier: {verifier}
  data <- extract_report(pdf_path, mdufa_period = "{row$report_mdufa_period}") |>
    dplyr::filter(
      table_number == "{row$table_number}",
      organization == "{row$organization}",
      performance_metric == "{row$performance_metric}"
    )

  expect_equal(data$value[data$fy == "{row$fy}"], "{row$value}")
}})
')
  cat(code)
  invisible(code)
}

#' Check Verification Status
#'
#' Uses dxr::agreement() to calculate the Wilson score confidence interval
#' and determines if verification is complete, failed, or needs more samples.
#'
#' @param pass_count Number of passed metrics
#' @param fail_count Number of failed metrics
#' @return List with status, message, agreement result, and optionally
#'   samples_needed
check_verification_status <- function(pass_count, fail_count) {
  if (fail_count > 0) {
    agr <- dxr::agreement(pass_count, fail_count)
    return(list(
      status = "FAILED",
      message = paste0(fail_count, " failure(s) - fix code and re-verify"),
      agreement = agr
    ))
  }

  if (pass_count == 0) {
    return(list(
      status = "CONTINUE",
      message = "No samples verified yet",
      samples_needed = 35
    ))
  }

  # Calculate Wilson score CI using dxr::agreement

  agr <- dxr::agreement(pass_count, fail_count)
  lower_bound <- agr$lower

  if (lower_bound > 0.90) {
    return(list(
      status = "COMPLETE",
      lower_bound = lower_bound,
      message = paste0(
        "LB = ", round(lower_bound * 100, 2),
        "% > 90% with n=", pass_count
      ),
      agreement = agr
    ))
  } else {
    # Estimate samples needed for LB > 90%
    # Iteratively find minimum n where agreement(n, 0)$lower > 0.90
    samples_needed <- pass_count
    while (dxr::agreement(samples_needed, 0)$lower <= 0.90 &&
           samples_needed < 500) {
      samples_needed <- samples_needed + 1
    }
    samples_needed <- samples_needed - pass_count

    return(list(
      status = "CONTINUE",
      lower_bound = lower_bound,
      message = paste0(
        "LB = ", round(lower_bound * 100, 2),
        "% <= 90% - need ~", samples_needed, " more metrics"
      ),
      samples_needed = samples_needed,
      agreement = agr
    ))
  }
}

#' Generate Test File
#'
#' Automatically generates a testthat file from verification results.
#'
#' @param results Passed verification results (data frame with status == "pass")
#' @param pdf_path Path to the PDF that was verified
#' @param mdufa_period MDUFA period string
#' @param output_file Path for the test file. If NULL (default), automatically
#'   generates filename based on mdufa_period and report date from pdf_path.
#' @param full_data Optional full extracted data to look up all FY values for
#'   each verified metric. If NULL, only the single FY from results is tested.
#' @return Path to generated file
generate_test_file <- function(results,
                               pdf_path,
                               mdufa_period,
                               output_file = NULL,
                               full_data = NULL) {
  mdufa_num <- switch(mdufa_period,
    "MDUFA III" = "3",
    "MDUFA IV" = "4",
    "MDUFA V" = "5"
  )

  # Extract report date from pdf_path
  report_date <- stringr::str_extract(basename(pdf_path), "\\d{4}-\\d{2}-\\d{2}")

  # Auto-generate output_file if not provided
  if (is.null(output_file)) {
    output_file <- file.path(
      "tests", "testthat",
      paste0("test-verified-mdufa", mdufa_num, "-", report_date, ".R")
    )
  }

  pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)

  # If full_data provided, expand results to include all FY values for each
  # verified metric (including NA values)
  if (!is.null(full_data)) {
    # Get unique metric keys from verified results
    key_cols <- c("table_number", "organization", "performance_metric")
    unique_metrics <- results |>
      dplyr::distinct(dplyr::across(dplyr::all_of(key_cols)))

    # Look up all FY values for each verified metric from full_data
    # Keep NA values - they are important to verify
    expanded_results <- full_data |>
      dplyr::semi_join(unique_metrics, by = key_cols) |>
      dplyr::select(
        dplyr::all_of(key_cols),
        "fy",
        "value"
      )

    n_new_metrics <- nrow(unique_metrics)
    n_new_values <- nrow(expanded_results)
    n_na <- sum(is.na(expanded_results$value))
    message(
      "Expanded ", n_new_metrics, " verified metrics to ",
      n_new_values, " FY/value pairs (", n_na, " NA values)"
    )
  } else {
    expanded_results <- results |>
      dplyr::select(
        .data$table_number,
        .data$organization,
        .data$performance_metric,
        .data$fy,
        .data$value
      )
    n_new_metrics <- nrow(results)
    n_new_values <- nrow(results)
  }

  # Check if test file already exists and parse existing assertions
  existing_results <- NULL
  if (file.exists(output_file)) {
    message("Found existing test file, will append new tests...")
    existing_results <- parse_existing_test_file(output_file)
    if (!is.null(existing_results) && nrow(existing_results) > 0) {
      message("  Parsed ", nrow(existing_results), " existing assertions")
    }
  }

  # Merge new and existing results, preferring new values for duplicates
  if (!is.null(existing_results) && nrow(existing_results) > 0) {
    key_cols_full <- c("table_number", "organization", "performance_metric", "fy")

    # Remove from existing any keys that are in new results
    existing_deduped <- existing_results |>
      dplyr::anti_join(expanded_results, by = key_cols_full)

    # Combine: new results first, then existing (deduped)
    combined_results <- dplyr::bind_rows(expanded_results, existing_deduped)

    n_existing_kept <- nrow(existing_deduped)
    n_replaced <- nrow(existing_results) - n_existing_kept
    if (n_replaced > 0) {
      message("  Replaced ", n_replaced, " existing assertions with new values")
    }
    message("  Kept ", n_existing_kept, " existing assertions")
  } else {
    combined_results <- expanded_results
  }

  # Count unique metrics for header
  n_metrics <- combined_results |>
    dplyr::distinct(table_number, organization, performance_metric) |>
    nrow()
  n_values <- nrow(combined_results)

  message("Total: ", n_metrics, " metrics, ", n_values, " values")

  # Generate header with helper function
  # Build nolint directive separately to avoid lintr seeing it in this file
  nolint_directive <- paste0("# nolint", " start")
  header <- glue::glue('
{nolint_directive}
# Verified extraction tests for {mdufa_period} {report_date} report
# Generated: {Sys.Date()}
# Verifier: Brendan O\'Leary
# Sample size: {n_metrics} metrics, {n_values} values
# Statistical basis: LB of 95% CI > 90% (Wilson score)

# Helper function to find local PDF (works from testthat directory)
find_local_pdf <- function(pattern) {{
  # testthat runs from tests/testthat, so go up two levels
  pdf_dir <- testthat::test_path("..", "..", "data-raw", "pdf_reports")
  files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NULL)
  files[1]
}}

test_that("{mdufa_period} {report_date} extraction is accurate", {{
  skip_if_not_installed("pdftools")
  pdf_path <- find_local_pdf("{pdf_pattern}")
  skip_if(is.null(pdf_path), "{mdufa_period} PDF not available locally")

  data <- suppressWarnings(extract_report(pdf_path, mdufa_period = "{mdufa_period}"))

')

  # Generate assertions for each row
  # Use different assertions for NA vs non-NA values
  assertions <- combined_results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      assertion = if (is.na(value)) {
        glue::glue(
          '# Table {table_number} | {organization} | {substr(performance_metric, 1, 40)}... | FY {fy} = NA
expect_true(is.na(
  data$value[which(data$table_number == "{table_number}" &
                   data$organization == "{organization}" &
                   data$performance_metric == "{performance_metric}" &
                   data$fy == "{fy}")]
))
'
        )
      } else {
        glue::glue(
          '# Table {table_number} | {organization} | {substr(performance_metric, 1, 40)}... | FY {fy}
expect_equal(
  data$value[which(data$table_number == "{table_number}" &
                   data$organization == "{organization}" &
                   data$performance_metric == "{performance_metric}" &
                   data$fy == "{fy}")],
  "{value}"
)
'
        )
      }
    ) |>
    dplyr::pull(.data$assertion) |>
    paste(collapse = "\n")

  # Build nolint end directive separately to avoid lintr seeing it in this file
  nolint_end <- paste0("# nolint", " end")
  footer <- paste0("})\n", nolint_end, "\n")

  # Write file
  writeLines(paste0(header, assertions, footer), output_file)
  message("Generated test file: ", output_file)
  output_file
}

#' Parse Existing Test File for Assertions
#'
#' Extracts table_number, organization, performance_metric, fy, and value
#' from existing expect_equal and expect_true(is.na(...)) assertions.
#'
#' @param test_file Path to existing test file
#' @return Data frame with parsed assertions, or NULL if parsing fails
#' @keywords internal
parse_existing_test_file <- function(test_file) {
  tryCatch(
    {
      lines <- readLines(test_file)
      content <- paste(lines, collapse = "\n")

      # Pattern for expect_equal assertions
      # Captures: table_number, organization, performance_metric, fy, value
      equal_pattern <- paste0(
        'data\\$value\\[data\\$table_number == "([^"]+)" &\\s*',
        'data\\$organization == "([^"]+)" &\\s*',
        'data\\$performance_metric == "([^"]+)" &\\s*',
        'data\\$fy == "([^"]+)"\\]\\s*,\\s*',
        '"([^"]*)"'
      )

      equal_matches <- stringr::str_match_all(content, equal_pattern)[[1]]

      # Pattern for expect_true(is.na(...)) assertions
      na_pattern <- paste0(
        "expect_true\\(is\\.na\\(\\s*",
        'data\\$value\\[data\\$table_number == "([^"]+)" &\\s*',
        'data\\$organization == "([^"]+)" &\\s*',
        'data\\$performance_metric == "([^"]+)" &\\s*',
        'data\\$fy == "([^"]+)"\\]'
      )

      na_matches <- stringr::str_match_all(content, na_pattern)[[1]]

      # Build data frames
      results <- NULL


      if (nrow(equal_matches) > 0) {
        equal_df <- data.frame(
          table_number = equal_matches[, 2],
          organization = equal_matches[, 3],
          performance_metric = equal_matches[, 4],
          fy = equal_matches[, 5],
          value = equal_matches[, 6],
          stringsAsFactors = FALSE
        )
        results <- equal_df
      }

      if (nrow(na_matches) > 0) {
        na_df <- data.frame(
          table_number = na_matches[, 2],
          organization = na_matches[, 3],
          performance_metric = na_matches[, 4],
          fy = na_matches[, 5],
          value = NA_character_,
          stringsAsFactors = FALSE
        )
        results <- if (is.null(results)) na_df else dplyr::bind_rows(results, na_df)
      }

      results
    },
    error = function(e) {
      message("Warning: Could not parse existing test file: ", e$message)
      NULL
    }
  )
}
