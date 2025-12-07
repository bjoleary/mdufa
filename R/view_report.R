#' Find PDF Index from Printed Page Number
#'
#' PDFs may have printed page numbers (e.g., "Page 210 of 313") that differ
#' from the PDF index due to cover pages. This function finds the PDF index
#' that contains the specified printed page number.
#'
#' @param pdf_path Path to the PDF file
#' @param printed_page The printed page number to find
#'
#' @return The PDF index (1-based) containing the printed page, or NA if not
#'
#' @keywords internal
find_pdf_index <- function(pdf_path, printed_page) {
  pages <- pdftools::pdf_text(pdf_path)
  pattern <- paste0("Page ", printed_page, " of \\d+")

  for (i in seq_along(pages)) {
    if (grepl(pattern, pages[i])) {
      return(i)
    }
  }
  NA_integer_
}

#' View MDUFA Report Page
#'
#' Opens a MDUFA report PDF to a specific page. Extracts the requested page
#' (plus one page of context) to a temporary PDF file to ensure the correct
#' page is displayed.
#'
#' @param pdf_path Path to the PDF file
#' @param page Page number to open to
#' @param search_text Optional text to search for (printed to console)
#' @param table_number Optional table number - will look up the page
#'   automatically
#' @param context Number of pages before/after to include (default 1)
#'
#' @return Invisibly returns the page number opened
#' @export
#'
#' @examples
#' \dontrun{
#' # Open to specific page
#' view_report("path/to/report.pdf", page = 231)
#'
#' # Open to table and highlight metric
#' view_report("path/to/report.pdf",
#'   table_number = "11.5",
#'   search_text = "Average FDA days"
#' )
#' }
view_report <- function(pdf_path,
                        page = NULL,
                        search_text = NULL,
                        table_number = NULL,
                        context = 1) {
  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required. Please install it.")
  }

  # If table_number provided, find the page
  if (!is.null(table_number) && is.null(page)) {
    page <- find_table_page(pdf_path, table_number)
    if (is.na(page)) {
      stop("Table ", table_number, " not found in PDF")
    }
    message("Table ", table_number, " found on page ", page)
  }

  if (is.null(page)) {
    page <- 1
  }

  # Get total pages
  pdf_info <- pdftools::pdf_info(pdf_path)
  total_pages <- pdf_info$pages

  # Calculate page range with context
  start_page <- max(1, page - context)
  end_page <- min(total_pages, page + context)



  # Extract pages to temp file using qpdf
  # (use /tmp so file persists after R exits)
  temp_pdf <- file.path(
    "/tmp",
    paste0("mdufa_page", page, "_", format(Sys.time(), "%H%M%S"), ".pdf")
  )

  # Use qpdf to extract pages
  qpdf_cmd <- paste(
    "qpdf",
    shQuote(normalizePath(pdf_path)),
    "--pages . ", paste0(start_page, "-", end_page), " --",
    shQuote(temp_pdf)
  )
  system(qpdf_cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (!file.exists(temp_pdf)) {
    stop("Failed to extract PDF pages with qpdf")
  }

  message("Opening page ", page, " of ", basename(pdf_path))
  message("(Extracted pages ", start_page, "-", end_page, " to temp file)")

  # Open the extracted PDF
  system(paste("open", shQuote(temp_pdf)), wait = FALSE)

  # If search_text provided, print it for manual Cmd+F
  if (!is.null(search_text)) {
    message("Search for: ", search_text)
    message("(Use Cmd+F in Preview to find)")
  }

  invisible(page)
}

#' Find Table Page in PDF
#'
#' Searches a PDF for a specific table number and returns the page number.
#'
#' @param pdf_path Path to the PDF file
#' @param table_number Table number to find (e.g., "11.5")
#'
#' @return Page number where table is found, or NA if not found
#' @export
#'
#' @examples
#' \dontrun{
#' find_table_page("path/to/report.pdf", "11.5")
#' }
find_table_page <- function(pdf_path, table_number) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required. Please install it.")
  }

  raw_text <- pdftools::pdf_text(pdf_path)

  # Build pattern to match table number at start of table title

  pattern <- paste0("Table\\s+", gsub("\\.", "\\\\.", table_number), "\\b")

  for (i in seq_along(raw_text)) {
    if (grepl(pattern, raw_text[[i]])) {
      return(i)
    }
  }

  NA_integer_
}

#' View Report Row
#'
#' Opens a MDUFA report to the page containing a specific data row,
#' highlighting the metric name.
#'
#' @param row A single row from mdufa3, mdufa4, or mdufa5 dataset
#' @param pdf_dir Directory containing PDF reports. Defaults to
#'   "data-raw/pdf_reports" relative to working directory.
#'
#' @return Invisibly returns the page number opened
#' @export
#'
#' @examples
#' \dontrun{
#' # View the source for a specific row
#' view_report_row(mdufa4[5208, ])
#' }
view_report_row <- function(row, pdf_dir = "data-raw/pdf_reports") {
  if (nrow(row) != 1) {
    stop("row must be a single row (data frame with 1 row)")
  }

  # Extract page number
  page <- as.integer(row$page)
  if (is.na(page)) {
    stop("Row has no page number")
  }

  # Find the PDF file based on report metadata
  if ("report_date" %in% names(row)) {
    report_date <- as.character(row$report_date)
  } else {
    stop("Row has no report_date - cannot identify PDF")
  }

  if ("report_mdufa_period" %in% names(row)) {
    mdufa_period <- row$report_mdufa_period
    mdufa_num <- gsub("MDUFA\\s*", "", mdufa_period)
    mdufa_num <- switch(mdufa_num,
      "III" = "3",
      "IV" = "4",
      "V" = "5",
      mdufa_num
    )
  } else {
    stop("Row has no report_mdufa_period - cannot identify PDF")
  }

  # Find matching PDF
  pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)
  pdf_files <- list.files(pdf_dir, pattern = pdf_pattern, full.names = TRUE)

  if (length(pdf_files) == 0) {
    stop("No PDF found matching pattern: ", pdf_pattern, " in ", pdf_dir)
  }

  pdf_path <- pdf_files[1]

  # Get search text from performance_metric
  search_text <- row$performance_metric
  if (is.na(search_text) || search_text == "") {
    search_text <- NULL
  }

  message("Opening: ", basename(pdf_path), " page ", page)
  if (!is.null(search_text)) {
    message("Searching for: ", search_text)
  }

  view_report(pdf_path, page = page, search_text = search_text)
}

#' Verify Data Row
#'
#' Creates an annotated image of a PDF page with the specific data cell
#' highlighted. Opens the image directly - no scrolling or searching needed.
#'
#' @param row A single row from mdufa3, mdufa4, or mdufa5 dataset
#' @param pdf_dir Directory containing PDF reports. Defaults to
#'   "data-raw/pdf_reports" relative to working directory.
#' @param dpi Resolution for rendering (default 150)
#'
#' @return Invisibly returns the path to the generated image
#' @export
#'
#' @examples
#' \dontrun{
#' # Verify a specific data row with highlighted value
#' verify_row(mdufa4[5208, ])
#' }
verify_row <- function(row, pdf_dir = "data-raw/pdf_reports", dpi = 150) {
  if (nrow(row) != 1) {
    stop("row must be a single row (data frame with 1 row)")
  }

  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The 'pdftools' package is required. Please install it.")
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("The 'magick' package is required. Please install it.")
  }

  # Extract row metadata
  page <- as.integer(row$page)
  if (is.na(page)) {
    stop("Row has no page number")
  }

  value <- as.character(row$value)
  metric <- as.character(row$performance_metric)
  fy <- as.character(row$fy)
  table_num <- as.character(row$table_number)

  # Find the PDF file
  if ("report_date" %in% names(row)) {
    report_date <- as.character(row$report_date)
  } else {
    stop("Row has no report_date - cannot identify PDF")
  }

  if ("report_mdufa_period" %in% names(row)) {
    mdufa_period <- row$report_mdufa_period
    mdufa_num <- gsub("MDUFA\\s*", "", mdufa_period)
    mdufa_num <- switch(mdufa_num,
      "III" = "3",
      "IV" = "4",
      "V" = "5",
      mdufa_num
    )
  } else {
    stop("Row has no report_mdufa_period - cannot identify PDF")
  }

  pdf_pattern <- paste0("mdufa-", mdufa_num, "_", report_date)
  pdf_files <- list.files(pdf_dir, pattern = pdf_pattern, full.names = TRUE)

  if (length(pdf_files) == 0) {
    stop("No PDF found matching pattern: ", pdf_pattern, " in ", pdf_dir)
  }

  pdf_path <- pdf_files[1]

  # Find actual PDF index from printed page number
  # The data stores printed page numbers, but we need PDF index to render
  pdf_index <- find_pdf_index(pdf_path, page)
  if (is.na(pdf_index)) {
    # Fall back to using page directly if no printed page found
    pdf_index <- page
  }

  # Get word-level data with coordinates
  all_page_data <- pdftools::pdf_data(pdf_path)
  page_data <- all_page_data[[pdf_index]]

  # Get page dimensions
  pdf_info <- pdftools::pdf_pagesize(pdf_path)
  page_width <- pdf_info$width[pdf_index]
  page_height <- pdf_info$height[pdf_index]


  # Find the table title to establish table boundaries
  # Look for "Table X.Y" pattern to find where our table starts
  table_title_rows <- page_data[page_data$text == "Table", ]


  # Find the specific table number after "Table" word
  # Match table number at START of text (e.g., "2.2" in "2.2.DCTD")
  # but not as suffix (e.g., don't match "12.2" when looking for "2.2")
  table_num_escaped <- gsub("\\.", "\\\\.", table_num)
  table_num_pattern <- paste0("^", table_num_escaped, "([^0-9]|$)")
  table_num_matches <- page_data[grepl(table_num_pattern, page_data$text), ]

  # Establish table y-boundaries
  table_start_y <- 0
  table_end_y <- Inf

  if (nrow(table_num_matches) > 0) {
    # Table starts at the "Table X.Y" header
    table_start_y <- min(table_num_matches$y) - 20 # Small buffer above

    # Find next table header to establish end boundary
    # Look for "Table" word positions, then find table numbers near them
    table_words <- page_data[page_data$text == "Table", ]
    if (nrow(table_words) > 0) {
      # Find table headers that come after our table
      min_y <- min(table_num_matches$y) + 20
      tables_after_y <- table_words$y[table_words$y > min_y]
      if (length(tables_after_y) > 0) {
        table_end_y <- min(tables_after_y) - 10 # End before next table
      }
    }
  }

  # Find the FY column header within table boundaries
  fy_pattern <- paste0("^", fy, "$")
  fy_header <- page_data[
    grepl(fy_pattern, page_data$text) &
      page_data$y >= table_start_y &
      page_data$y <= table_end_y,
  ]

  # Find the value in the page data within table boundaries
  # Clean value for matching (handle percentages, parentheses, etc.)
  # Skip matching for NA/empty values - they'll be handled separately
  value_clean <- trimws(value)
  value_is_empty <- is.na(value) || value == "" || value == "NA" ||
    value == "<NA>" || tolower(value) == "na"

  value_matches <- data.frame()
  if (!value_is_empty) {
    value_matches <- page_data[
      page_data$text == value_clean &
        page_data$y >= table_start_y &
        page_data$y <= table_end_y,
    ]

    # If exact match fails, try first word of value
    if (nrow(value_matches) == 0) {
      first_word <- strsplit(value_clean, "\\s+")[[1]][1]
      value_matches <- page_data[
        page_data$text == first_word &
          page_data$y >= table_start_y &
          page_data$y <= table_end_y,
      ]
    }
  }

  # Find metric words for row identification - look for unique/distinctive words
  # Constrain search to within table boundaries
  metric_y <- NULL
  if (!is.na(metric)) {
    metric_words <- strsplit(metric, "\\s+")[[1]]
    # Skip common words, prefer longer/more distinctive words
    common_words <- c(
      "Number", "of", "the", "a", "an", "to", "for", "and", "or",
      "with", "in", "on", "by", "Rate", "Average", "Total"
    )
    # Try distinctive words first (longer words, not in common list)
    distinctive <- metric_words[
      !metric_words %in% common_words & nchar(metric_words) > 3
    ]

    # Collect y-positions for all distinctive words
    # Find rows where multiple distinctive words co-occur
    y_tolerance <- 5 # pixels tolerance for same row
    word_y_positions <- list()

    for (word in distinctive) {
      matches <- page_data[
        page_data$text == word &
          page_data$y >= table_start_y &
          page_data$y <= table_end_y,
      ]
      if (nrow(matches) > 0) {
        if (nrow(fy_header) > 0) {
          header_y <- min(fy_header$y)
          matches <- matches[matches$y > header_y, ]
        }
        if (nrow(matches) > 0) {
          word_y_positions[[word]] <- matches$y
        }
      }
    }

    # Find y-position where most distinctive words co-occur
    if (length(word_y_positions) > 0) {
      all_ys <- unlist(word_y_positions)
      unique_ys <- unique(all_ys)

      best_y <- NULL
      best_count <- 0

      for (y in unique_ys) {
        # Count how many words have a match near this y
        count <- sum(sapply(word_y_positions, function(ys) {
          any(abs(ys - y) <= y_tolerance)
        }))
        if (count > best_count) {
          best_count <- count
          best_y <- y
        }
      }

      if (!is.null(best_y)) {
        metric_y <- best_y
      }
    }

    # Fallback: try each word individually
    if (is.null(metric_y)) {
      word_order <- c(distinctive, metric_words[!metric_words %in% distinctive])
      for (word in word_order) {
        matches <- page_data[
          page_data$text == word &
            page_data$y >= table_start_y &
            page_data$y <= table_end_y,
        ]
        if (nrow(matches) > 0) {
          if (nrow(fy_header) > 0) {
            header_y <- min(fy_header$y)
            matches <- matches[matches$y > header_y, ]
          }
          if (nrow(matches) == 1) {
            metric_y <- matches$y[1]
            break
          } else if (nrow(matches) > 0 && is.null(metric_y)) {
            metric_y <- matches$y[1]
          }
        }
      }
    }
  }

  # Render page as image
  img <- magick::image_read_pdf(pdf_path, pages = pdf_index, density = dpi)

  # Calculate scale factor (pdf coordinates are in points, 72 per inch)
  scale <- dpi / 72

  # Draw highlights
  highlight_drawn <- FALSE

  if (nrow(value_matches) > 0) {
    # Use FY column x-position and metric row y-position to find cell
    if (!is.null(metric_y) && nrow(fy_header) > 0) {
      fy_x <- fy_header$x[1]
      # Find value closest to both metric_y (row) and fy_x (column)
      value_matches$y_diff <- abs(value_matches$y - metric_y)
      value_matches$x_diff <- abs(value_matches$x - fy_x)
      # Weight y more heavily since rows are distinct
      value_matches$score <- value_matches$y_diff + (value_matches$x_diff * 0.5)
      value_matches <- value_matches[order(value_matches$score), ]
    } else if (!is.null(metric_y)) {
      # Fall back to just y-matching
      value_matches$y_diff <- abs(value_matches$y - metric_y)
      value_matches <- value_matches[order(value_matches$y_diff), ]
    }

    # Highlight the value (first/best match)
    v <- value_matches[1, ]
    x <- v$x * scale
    y <- v$y * scale
    w <- v$width * scale
    h <- v$height * scale

    # Draw yellow highlight box
    img <- magick::image_draw(img)
    graphics::rect(x - 2, y - 2, x + w + 2, y + h + 2,
      col = grDevices::adjustcolor("yellow", alpha.f = 0.4),
      border = "red", lwd = 2
    )
    grDevices::dev.off()
    highlight_drawn <- TRUE
  }

  # For NA/empty values, highlight cell at metric row and FY column
  if (!highlight_drawn && !is.null(metric_y) && nrow(fy_header) > 0) {
    fy_x <- fy_header$x[1]
    fy_w <- fy_header$width[1]

    # Estimate cell dimensions (use typical cell size)
    cell_w <- max(40, fy_w * 1.5)
    cell_h <- 15

    x <- fy_x * scale
    y <- metric_y * scale
    w <- cell_w * scale
    h <- cell_h * scale

    # Draw red dashed box for empty cell
    img <- magick::image_draw(img)
    graphics::rect(x - 5, y - 5, x + w + 5, y + h + 5,
      col = grDevices::adjustcolor("orange", alpha.f = 0.3),
      border = "red", lwd = 2, lty = 2
    )
    grDevices::dev.off()
  }

  # Add annotation with row info
  label <- paste0(
    "Table ", table_num, " | ",
    ifelse(is.na(metric), "[NA metric]", metric), " | ",
    "FY ", fy, " | Value: ", value
  )

  img <- magick::image_annotate(img, label,
    size = 14, color = "black", boxcolor = "white",
    location = "+10+10"
  )

  # Save and open
  timestamp <- format(Sys.time(), "%H%M%S")
  filename <- paste0("verify_", table_num, "_", fy, "_", timestamp, ".png")
  out_path <- file.path("/tmp", filename)
  magick::image_write(img, out_path)

  message("Table: ", table_num)
  message("Metric: ", ifelse(is.na(metric), "[NA]", metric))
  message("FY: ", fy, " | Value: ", value)
  message("Opening: ", out_path)

  system(paste("open", shQuote(out_path)), wait = FALSE)

  invisible(out_path)
}
