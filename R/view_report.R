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

  # Get word-level data with coordinates
  all_page_data <- pdftools::pdf_data(pdf_path)
  page_data <- all_page_data[[page]]

  # Get page dimensions
  pdf_info <- pdftools::pdf_pagesize(pdf_path)
  page_width <- pdf_info$width[page]
  page_height <- pdf_info$height[page]

  # Find the table title to get table boundaries
  table_pattern <- paste0("^", gsub("\\.", "\\\\.", table_num), "$")
  table_words <- page_data[grepl(table_pattern, page_data$text), ]

  # Find the FY column header to get x-coordinate for the correct column
  fy_pattern <- paste0("^", fy, "$")
  fy_header <- page_data[grepl(fy_pattern, page_data$text), ]

  # Find the value in the page data
  # Clean value for matching (handle percentages, parentheses, etc.)
  value_clean <- trimws(value)
  value_matches <- page_data[page_data$text == value_clean, ]

  # If exact match fails, try first word of value
  if (nrow(value_matches) == 0 && !is.na(value)) {
    first_word <- strsplit(value_clean, "\\s+")[[1]][1]
    value_matches <- page_data[page_data$text == first_word, ]
  }

  # Find metric words for row identification - look for unique/distinctive words
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
    word_order <- c(distinctive, metric_words[!metric_words %in% distinctive])

    for (word in word_order) {
      matches <- page_data[page_data$text == word, ]
      if (nrow(matches) > 0) {
        # If we have FY header, prefer metric matches that are below the header
        if (nrow(fy_header) > 0) {
          header_y <- min(fy_header$y)
          matches <- matches[matches$y > header_y, ]
        }
        # If only one match, use it; otherwise keep looking for unique match
        if (nrow(matches) == 1) {
          metric_y <- matches$y[1]
          break
        } else if (nrow(matches) > 0 && is.null(metric_y)) {
          # Store first match as fallback
          metric_y <- matches$y[1]
        }
      }
    }
  }

  # Render page as image
  img <- magick::image_read_pdf(pdf_path, pages = page, density = dpi)

  # Calculate scale factor (pdf coordinates are in points, 72 per inch)
  scale <- dpi / 72

  # Draw highlights
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
