# verification_app.R
# Shiny app for interactive MDUFA extraction verification
# NOT part of the installed package - development use only

#' Generate Single Verification Image
#'
#' Generates annotated PDF image for a single row on-demand.
#'
#' @param row Single row from sample
#' @param row_idx Row index (for naming)
#' @param pdf_path Path to PDF
#' @return List with path and highlight_y position
#' @keywords internal
generate_single_image <- function(row, row_idx, pdf_path) {
  output_dir <- file.path("/tmp", "verify_images")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Use content-based naming to avoid regenerating identical images
  # Hash key fields: table_number, organization, performance_metric, page
  content_key <- paste(
    row$table_number, row$organization, row$performance_metric, row$page,
    sep = "|"
  )
  content_hash <- substr(digest::digest(content_key, algo = "md5"), 1, 12)
  img_path <- file.path(output_dir, paste0("img_", content_hash, ".png"))
  meta_path <- file.path(output_dir, paste0("img_", content_hash, "_meta.rds"))

  # Only generate if not already cached
  if (!file.exists(img_path)) {
    result <- generate_row_image(
      table_number = as.character(row$table_number),
      organization = as.character(row$organization),
      performance_metric = as.character(row$performance_metric),
      page = as.integer(row$page),
      pdf_path = pdf_path,
      output_path = img_path
    )
    # Cache the highlight position
    saveRDS(result$highlight_y, meta_path)
    highlight_y <- result$highlight_y
  } else if (file.exists(meta_path)) {
    highlight_y <- readRDS(meta_path)
  } else {
    highlight_y <- NA
  }

  list(path = img_path, highlight_y = highlight_y)
}

#' Launch Verification App
#'
#' Opens a Shiny app for interactive verification of sampled rows.
#' Shows diff info, PDF page image, and pass/fail buttons.
#'
#' @param sample Sampled rows for verification (from generate_verification_sample)
#' @param pdf_path Path to the PDF being verified
#' @param mdufa_period MDUFA period string
#' @param output_path Where to save results CSV when done
#' @param resume_from Optional path to previous results CSV to resume from
#' @param full_data Full extracted dataset (for showing all FY values)
#' @return Path to results CSV
launch_verification_app <- function(sample,
                                    pdf_path = NULL,
                                    mdufa_period = NULL,
                                    output_path = "/tmp/verification_results.csv",
                                    resume_from = NULL,
                                    full_data = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package required for verification app")
  }

  # Handle resume
  if (!is.null(resume_from) && file.exists(resume_from)) {
    message("Resuming from: ", resume_from)
    # Read all columns as character to avoid type coercion issues
    sample <- utils::read.csv(
      resume_from,
      stringsAsFactors = FALSE,
      colClasses = "character"
    )
    if (is.null(pdf_path) || is.null(mdufa_period)) {
      stop("pdf_path and mdufa_period required when resuming")
    }
  }

  # Images will be generated on-demand (streaming)
  message("Images will load on-demand...")

  # Build UI
  ui <- shiny::fluidPage(
    shiny::titlePanel(paste("MDUFA Verification -", mdufa_period)),

    # CSS for layout
    shiny::tags$style(shiny::HTML("
      .main-panel-image {
        max-height: calc(100vh - 100px);
        overflow: hidden;
        position: relative;
      }
      .main-panel-image img {
        max-height: calc(100vh - 120px);
        width: auto !important;
        object-fit: contain;
        display: block;
      }
      /* Floating values overlay - will be positioned by JavaScript over actual image */
      .values-overlay {
        position: absolute;
        background: rgba(255, 255, 255, 0.95);
        border: 2px solid #333;
        border-radius: 8px;
        padding: 12px 20px;
        z-index: 1000;
        box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        text-align: center;
        cursor: move;
        user-select: none;
      }
      .values-overlay h4 {
        margin: 0 0 8px 0;
        font-size: 13px;
        color: #666;
      }
      .values-table {
        font-family: monospace;
        border-collapse: collapse;
        margin: 0 auto;
      }
      .values-table th {
        font-size: 14px;
        font-weight: normal;
        color: #666;
        padding: 2px 12px;
        border-bottom: 1px solid #ddd;
      }
      .values-table td {
        font-size: 22px;
        font-weight: bold;
        color: #000;
        padding: 6px 12px;
        text-align: center;
      }
      .metric-name {
        font-size: 11px;
        color: #555;
        margin-top: 8px;
        max-width: 500px;
      }
      /* Bold values in sidebar table */
      .sidebar table td:nth-child(2) {
        font-weight: bold;
      }
    ")),

    # JavaScript for positioning and keyboard shortcuts
    shiny::tags$script(shiny::HTML("
      // Function to position overlay centered over the actual image
      function positionOverlay() {
        var overlay = $('.values-overlay');
        var img = $('.main-panel-image img');

        if (overlay.length && img.length && img[0].complete && img.width() > 0) {
          // Get image position relative to container
          var imgPosition = img.position();
          var imgWidth = img.width();

          // Center overlay horizontally over the image
          var overlayWidth = overlay.outerWidth();
          var leftPos = imgPosition.left + (imgWidth / 2) - (overlayWidth / 2);

          // Keep within reasonable bounds
          leftPos = Math.max(0, leftPos);

          overlay.css('left', leftPos + 'px');
        }
      }

      // Position overlay when Shiny updates content
      $(document).on('shiny:value', function(e) {
        if (e.name === 'pdf_image' || e.name === 'values_overlay') {
          setTimeout(positionOverlay, 100);
        }
      });

      // Also reposition on window resize
      $(window).on('resize', function() {
        positionOverlay();
      });

      // Initial positioning after page load
      $(document).ready(function() {
        setTimeout(positionOverlay, 500);
      });

      $(document).on('keydown', function(e) {
        // Don't capture keys when typing in notes field
        if (document.activeElement.tagName === 'INPUT' ||
            document.activeElement.tagName === 'TEXTAREA') {
          if (e.key === 'Escape') {
            document.activeElement.blur();
            e.preventDefault();
          }
          return;
        }

        var key = e.key.toLowerCase();
        if (e.key === 'Enter') key = 'enter';

        switch(key) {
          case 'p':
          case 'enter':
            $('#pass').click();
            e.preventDefault();
            break;
          case 'f':
            $('#fail').click();
            e.preventDefault();
            break;
          case 'g':
            $('#garbage').click();
            e.preventDefault();
            break;
          case 's':
            $('#skip').click();
            e.preventDefault();
            break;
          case 'arrowleft':
          case 'k':
            $('#prev').click();
            e.preventDefault();
            break;
          case 'arrowright':
          case 'j':
          case ' ':
            $('#next_btn').click();
            e.preventDefault();
            break;
          case 'arrowup':
            // Move overlay up
            var overlay = $('.values-overlay');
            if (overlay.length) {
              var currentTop = parseInt(overlay.css('top')) || 0;
              overlay.css('top', Math.max(10, currentTop - 30) + 'px');
            }
            e.preventDefault();
            break;
          case 'arrowdown':
            // Move overlay down
            var overlay = $('.values-overlay');
            if (overlay.length) {
              var currentTop = parseInt(overlay.css('top')) || 0;
              overlay.css('top', (currentTop + 30) + 'px');
            }
            e.preventDefault();
            break;
          case 'n':
            $('#notes').focus();
            e.preventDefault();
            break;
          case 'q':
            $('#done').click();
            e.preventDefault();
            break;
        }
      });

      // Draggable overlay (absolute positioning within container)
      $(document).on('mousedown', '.values-overlay', function(e) {
        var overlay = $(this);
        var container = overlay.closest('.main-panel-image');
        var containerOffset = container.offset();

        // Get current position relative to container
        var overlayLeft = overlay.position().left;
        var overlayTop = overlay.position().top;
        var startMouseX = e.pageX;
        var startMouseY = e.pageY;

        // Remove centering transform once user starts dragging
        overlay.css('transform', 'none');

        $(document).on('mousemove.drag', function(e) {
          var dx = e.pageX - startMouseX;
          var dy = e.pageY - startMouseY;
          overlay.css({
            left: overlayLeft + dx,
            top: overlayTop + dy
          });
        });

        $(document).on('mouseup.drag', function() {
          $(document).off('.drag');
        });
      });
    ")),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Progress"),
        shiny::uiOutput("progress_bar"),
        shiny::textOutput("progress"),
        shiny::textOutput("ci_status"),
        shiny::hr(),
        shiny::h4("Current Row"),
        shiny::tableOutput("row_info"),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::actionButton("pass", "Pass (P)",
              class = "btn-success btn-block"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton("fail", "Fail (F)",
              class = "btn-danger btn-block"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton("garbage", "Garbage (G)",
              class = "btn-secondary btn-block",
              style = "background-color: #6c757d; color: white;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton("skip", "Skip (S)",
              class = "btn-warning btn-block"
            )
          )
        ),
        shiny::hr(),
        shiny::textAreaInput("notes", "Notes (N)", rows = 2),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton("prev", "\u2190 Prev (K)", class = "btn-block")
          ),
          shiny::column(
            6,
            shiny::actionButton("next_btn", "Next (J) \u2192", class = "btn-block")
          )
        ),
        shiny::hr(),
        shiny::actionButton("done", "Finish & Save (Q)", class = "btn-primary btn-block"),
        shiny::hr(),
        shiny::actionButton("add_rows", "+ Add 10 More Rows", class = "btn-info btn-block")
      ),
      shiny::mainPanel(
        width = 9,
        shiny::h4(shiny::textOutput("image_title")),
        shiny::div(
          class = "main-panel-image",
          shiny::imageOutput("pdf_image", height = "auto"),
          # Floating values overlay - inside container for relative positioning
          shiny::uiOutput("values_overlay")
        )
      )
    )
  )

  # Build server
  server <- function(input, output, session) {
    # Initialize results with status and notes columns if not present
    init_sample <- sample
    if (!"status" %in% names(init_sample)) {
      init_sample$status <- NA_character_
    }
    if (!"notes" %in% names(init_sample)) {
      init_sample$notes <- ""
    }

    # Find first unreviewed row (for resume functionality)
    first_unreviewed <- which(is.na(init_sample$status))[1]
    if (is.na(first_unreviewed)) first_unreviewed <- 1

    # Reactive state - start at first unreviewed row
    current_idx <- shiny::reactiveVal(first_unreviewed)
    results <- shiny::reactiveVal(init_sample)

    # Progress bar
    output$progress_bar <- shiny::renderUI({
      r <- results()
      done <- sum(!is.na(r$status))
      total <- nrow(r)
      pct <- round(done / total * 100)

      shiny::div(
        style = "margin-bottom: 8px;",
        shiny::div(
          style = paste0(
            "background: #e0e0e0; border-radius: 4px; height: 20px; ",
            "position: relative; overflow: hidden;"
          ),
          shiny::div(
            style = paste0(
              "background: #28a745; height: 100%; width: ", pct, "%; ",
              "transition: width 0.3s ease;"
            )
          ),
          shiny::div(
            style = paste0(
              "position: absolute; top: 0; left: 0; right: 0; ",
              "text-align: center; line-height: 20px; font-size: 12px; ",
              "font-weight: bold; color: ", ifelse(pct > 50, "white", "#333"), ";"
            ),
            paste0(pct, "%")
          )
        )
      )
    })

    # Progress display
    output$progress <- shiny::renderText({
      r <- results()
      done <- sum(!is.na(r$status))
      paste0("Row ", current_idx(), " of ", nrow(r), " | ", done, " completed")
    })

    # CI status display - show agreement rate with full Wilson CI
    # Garbage rows are excluded from agreement calculation
    output$ci_status <- shiny::renderText({
      r <- results()
      pass_count <- sum(r$status == "pass", na.rm = TRUE)
      fail_count <- sum(r$status == "fail", na.rm = TRUE)
      garbage_count <- sum(r$status == "garbage", na.rm = TRUE)
      n <- pass_count + fail_count # Garbage not included in agreement

      garbage_note <- if (garbage_count > 0) {
        paste0(" [", garbage_count, " garbage]")
      } else {
        ""
      }

      if (n == 0) {
        return(paste0("Agreement: -- (no results yet)", garbage_note))
      }

      # Calculate Wilson score CI
      z <- 1.96
      z_sq <- z^2
      p <- pass_count / n

      denom <- 1 + z_sq / n
      center <- (p + z_sq / (2 * n)) / denom
      margin <- z * sqrt(p * (1 - p) / n + z_sq / (4 * n^2)) / denom

      lb <- center - margin
      ub <- center + margin

      lb_pct <- round(lb * 100, 1)
      ub_pct <- round(ub * 100, 1)
      rate_pct <- round(p * 100, 1)

      # Calculate how many more passes needed for LB > 90%
      # Iteratively add passes until Wilson LB exceeds 0.90
      calc_wilson_lb <- function(passes, fails) {
        total <- passes + fails
        if (total == 0) {
          return(0)
        }
        p_hat <- passes / total
        denom <- 1 + z_sq / total
        center <- (p_hat + z_sq / (2 * total)) / denom
        margin <- z * sqrt(p_hat * (1 - p_hat) / total + z_sq / (4 * total^2)) / denom
        center - margin
      }

      needed <- 0
      test_passes <- pass_count
      while (calc_wilson_lb(test_passes, fail_count) <= 0.90 && needed < 500) {
        test_passes <- test_passes + 1
        needed <- needed + 1
      }

      # Build status message
      status_prefix <- if (fail_count > 0) {
        paste0("FAIL(", fail_count, ") ")
      } else {
        ""
      }

      if (lb > 0.90 && fail_count == 0) {
        paste0(status_prefix, "Agreement: ", rate_pct, "% (", lb_pct, "-", ub_pct, "%) COMPLETE!", garbage_note)
      } else if (needed >= 500) {
        paste0(status_prefix, "Agreement: ", rate_pct, "% (", lb_pct, "-", ub_pct, "%) - too many failures", garbage_note)
      } else {
        paste0(status_prefix, "Agreement: ", rate_pct, "% (", lb_pct, "-", ub_pct, "%) need ~", needed, " more", garbage_note)
      }
    })

    # Current row info - show all FY values for the metric
    output$row_info <- shiny::renderTable(
      {
        row <- results()[current_idx(), ]

        # Get all FY values for this metric from full_data
        fy_values <- ""
        if (!is.null(full_data)) {
          metric_rows <- full_data |>
            dplyr::filter(
              .data$table_number == row$table_number,
              .data$organization == row$organization,
              .data$performance_metric == row$performance_metric
            ) |>
            dplyr::arrange(.data$fy)

          if (nrow(metric_rows) > 0) {
            fy_values <- paste(
              paste0(metric_rows$fy, ": ", metric_rows$value),
              collapse = "\n"
            )
          }
        }

        # If no full_data, just show the single FY
        if (fy_values == "") {
          fy_values <- paste0(row$fy, ": ", row$value)
        }

        # Get report metadata if available
        # Extract date from pdf_path if not in row data
        report_date <- if ("report_date" %in% names(row) && !is.na(row$report_date)) {
          as.character(row$report_date)
        } else {
          # Try to extract from pdf_path (format: mdufa-X_YYYY-MM-DD_...)
          date_match <- stringr::str_extract(basename(pdf_path), "\\d{4}-\\d{2}-\\d{2}")
          if (!is.na(date_match)) date_match else "-"
        }
        report_period <- if ("report_mdufa_period" %in% names(row) && !is.na(row$report_mdufa_period)) {
          as.character(row$report_mdufa_period)
        } else {
          mdufa_period
        }

        data.frame(
          Field = c("Report", "Period", "Table", "Org", "Page", "Metric", "Values", "Status"),
          Value = c(
            report_date,
            report_period,
            as.character(row$table_number),
            as.character(row$organization),
            as.character(row$page),
            substr(as.character(row$performance_metric), 1, 40),
            fy_values,
            ifelse(is.na(row$status), "-", row$status)
          )
        )
      },
      striped = TRUE,
      hover = TRUE,
      width = "100%"
    )

    # Image title
    output$image_title <- shiny::renderText({
      row <- results()[current_idx(), ]
      paste0(
        "Page ", row$page, " - Table ", row$table_number,
        " (", row$organization, ")"
      )
    })

    # Store current highlight position for overlay
    current_highlight_y <- shiny::reactiveVal(NA)

    # PDF image display - generated on-demand
    output$pdf_image <- shiny::renderImage(
      {
        row <- results()[current_idx(), ]
        img_result <- generate_single_image(row, current_idx(), pdf_path)
        current_highlight_y(img_result$highlight_y)
        list(
          src = img_result$path,
          contentType = "image/png",
          width = "100%"
        )
      },
      deleteFile = FALSE
    )

    # Floating values overlay - positioned dynamically above highlight
    output$values_overlay <- shiny::renderUI({
      row <- results()[current_idx(), ]

      # Get all FY values for this metric
      fy_rows <- NULL
      if (!is.null(full_data)) {
        fy_rows <- full_data |>
          dplyr::filter(
            .data$table_number == row$table_number,
            .data$organization == row$organization,
            .data$performance_metric == row$performance_metric
          ) |>
          dplyr::arrange(.data$fy)
      }

      # Fall back to single row if no full_data
      if (is.null(fy_rows) || nrow(fy_rows) == 0) {
        fy_rows <- data.frame(fy = row$fy, value = row$value)
      }

      # Build horizontal table: FY headers on top, values below
      header_cells <- lapply(fy_rows$fy, function(fy) {
        shiny::tags$th(fy)
      })
      value_cells <- lapply(fy_rows$value, function(val) {
        shiny::tags$td(val)
      })

      # Calculate dynamic position based on highlight_y
      # highlight_y is in pixels (at 150 DPI rendering)
      # Position overlay above or below the highlight row depending on position
      highlight_y <- current_highlight_y()
      if (!is.na(highlight_y) && highlight_y > 0) {
        # If row is near top of page, put overlay below; otherwise above
        # Threshold: 280px (overlay is ~250px tall, need room above)
        if (highlight_y < 280) {
          # Put overlay below the highlight row
          top_px <- highlight_y + 60 # Below the highlight box
        } else {
          # Put overlay above the highlight row
          # Overlay is ~250px tall, plus ~20px margin
          top_px <- highlight_y - 270
        }
        top_position <- paste0(max(10, top_px), "px")
      } else {
        # Default to 40% if no highlight info
        top_position <- "40%"
      }

      shiny::div(
        class = "values-overlay",
        style = paste0("top: ", top_position, ";"),
        shiny::tags$h4(paste0("Table ", row$table_number, " | ", row$organization)),
        shiny::tags$table(
          class = "values-table",
          shiny::tags$thead(shiny::tags$tr(header_cells)),
          shiny::tags$tbody(shiny::tags$tr(value_cells))
        ),
        shiny::div(
          class = "metric-name",
          row$performance_metric
        )
      )
    })

    # Navigation
    shiny::observeEvent(input$prev, {
      if (current_idx() > 1) {
        current_idx(current_idx() - 1)
      }
    })

    shiny::observeEvent(input$next_btn, {
      if (current_idx() < nrow(results())) {
        current_idx(current_idx() + 1)
      }
    })

    # Record results helper
    record_result <- function(status) {
      r <- results()
      r$status[current_idx()] <- status
      r$notes[current_idx()] <- input$notes
      results(r)
      shiny::updateTextAreaInput(session, "notes", value = "")

      # Auto-save after each action
      utils::write.csv(r, output_path, row.names = FALSE)

      # Move to next row
      if (current_idx() < nrow(r)) {
        current_idx(current_idx() + 1)
      }
    }

    # Helper to add replacement sample when garbage is marked
    add_replacement_sample <- function() {
      if (is.null(full_data)) {
        return()
      }

      r <- results()
      # Key columns that identify unique rows
      key_cols <- c("table_number", "organization", "performance_metric", "fy")

      # Find rows in full_data not already in results
      existing_keys <- r[, key_cols, drop = FALSE]
      candidates <- full_data |>
        dplyr::anti_join(existing_keys, by = key_cols) |>
        dplyr::filter(!is.na(page), !is.na(value))

      if (nrow(candidates) == 0) {
        message("No replacement candidates available")
        return()
      }

      # Sample one replacement row
      new_row <- candidates |> dplyr::slice_sample(n = 1)
      new_row$status <- NA_character_
      new_row$notes <- ""
      new_row$sample_category <- "replacement"

      # Convert all columns to character to match CSV-loaded data
      new_row <- new_row |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      # Add to results
      r <- dplyr::bind_rows(r, new_row)
      results(r)

      # Pre-generate image for new row in background
      new_idx <- nrow(r)
      later::later(function() {
        tryCatch(
          {
            generate_single_image(new_row, new_idx, pdf_path)
            message("Generated image for replacement row ", new_idx)
          },
          error = function(e) {
            message("Failed to generate image: ", e$message)
          }
        )
      }, delay = 0.1)

      message("Added replacement sample (row ", new_idx, ")")
    }

    shiny::observeEvent(input$pass, record_result("pass"))
    shiny::observeEvent(input$fail, record_result("fail"))
    shiny::observeEvent(input$garbage, {
      record_result("garbage")
      add_replacement_sample()
    })
    shiny::observeEvent(input$skip, record_result("skip"))

    # Add more rows button
    shiny::observeEvent(input$add_rows, {
      if (is.null(full_data)) {
        shiny::showNotification("Cannot add rows: full_data not provided", type = "error")
        return()
      }

      r <- results()
      key_cols <- c("table_number", "organization", "performance_metric", "fy")

      # Find rows in full_data not already in results
      existing_keys <- r[, key_cols, drop = FALSE]
      candidates <- full_data |>
        dplyr::anti_join(existing_keys, by = key_cols) |>
        dplyr::filter(!is.na(page), !is.na(value))

      if (nrow(candidates) == 0) {
        shiny::showNotification("No more candidates available", type = "warning")
        return()
      }

      # Sample up to 10 new rows
      n_to_add <- min(10, nrow(candidates))
      new_rows <- candidates |> dplyr::slice_sample(n = n_to_add)
      new_rows$status <- NA_character_
      new_rows$notes <- ""
      new_rows$sample_category <- "added"

      # Convert all columns to character to match CSV-loaded data
      new_rows <- new_rows |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

      # Add to results
      r <- dplyr::bind_rows(r, new_rows)
      results(r)

      # Save immediately
      utils::write.csv(r, output_path, row.names = FALSE)

      shiny::showNotification(
        paste0("Added ", n_to_add, " rows (total: ", nrow(r), ")"),
        type = "message"
      )
      message("Added ", n_to_add, " more rows to verification sample")
    })

    # Save and exit
    shiny::observeEvent(input$done, {
      r <- results()
      utils::write.csv(r, output_path, row.names = FALSE)
      message("Results saved to: ", output_path)

      # Print summary
      pass_count <- sum(r$status == "pass", na.rm = TRUE)
      fail_count <- sum(r$status == "fail", na.rm = TRUE)
      garbage_count <- sum(r$status == "garbage", na.rm = TRUE)
      skip_count <- sum(r$status == "skip", na.rm = TRUE)

      message("\n=== Verification Summary ===")
      message("Passed: ", pass_count)
      message("Failed: ", fail_count)
      message("Garbage: ", garbage_count, " (not counted in agreement)")
      message("Skipped: ", skip_count)
      message("Not reviewed: ", sum(is.na(r$status)))

      if (fail_count > 0) {
        message("\nSTATUS: FAILED - Review failures and fix extraction code")
      } else if (pass_count > 0) {
        z_sq <- 3.84
        lb <- pass_count / (pass_count + z_sq)
        if (lb > 0.90) {
          message("\nSTATUS: COMPLETE - LB = ", round(lb * 100, 2), "% > 90%")

          # Auto-generate test file on completion
          message("\n=== Generating Verification Outputs ===")
          tryCatch(
            {
              test_file <- generate_test_file(
                results = r,
                pdf_path = pdf_path,
                mdufa_period = mdufa_period,
                full_data = full_data
              )
              message("Test file: ", test_file)
            },
            error = function(e) {
              message("Error generating test file: ", e$message)
            }
          )

          # Write garbage rows to known issues markdown file
          if (garbage_count > 0) {
            garbage_rows <- dplyr::filter(r, .data$status == "garbage")
            message("\n=== Garbage Rows (", garbage_count, ") ===")

            # Generate known issues markdown file
            report_date <- stringr::str_extract(
              basename(pdf_path), "\\d{4}-\\d{2}-\\d{2}"
            )
            mdufa_num <- switch(mdufa_period,
              "MDUFA III" = "3",
              "MDUFA IV" = "4",
              "MDUFA V" = "5",
              "X"
            )
            issues_file <- file.path(
              "local",
              paste0("known_issues_mdufa", mdufa_num, "_", report_date, ".md")
            )

            # Build markdown content
            md_lines <- c(
              paste0("# Garbage Rows: ", mdufa_period, " ", report_date),
              "",
              paste0("Generated: ", Sys.Date()),
              "",
              paste0(
                "During verification, ", garbage_count,
                " rows were identified as garbage data. ",
                "These may need to be extracted better or removed."
              ),
              "",
              "## Garbage Rows",
              ""
            )

            # Add each garbage row
            for (i in seq_len(nrow(garbage_rows))) {
              row <- garbage_rows[i, ]
              md_lines <- c(
                md_lines,
                paste0("### Row ", i),
                "",
                paste0("- **Page**: ", row$page),
                paste0("- **Table**: ", row$table_number),
                paste0("- **Organization**: ", row$organization),
                paste0("- **Metric**: ", row$performance_metric),
                paste0("- **Value**: ", row$value),
                paste0("- **FY**: ", row$fy),
                if (!is.na(row$notes) && row$notes != "") {
                  paste0("- **Notes**: ", row$notes)
                } else {
                  NULL
                },
                ""
              )
              message(
                "  - Page ", row$page, " | Table ", row$table_number,
                " | ", row$performance_metric
              )
            }

            writeLines(md_lines, issues_file)
            message("Known issues file: ", issues_file)
          }
        } else {
          message("\nSTATUS: CONTINUE - Need more samples for 90% LB")
        }
      }

      shiny::stopApp()
    })

    # Auto-save on session end
    session$onSessionEnded(function() {
      r <- shiny::isolate(results())
      utils::write.csv(r, output_path, row.names = FALSE)
      message("Auto-saved results to: ", output_path)
    })
  }

  # Run the app
  shiny::runApp(shiny::shinyApp(ui, server))

  # Return output path
  output_path
}
