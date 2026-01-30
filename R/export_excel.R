#' Export to Excel
#'
#' @param data The data to export. Must have identical column names to
#'   \code{colnames(mdufa::quarterly_performance)}.
#' @param filepath The \code{.xlsx} file to write to. Existing files with the
#'   same name will not be overwritten.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' filepath <- tempfile(fileext = ".xlsx")
#' export_excel(mdufa::quarterly_performance, filepath)
#' }
export_excel <- function(data, filepath) {
  chk::chk_ext(
    x = tolower(filepath),
    ext = "xlsx"
  )
  chk::check_names(
    x = data,
    names =
      c(
        "report_description",
        "report_link",
        "report_date",
        "report_mdufa_period",
        "source",
        "page",
        "table_number",
        "organization",
        "program",
        "table_title",
        "metric_type",
        "performance_metric",
        "fy",
        "value"
      ),
    exclusive = TRUE
  )

  data <-
    data |>
    dplyr::filter(!is.na(.data$metric_type))
  workbook <- openxlsx::createWorkbook()
  # Make a readme -----
  ## Store session info
  session <- utils::sessionInfo()
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "readme",
    tabColour = "yellow",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        "README",
        # Right side header
        ""
      ),
    footer =
      c(
        # Left side footer:
        "",
        # Center footer:
        "Page &[Page] of &[Pages]",
        # Right side footer
        ""
      ),
    paperSize = 1, # Letter paper
    orientation = "portrait",
    gridLines = FALSE
  )
  text_readme <-
    c(
      paste0(
        "This workbook contains data sourced from FDA's Medical Device User ",
        "Fee Ammendments reports, which are available at ",
        "https://www.fda.gov/industry/medical-device-user-fee-",
        "amendments-mdufa/mdufa-reports",
        ". The data was scraped from these PDF reports using the \"mdufa\" R ",
        "package, an open source tool developed by Brendan O'Leary and ",
        "available at ",
        "https://github.com/bjoleary/mdufa",
        ". "
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "Data in a \"tidy\" format (see ",
        "https://vita.had.co.nz/papers/tidy-data.pdf",
        ") is available in the \"data\" sheet. In this sheet, the \"value\" ",
        "field contains the character string extracted from the associated ",
        "PDF MDUFA report. The \"value_formatted\" field ",
        "includes the numeric version of percent metrics and uses ",
        "metric-specific cell formatting. ",
        "For example, integer metrics will ",
        "be formatted as numbers that do not include decimal places in the ",
        "\"value_formatted\" field while other numeric metrics will be ",
        "formatted as numbers with decimal places."
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "Subsequent sheets filter the data from the \"data\" sheet ",
        "by type (integer metrics, percentage metrics, etc.) and pivot ",
        "it by year. The values in these sheets are from the ",
        "\"value_formatted\" field in the \"data\" sheet."
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "\"Text\" metrics are typically descriptions of the MDUFA performance ",
        "goals themselves (e.g. \"90% Within 320 FDA Days\"). Because of the ",
        "way these values break across lines within table cells in the PDF ",
        "reports, they are more difficult to retrieve and may be more likely ",
        "to be incomplete or to be inaccurate. Similarly, some tables in the ",
        "reports have footnotes, and they are not included in this ",
        "dataset. "
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "Aside from some spot-checks, the data provided by the mdufa R ",
        "package and included in this workbook has had limited verification ",
        "and may be inaccurate. Use this information at your own risk and ",
        "verify information using the reports provided directly by FDA. ",
        "To facilitate this, each data point provided includes information ",
        "about its source, including a link to the FDA report from which it ",
        "came, the relevant page number, and more. "
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "If you find a problem in this dataset, please report it here: ",
        "https://github.com/bjoleary/mdufa/issues. "
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      rep("*", 80) |> paste0(collapse = ""),
      paste0(
        "Please carefully read the \"license\" tab for additional ",
        "important information."
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      rep("*", 80) |> paste0(collapse = ""),
      "",
      "Session Info:",
      "",
      session$R.version$version.string,
      paste("Platform:", session$platform),
      paste("Running under:", session$running),
      "",
      "Package info: ",
      paste0(
        session$otherPkgs$mdufa$Package,
        " version ",
        session$otherPkgs$mdufa$Version
      )
    ) |>
    purrr::simplify()
  openxlsx::writeData(
    wb = workbook,
    sheet = "readme",
    x = text_readme
  )
  openxlsx::addStyle(
    wb = workbook,
    sheet = "readme",
    style = openxlsx::createStyle(fontName = "Courier New"),
    rows = seq_along(text_readme),
    cols = 1
  )
  # Make license sheet -----
  openxlsx::addWorksheet(
    wb = workbook,
    sheetName = "license",
    tabColour = "red",
    header =
      c(
        # Left side header:
        "",
        # Center header:
        "MIT License",
        # Right side header
        ""
      ),
    footer =
      c(
        # Left side footer:
        "",
        # Center footer:
        "Page &[Page] of &[Pages]",
        # Right side footer
        ""
      ),
    paperSize = 1, # Letter paper
    orientation = "portrait",
    gridLines = FALSE
  )
  text_license <-
    c(
      "MIT License",
      "",
      "Copyright (c) 2023 mdufa authors",
      "",
      paste0(
        "Permission is hereby granted, free of charge, to any person ",
        "obtaining a copy of this software and associated documentation files ",
        "(the \"Software\"), to deal in the Software without restriction, ",
        "including without limitation the rights to use, copy, modify, merge, ",
        "publish, distribute, sublicense, and/or sell copies of the Software, ",
        "and to permit persons to whom the Software is furnished to do so, ",
        "subject to the following conditions: "
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "The above copyright notice and this permission notice shall be ",
        "included in all copies or substantial portions of the Software."
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n"),
      "",
      paste0(
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, ",
        "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ",
        "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ",
        "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS ",
        "BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ",
        "ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN ",
        "CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ",
        "SOFTWARE."
      ) |>
        stringr::str_wrap() |>
        stringr::str_split(pattern = "\\n")
    ) |>
    purrr::simplify()
  openxlsx::writeData(
    wb = workbook,
    sheet = "license",
    x = text_license
  )
  openxlsx::addStyle(
    wb = workbook,
    sheet = "license",
    style = openxlsx::createStyle(fontName = "Courier New"),
    rows = seq_along(text_license),
    cols = 1
  )

  # Make data sheet -----
  worksheet <- openxlsx::addWorksheet(workbook, "data")

  ## Define styles -----
  style_text <-
    openxlsx::createStyle(
      numFmt = "TEXT",
      wrapText = TRUE,
      halign = "left",
      valign = "top",
    )
  style_percent <-
    openxlsx::createStyle(
      numFmt = "0.00%",
      halign = "right"
    )
  style_integer <-
    openxlsx::createStyle(
      numFmt = "0.",
      halign = "right"
    )
  style_double <-
    openxlsx::createStyle(
      numFmt = "0.00",
      halign = "right"
    )

  ## Format data -----
  data2 <-
    data |>
    # Create a temporary variable, fp, that is just the numeric characters
    # associated with percent metrics. In other words, strip the percent sign
    # out. But for good measure, in case the data is messier than we expect,
    # strip everything that is not a digit or a decimal.
    dplyr::mutate(
      fp =
        dplyr::case_when(
          .data$metric_type == "percent" ~
            stringr::str_remove_all(
              string = .data$value,
              pattern = stringr::regex("[^0-9.]")
            ),
          TRUE ~ NA_character_
        )
    ) |>
    # Create a second temporary variable, fp_two, that is fp converted to a
    # numeric and divided by 100, since excel expects percentages less than
    # 100% to be expressed as numbers less than 1.
    dplyr::mutate(
      fp_two =
        as.numeric(.data$fp) / 100
    ) |>
    # Now create a new variable, value_formatted, which is just the value
    # for anything that is not a percent and is a character string from fp_two
    # for anything that is a percent.
    dplyr::mutate(
      value_formatted =
        dplyr::case_when(
          .data$metric_type == "percent" ~ as.character(.data$fp_two),
          TRUE ~ .data$value
        )
    ) |>
    dplyr::select(
      -"fp",
      -"fp_two"
    )

  ## Write data -----
  openxlsx::writeDataTable(
    wb = workbook,
    sheet = worksheet,
    x = data2,
    keepNA = TRUE
  )

  ## Apply styles -----
  ### Text -----
  openxlsx::addStyle(
    wb = workbook,
    sheet = worksheet,
    style = style_text,
    rows = which(data2$metric_type == "text") + 1,
    cols = which(colnames(data2) == "value_formatted"),
    gridExpand = FALSE
  )
  ### Percentages -----
  # Unfortunately, this doesn't really work all that great. See
  # https://github.com/ycphs/openxlsx/issues/368. Nonetheless, at least in the
  # online version of Office365, this results in the right style metadata
  # being attached to the cell, and after a cell is opened and closed, the
  # formatting shows up.
  openxlsx::addStyle(
    wb = workbook,
    sheet = worksheet,
    style = style_percent,
    rows = which(data2$metric_type == "percent") + 1,
    cols = which(colnames(data2) == "value_formatted"),
    gridExpand = FALSE
  )
  ### Integers -----
  openxlsx::addStyle(
    wb = workbook,
    sheet = worksheet,
    style = style_integer,
    rows = which(data2$metric_type == "integer") + 1,
    cols = which(colnames(data2) == "value_formatted"),
    gridExpand = FALSE
  )
  ### Doubles -----
  openxlsx::addStyle(
    wb = workbook,
    sheet = worksheet,
    style = style_double,
    rows = which(data2$metric_type == "double") + 1,
    cols = which(colnames(data2) == "value_formatted"),
    gridExpand = FALSE
  )

  metric_types <-
    unique(data2$metric_type)

  for (i in seq_along(metric_types)) {
    current_type <- metric_types[[i]]
    pivot_data <- build_pivot(data2, current_type)
    openxlsx::addWorksheet(
      wb = workbook,
      sheetName = paste0("pivot_", current_type)
    )
    openxlsx::writeDataTable(
      wb = workbook,
      sheet = paste0("pivot_", current_type),
      x = pivot_data
    )
    style <- openxlsx::createStyle(numFmt = "GENERAL")
    if (current_type == "percent") style <- style_percent
    if (current_type == "integer") style <- style_integer
    if (current_type == "text") style <- style_text
    if (current_type == "double") style <- style_double
    if (current_type == "percent") style <- style_percent
    openxlsx::addStyle(
      wb = workbook,
      sheet = paste0("pivot_", current_type),
      style = style,
      rows = seq_along(pivot_data$metric_type) + 1,
      cols = which(colnames(pivot_data) %in% data2$fy),
      gridExpand = TRUE
    )
  }

  # Save workbook -----
  openxlsx::saveWorkbook(
    wb = workbook,
    file = filepath,
    overwrite = TRUE
  )
}

#' Build Pivot Table
#'
#' Pivot data by year, filtered to a metric type
#'
#' @param data A table of MDUFA metrics, such as \code{mdufa::mdufa4}.
#' @param metric_type_filter The metric type to filter to before pivoting, equal
#'   to either \code{"integer"}, \code{"percent"}, \code{"double"}, or
#'   \code{"text"}.
#'
#' @return A tibble.
#' @export
#'
build_pivot <-
  function(data,
           metric_type_filter = c("integer", "double", "percent", "text")) {
    chk::check_names(
      x = data,
      names =
        c(
          "value",
          "metric_type",
          "fy",
          "value_formatted"
        )
    )
    data |>
      dplyr::filter(
        .data$metric_type == metric_type_filter
      ) |>
      dplyr::select(
        -"value"
      ) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(
        names_from = "fy",
        values_from = "value_formatted"
      )
  }
