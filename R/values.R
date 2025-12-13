submission_types <-
  c(
    "510.{0,4}",
    "De Novo",
    "PMA Original and Panel\\-Track Supplements",
    "PMA Originals and Panel Tracked Supplements",
    "PMA 180\\-Day Supplements",
    "PMA Real-Time Supplements",
    "Pre\\-Market Report Submissions",
    "IDE",
    "Pre\\-Sub"
  ) |>
  # Wrap with word boundaries
  (\(x) paste0("\\b", x, "\\b"))() |>
  # Add patterns with parentheses that don't work well with word boundaries
  # DUAL CLIA pattern must come before 510(k) to match first
  (\(x) c(
    "DUAL \\(510\\(k\\) and CLIA Waiver\\)",
    x,
    "PMAs \\(All Review Tracks\\)"
  ))() |>
  # Collapse with "OR":
  paste(collapse = "|") |>
  stringr::regex()

url_report_page <-
  paste0(
    "https://www.fda.gov/industry/medical-device-user-fee-amendments-mdufa/",
    "mdufa-reports"
  )
