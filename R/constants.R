#' IVD Division Identifiers
#'
#' MDUFA III division codes that map to OHT7 (In Vitro Diagnostics).
#' These four divisions were consolidated into OHT7 during the FDA
#' reorganization on September 30, 2019.
#'
#' @format Character vector of division codes
#' @export
ivd_divisions <- c("DCTD", "DIHD", "DMD", "DMGP")

#' IVD Division Full Names
#'
#' Full names for IVD divisions as they appear in MDUFA III reports.
#'
#' @format Character vector of full division names
#' @export
ivd_divisions_full <- c(
  "DCTD: Chemistry and Toxicology Devices",
  "DIHD: Immunology and Hematology Devices",
  "DMD: Microbiology Devices",
  "DMGP: Molecular Genetics and Pathology Devices"
)

#' Division to OHT Mapping
#'
#' Maps MDUFA III division codes to their corresponding OHT organization
#' codes used in MDUFA IV and V. Some divisions map 1:1, while IVD
#' divisions (DCTD, DIHD, DMD, DMGP) all map to OHT7.
#'
#' Divisions that cannot be cleanly mapped (DAGRID, DOED, DSD, DRGUD)
#' are excluded from cross-MDUFA comparison as they were split or

#' combined during the reorganization.
#'
#' @format Named character vector where names are division codes and
#'   values are OHT codes
#' @export
division_to_oht <- c(
  "DCD" = "OHT2",
  "DNPMD" = "OHT5",
  "DOD" = "OHT6",
  "DRH" = "OHT8",
  "DCTD" = "OHT7",
  "DIHD" = "OHT7",
  "DMD" = "OHT7",
  "DMGP" = "OHT7"
)

#' Divisions Excluded from Cross-MDUFA Comparison
#'
#' Division codes that cannot be mapped to OHT organizations because
#' they were split across multiple OHTs or combined with other divisions
#' during the FDA reorganization.
#'
#' @format Character vector of division codes
#' @export
excluded_divisions <- c("DAGRID", "DOED", "DSD", "DRGUD")

#' OHT Full Names
#'
#' Full names for OHT organizations as they appear in MDUFA IV and V reports.
#' Used for labeling plots and tables.
#'
#' @format Named character vector where names are OHT codes and values are
#'   full names
#' @export
oht_names <- c(
  "OHT1" = "OHT1: Ophthalmic, Anesthesia, Respiratory, ENT, Dental",
  "OHT2" = "OHT2: Cardiovascular",
  "OHT3" = "OHT3: Gastroenterology-Renal-Urological-General Hospital",
  "OHT4" = "OHT4: General and Plastic Surgery, Infection Control",
  "OHT5" = "OHT5: Neurological and Physical Medicine",
  "OHT6" = "OHT6: Orthopedic Devices",
  "OHT7" = "OHT7: In Vitro Diagnostics",
  "OHT8" = "OHT8: Radiological Health"
)

#' Comparable OHT Organizations
#'
#' OHT codes that can be compared across MDUFA III, IV, and V periods.
#' OHT1, OHT3, and OHT4 are excluded because they don't have clean
#' mappings from MDUFA III divisions.
#'
#' @format Character vector of OHT codes
#' @export
comparable_ohts <- c("OHT2", "OHT5", "OHT6", "OHT7", "OHT8")

#' FDA Blue Color
#'
#' The primary blue color used in FDA visualizations and branding.
#'
#' @format Character string with hex color code
#' @export
color_fda_blue <- "#2e79b4"

#' COVID-19 Public Health Emergency Start Date
#'
#' The date the COVID-19 Public Health Emergency was declared
#' (January 27, 2020). This date marks the beginning of the PHE period
#' which affected FDA review timelines.
#'
#' @format Date object
#' @export
date_phe_start <- as.Date("2020-01-27")

#' COVID-19 Public Health Emergency End Date
#'
#' The date the COVID-19 Public Health Emergency ended (May 11, 2023).
#' This date marks the end of the PHE period.
#'
#' @format Date object
#' @export
date_phe_end <- as.Date("2023-05-11")

#' Organization Full Names
#'
#' Full names for all organizations that appear in MDUFA reports, including
#' centers (CDRH, CBER), offices (ODE, OIR), MDUFA III divisions, and
#' MDUFA IV/V OHTs. Use this lookup to convert organization codes to
#' descriptive labels for plots and tables.
#'
#' @format Named character vector where names are organization codes and
#'   values are full names with the code prefix
#' @export
#'
#' @examples
#' # Get full name for a single organization
#' org_names["OHT7"]
#'
#' # Use with dplyr to add full names
#' library(dplyr)
#' mdufa4 |>
#'   mutate(org_full = org_names[organization]) |>
#'   select(organization, org_full) |>
#'   distinct()
org_names <- c(
  # Centers
  "CBER" = "CBER: Center for Biologics Evaluation and Research",
  "CDRH" = "CDRH: Center for Devices and Radiological Health",
  # Offices
  "ODE" = "ODE: Office of Device Evaluation",
  "OIR" = "OIR: Office of In Vitro Diagnostics and Radiological Health",
  # MDUFA III Divisions
  "DAGRID" = paste0(
    "DAGRID: Anesthesiology, General Hospital, Respiratory, ",
    "Infection Control, and Dental Devices"
  ),
  "DCD" = "DCD: Cardiovascular Devices",
  "DCTD" = "DCTD: Chemistry and Toxicology Devices",
  "DIHD" = "DIHD: Immunology and Hematology Devices",
  "DMD" = "DMD: Microbiology Devices",
  "DMGP" = "DMGP: Molecular Genetics and Pathology Devices",
  "DNPMD" = "DNPMD: Neurological and Physical Medicine Devices",
  "DOD" = "DOD: Orthopedic Devices",
  "DOED" = "DOED: Ophthalmic and Ear, Nose, and Throat Devices",
  "DRGUD" = "DRGUD: Reproductive, Gastro-Renal, and Urological Devices",
  "DRH" = "DRH: Radiological Health",
  "DSD" = "DSD: Surgical Devices",
  # MDUFA IV/V OHTs
  "OHT1" = "OHT1: Ophthalmic, Anesthesia, Respiratory, ENT, and Dental Devices",
  "OHT2" = "OHT2: Cardiovascular Devices",
  "OHT3" = "OHT3: GastroRenal, ObGyn, General Hospital, and Urology Devices",
  "OHT4" = "OHT4: Surgical and Infection Control Devices",
  "OHT5" = "OHT5: Neurological and Physical Medicine Devices",
  "OHT6" = "OHT6: Orthopedic Devices",
  "OHT7" = "OHT7: In Vitro Diagnostics",
  "OHT8" = "OHT8: Radiological Health"
)

# Column Name Constants --------------------------------------------------------

#' Expected Columns for Individual MDUFA Datasets
#'
#' Column names in the expected order for mdufa2, mdufa3, mdufa4, and mdufa5
#' datasets. Report metadata columns come first, followed by table location,
#' then metric data.
#'
#' @format Character vector of 14 column names
#' @export
#'
#' @examples
#' # Check if a dataset has expected columns
#' identical(names(mdufa4), mdufa_cols)
#'
#' # Reorder columns to standard order
#' data[, mdufa_cols]
mdufa_cols <- c(
  # Report metadata
  "report_description",
  "report_link",
  "report_date",
  "report_mdufa_period",
  # Table location
  "source",
  "page",
  "table_number",
  "organization",
  "program",
  "table_title",
  # Metric data
  "metric_type",
  "performance_metric",
  "fy",
  "value"
)

#' Expected Columns for mdufa_combined Dataset
#'
#' Column names in the expected order for the mdufa_combined dataset.
#' Builds on [mdufa_cols] with additional columns: `org` (full organization
#' name), `metric_harmonized` (standardized metric names), and `derived`
#' (flag for aggregated rows).
#'
#' @format Character vector of 17 column names
#' @export
#'
#' @seealso [mdufa_cols] for base column definitions
mdufa_combined_cols <- c(
  mdufa_cols[1:8],
  "org",
  mdufa_cols[9:12],
  "metric_harmonized",
  mdufa_cols[13:14],
  "derived"
)

#' Expected Columns for cohort_status Dataset
#'
#' Column names in the expected order for the cohort_status dataset.
#' Follows [mdufa_cols] order where columns overlap, with cohort-specific
#' columns (percent_closed, status) at the end.
#'
#' @format Character vector of 8 column names
#' @export
#'
#' @seealso [mdufa_cols] for base column definitions
cohort_status_cols <- c(
  "report_date",
  "report_mdufa_period",
  "organization",
  "org",
  "program",
  "fy",
  "percent_closed",
  "status"
)

#' Expected Columns for report_dates Dataset
#'
#' Column names in the expected order for the report_dates dataset.
#' Contains report publication dates and their corresponding data cutoff dates.
#'
#' @format Character vector of 5 column names
#' @export
report_dates_cols <- c(
  "report_date",
  "report_cutoff_date",
  "report_mdufa_period",
  "report_description",
  "report_link"
)
