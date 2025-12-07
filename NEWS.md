# mdufa (development version)

# mdufa 0.2.0

## New Features

* New `extract_report()` function provides unified extraction for MDUFA III, IV,
  and V PDF reports. This is the recommended function for extracting data from
  local PDF files.

* New `verify_row()` function for visual verification of extracted data. Opens
  an annotated PDF page image with the specific data cell highlighted in yellow.

* New `view_report()` and `view_report_row()` helper functions for opening PDF
  reports to specific pages during verification.

* Comprehensive regression test suite with 595+ tests covering extraction
  accuracy across all MDUFA periods.

* Snapshot tests for MDUFA III, IV, and V ensure extraction consistency.

## Bug Fixes

* Table 9.2 (Q-Sub written feedback metrics) now extracted for MDUFA V
  (fixes #1).

* Page numbers now correctly populated for MDUFA V reports that lack printed
  "Page X of Y" footers by using the PDF page index (fixes #6).

* Fixed MDUFA III Substantive Interaction (SI) Goals extraction where
  performance goal text was incorrectly parsed.

* Fixed Breakthrough Therapy metric extraction for MDUFA V.

* Fixed table number extraction for malformed headers like "Table 9.1DAGRID"
  (missing space after table number).

* Fixed metric type matching for various edge cases.

## Internal

* Added `find_local_pdf()` helper for locating PDF files in the local archive.

* Improved extraction pipeline with better handling of multi-line table titles
  and metric labels.

# mdufa 0.1.4

* Updated bundled `mdufa5` dataset with February 27, 2025 MDUFA V report.

* Added `download_all_reports.R` script for maintaining local PDF archive.

# mdufa 0.1.3

* Added `mdufa5` dataset with MDUFA V performance data.

* New `get_m5()` function for extracting data from MDUFA V PDF reports.

* Replaced `checkr` package dependency with `chk`.

# mdufa 0.1.2

* Added support for parsing annual reports (e.g., 2021-11-16 report).

* Fixed URL handling after retirement of go.usa.gov URL shortener.

* Data refresh with latest available reports.

# mdufa 0.1.1

* Added `mdufa2` dataset with MDUFA II performance data (manually curated).

* Added `mdufa3` dataset with MDUFA III performance data.

* Enhanced Excel export functionality with improved styling.

* Fixed MDUFA III Substantive Interaction table parsing.

# mdufa 0.1.0

* Initial release.

* Bundled `mdufa4` dataset with MDUFA IV performance data.

* `filter_metrics()` function for filtering data by metric type and converting
  values to appropriate R types.

* `export_excel()` function for exporting data to Excel format with formatting.
