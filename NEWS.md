# mdufa (development version)

# mdufa 0.3.1

## Bug Fixes

* Fixed missing `table_title` for Table 9.5 OHT8 in MDUFA IV (PDF defect in
  2023-11-16 report where subtitle line was missing).

* Fixed truncated metric name in Table 9.5: "Which Industry Provided Minutes
  Within 15" now extracts as full name "Percent of Submissions With Meetings
  for Which Industry Provided Minutes Within 15 Days".

* Fixed `metric_type` for the above metric from "integer" to "percent" (values
  are percentages).

## Data Updates

* Re-extracted `mdufa4` and `mdufa5` datasets with above fixes.

# mdufa 0.3.0

## New Features

* New `mdufa_combined` dataset provides unified data spanning all MDUFA periods
  (II through V) with harmonized metric names for cross-period analysis.

* New `mdufa3_oht` dataset (experimental) reorganizes MDUFA III data by Office
  of Health Technology (OHT) to enable comparison with MDUFA IV/V reporting
  structure. Division-level data is mapped to OHTs with intelligent aggregation.

* New helper functions for MDUFA data analysis:
  - `filter_metrics()` enhanced with additional filtering options
  - `harmonize_metric_names()` for case-normalized cross-period comparisons
  - `identify_aggregation_type()` determines appropriate aggregation method

* New `cohort_status` and `report_dates` reference datasets for programmatic
  access to MDUFA reporting metadata.

## Bug Fixes

* Fixed MDUFA III extraction for "Performance Metric" goal value patterns.

* Fixed wrapped "Current Performance Percent within X FDA Days" metric extraction
  where text broke across lines.

* Fixed MDUFA III Table 1.4 CDRH truncated "to Substantive" metrics.

## Internal

* Added S3 bucket integration for PDF reports, enabling CI/CD tests to download
  PDFs from public S3 bucket when not available locally.

* Fixed MDUFA III filename bug where Roman numeral replacement order caused
  "MDUFA III" to be mislabeled as "mdufa-2i". Renamed 23 affected files.

* Regenerated test-coverage workflow from r-lib/actions template to fix Codecov
  upload issues.

* Added verification test infrastructure with `verify_row()` for PDF-to-data
  validation using annotated page images.

# mdufa 0.2.3

## Bug Fixes

* Fixed CBER pattern matching bug in MDUFA V extraction that was incorrectly
  dropping CDRH/OHT data from Tables 8.x and 9.x. The pattern `".*CBER.*"` was
  matching a new "(CDRH+CBER)" metric name, causing pages 162-249 to be excluded.
  Changed to `"Table.*CBER"` to match only actual CBER table pages.

## Data Updates

* Updated `mdufa5` dataset with November 20, 2025 annual report (17,598 rows).
  Annual reports include additional CLIA Waiver tables (5.x, 7.x, 11.x, 12.x)
  not present in quarterly reports.

## Internal

* Fixed stratified sampling bug in verification tooling where `n_groups()` was
  referencing the wrong object, causing samples to be dominated by alphabetically
  first organizations.

* Added append functionality to verification test generation so new verifications
  merge with existing tests rather than overwriting.

* Fixed CSV column type handling to preserve table numbers like "1.10" (was being
  read as numeric "1.1").

* Added 450 verified extraction assertions (90 unique metrics across 5 fiscal years).

# mdufa 0.2.1

## Bug Fixes

* Fixed MDUFA V extraction to properly capture metrics with N/A values (e.g.,
  "Mean FDA Days for Submissions that Missed the Goal" in Table 1.12). The
  `fix_empty_rows()` function was incorrectly filtering these rows.

* Added footnote filter to remove ~1,160 garbage rows from MDUFA V extraction
  where PDF footnote text was incorrectly captured as performance metrics.

## Data Updates

* Updated `mdufa3` dataset with December 10, 2018 report (16,905 rows).

* Updated `mdufa4` dataset with November 16, 2023 report (16,275 rows).

* Updated `mdufa5` dataset with August 27, 2025 report (16,498 rows).

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
