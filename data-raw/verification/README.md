# MDUFA Extraction Verification System

This directory contains development-only tools for verifying MDUFA PDF extraction accuracy.
These tools are NOT part of the installed package.

## Prerequisites

Install development dependencies:

```r
install.packages(c("shiny", "glue", "stringr"))
```

Ensure local PDF copies exist in `data-raw/pdf_reports/`.

## Quick Start

### One-Command Verification

```r
source("data-raw/verification/run_verification.R")
verify_report("data-raw/pdf_reports/mdufa-4_2023-11-16.pdf", "MDUFA IV")
```

This runs the complete workflow:

1. Generates diff and opens in KSDiff
2. Waits for you to review removed rows
3. Generates stratified sample (35 rows)
4. Launches Shiny verification app
5. Generates test file when verification passes

### Step-by-Step Workflow

For more control:

```r
source("data-raw/verification/run_verification.R")

# Step 1: Generate diff and review in KSDiff
diff_result <- generate_diff_for_report(
  pdf_path = "data-raw/pdf_reports/mdufa-4_2023-11-16.pdf",
  mdufa_period = "MDUFA IV"
)

# Review removed rows in KSDiff before proceeding

# Step 2: Generate sample
sample <- generate_verification_sample(diff_result, n = 35)

# Step 3: Launch verification app
results_path <- launch_verification_app(
  sample = sample,
  pdf_path = "data-raw/pdf_reports/mdufa-4_2023-11-16.pdf",
  mdufa_period = "MDUFA IV"
)

# Step 4: Check results and generate test file
results <- read.csv(results_path)
passed <- results |> dplyr::filter(status == "pass")
failed <- results |> dplyr::filter(status == "fail")

status <- check_verification_status(nrow(passed), nrow(failed))
message(status$message)

if (status$status == "COMPLETE") {
  generate_test_file(
    results = passed,
    pdf_path = "data-raw/pdf_reports/mdufa-4_2023-11-16.pdf",
    mdufa_period = "MDUFA IV",
    output_file = "tests/testthat/test-verified-mdufa4-2023-11-16.R"
  )
}
```

## Keyboard Shortcuts (Shiny App)

| Key | Action |
|-----|--------|
| `P` or `Enter` | Pass current row |
| `F` | Fail current row |
| `S` | Skip current row |
| `G` | Mark as garbage data |
| `Left` or `K` | Previous row |
| `Right` or `J` or `Space` | Next row |
| `N` | Focus notes field |
| `Escape` | Exit notes field |
| `Q` | Finish & Save |

## Statistical Basis

- **Goal:** Lower bound of 95% CI for accuracy > 90%
- **Method:** Wilson score interval
- **Minimum sample:** 35 rows (with 100% pass rate)

The Wilson score lower bound is calculated as:

```
LB = n / (n + z²)
```

Where z = 1.96 for 95% CI, so z² ≈ 3.84.

| Sample Size | Pass Rate | Lower Bound |
|-------------|-----------|-------------|
| 34 | 100% | 89.85% |
| 35 | 100% | 90.11% |
| 40 | 100% | 91.24% |
| 50 | 100% | 92.88% |

## Resuming Interrupted Sessions

The app auto-saves progress. To resume:

```r
launch_verification_app(
  resume_from = "/tmp/verification_results.csv",
  pdf_path = "data-raw/pdf_reports/mdufa-4_2023-11-16.pdf",
  mdufa_period = "MDUFA IV"
)
```

## Handling Failures

If any row fails verification:

1. Document the failure in the app (add notes)
2. Stop verification (Q or Finish & Save)
3. Fix the extraction code
4. Restart verification from scratch

## Verification from CSV

Run verification directly from a CSV file containing rows to verify:

```r
source("data-raw/verification/run_verification.R")
verify_from_csv("tests/testthat/fixtures/decision-rate-oht.csv")
```

Two CSV formats are supported:

**Full format** (has `report_mdufa_period` and `page` columns):
```
report_mdufa_period,page,table_number,organization,performance_metric,fy,value
MDUFA III,45,3.1,CDRH,Total Decisions,2018,1234
```

**Simple format** (has `dataset` column, page looked up from bundled data):
```
dataset,table_number,organization,performance_metric,fy,value
mdufa4,3.1,CDRH,Total Decisions,2018,1234
```

The app processes each MDUFA period sequentially, sampling 35 unique metrics per report.

## Quick Check Mode

For spot-checking without full verification:

```r
source("data-raw/verification/run_verification.R")
quick_check("data-raw/pdf_reports/mdufa-4_2023-11-16.pdf", "MDUFA IV", n = 5)
```

This verifies 5 random rows interactively without the Shiny app.

## Files

| File | Purpose |
|------|---------|
| `run_verification.R` | Entry point - source this first |
| `verification_app.R` | Shiny app for interactive verification |
| `verification_diff.R` | Diff generation and comparison |
| `verification_sample.R` | Stratified sampling functions |
| `verification_helpers.R` | Image generation, row highlighting |
| `README.md` | This file |

## Test File Naming Convention

Generated test files follow the pattern:

```
tests/testthat/test-verified-mdufa{N}-{YYYY-MM-DD}.R
```

Examples:

- `test-verified-mdufa3-2018-12-10.R`
- `test-verified-mdufa4-2023-11-16.R`
- `test-verified-mdufa5-2025-08-27.R`

## Workflow Diagram

```
┌─────────────────────────────────────────┐
│         verify_report(pdf, period)      │
└─────────────────┬───────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│  generate_diff_for_report() → KSDiff    │
└─────────────────┬───────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│  User reviews removed rows in KSDiff    │
└─────────────────┬───────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│  generate_verification_sample(n=35)     │
└─────────────────┬───────────────────────┘
                  │
                  ▼
┌─────────────────────────────────────────┐
│  launch_verification_app() → Shiny      │
└─────────────────┬───────────────────────┘
                  │
          ┌───────┴───────┐
          │               │
          ▼               ▼
    ┌──────────┐    ┌──────────┐
    │   PASS   │    │   FAIL   │
    └────┬─────┘    └────┬─────┘
         │               │
         ▼               ▼
┌─────────────────┐  ┌─────────────────────────┐
│ check_status()  │  │ STOP - fix code         │
│ LB > 90%?       │  │ restart verification    │
└────────┬────────┘  └─────────────────────────┘
         │
    YES  │
         ▼
┌─────────────────────────────────────────┐
│  generate_test_file() → test-verified-* │
└─────────────────────────────────────────┘
```
