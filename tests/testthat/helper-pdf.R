# Helper functions for PDF-based tests
# Automatically sourced by testthat before running tests

# S3 bucket URL for public PDF reports
s3_pdf_base_url <- "https://s3.us-east-005.dream.io/boleary-com-blog"

# Cache directory for downloaded PDFs (persists across test runs)
get_pdf_cache_dir <- function() {

  cache_dir <- file.path(tempdir(), "mdufa_pdf_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}

# Get a test PDF - checks local first, then downloads from S3
# Returns path to PDF or NULL if unavailable
get_test_pdf <- function(pattern) {
  # First try local directories
  local_path <- find_local_pdf_path(pattern)
  if (!is.null(local_path)) {
    return(local_path)
  }

  # Try cache
  cached_path <- find_cached_pdf(pattern)
  if (!is.null(cached_path)) {
    return(cached_path)
  }


  # Download from S3
  download_pdf_from_s3(pattern)
}

# Find PDF in local directories (original behavior)
find_local_pdf_path <- function(pattern) {
  pkg_root <- tryCatch(
    rprojroot::find_package_root_file(),
    error = function(e) NULL
  )

  possible_dirs <- c(
    if (!is.null(pkg_root)) file.path(pkg_root, "data-raw", "pdf_reports"),
    "data-raw/pdf_reports",
    "../data-raw/pdf_reports",
    "../../data-raw/pdf_reports"
  )

  for (pdf_dir in possible_dirs) {
    if (!is.null(pdf_dir) && dir.exists(pdf_dir)) {
      files <- list.files(pdf_dir, pattern = pattern, full.names = TRUE)
      if (length(files) > 0) {
        return(files[1])
      }
    }
  }
  NULL
}

# Find PDF in cache directory
find_cached_pdf <- function(pattern) {
  cache_dir <- get_pdf_cache_dir()
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  if (length(files) > 0) {
    return(files[1])
  }
  NULL
}

# Download PDF from S3 bucket
download_pdf_from_s3 <- function(pattern) {
  # Require curl package for downloads

  if (!requireNamespace("curl", quietly = TRUE)) {
    return(NULL)
  }

  # Convert pattern to filename (e.g., "mdufa-4_2023-11-16" -> full filename)
  filename <- paste0(pattern, "_quarterly-report.pdf")
  url <- paste0(s3_pdf_base_url, "/", filename)
  dest <- file.path(get_pdf_cache_dir(), filename)

  tryCatch(
    {
      # Use curl for better error handling
      resp <- curl::curl_fetch_disk(url, dest)
      if (resp$status_code == 200 && file.exists(dest)) {
        message("Downloaded test PDF: ", filename)
        return(dest)
      } else {
        if (file.exists(dest)) file.remove(dest)
        NULL
      }
    },
    error = function(e) {
      message("Failed to download PDF: ", conditionMessage(e))
      NULL
    }
  )
}

# Wrapper for backward compatibility - replaces find_local_pdf in test files
find_local_pdf <- function(pattern) {
  get_test_pdf(pattern)
}
