
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mdufa: Structured Data on FDA’s Medical Device User Fee Program Performance

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mdufa)](https://CRAN.R-project.org/package=mdufa)
[![R-CMD-check](https://github.com/bjoleary/mdufa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bjoleary/mdufa/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bjoleary/mdufa/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bjoleary/mdufa?branch=main)
<!-- badges: end -->

This package includes structured data from the FDA’s quarterly Medical
Device User Fee Amendments (MDUFA) programs, which are published as PDFs
on
[fda.gov](https://www.fda.gov/industry/medical-device-user-fee-amendments-mdufa/mdufa-reports).
The package includes functions to scrape those PDFs to extract the data
and includes the most recent dataset as of publication.

Aside from some spot-checks, the data provided by the `mdufa` R package
has had limited verification and may be inaccurate. Use this information
at your own risk and verify information using the reports provided
directly by FDA. To facilitate this, each data point provided includes
information about its source, including a link to the FDA report from
which it came, the relevant page number, and more.

“Text” metrics are typically descriptions of the MDUFA performance goals
themselves (e.g. “90% Within 320 FDA Days”). Because of the way these
values break across lines within table cells in the PDF reports, they
are more difficult to retrieve and may be more likely to be incomplete
or to be inaccurate. Similarly, some tables in the reports have
footnotes, and they are not included in this dataset.

Aside from some spot-checks, the data provided by the mdufa R package
and included in this workbook has had limited verification and may be
inaccurate. Use this information at your own risk and verify information
using the reports provided directly by FDA. To facilitate this, each
data point provided includes information about its source, including a
link to the FDA report from which it came, the relevant page number, and
more.

If you find a problem in this dataset, please report it
[here](https://github.com/bjoleary/mdufa/issues).

Please carefully read the
[license](https://github.com/bjoleary/mdufa/blob/main/LICENSE.md) for
additional important information.

## Installation

<!-- You can install the released version of mdufa from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("mdufa") -->
<!-- ``` -->

Install the development version from
[GitHub](https://github.com/bjoleary/mdufa) with:

``` r
# install.packages("devtools")
devtools::install_github("bjoleary/mdufa")
```

## Examples

You can access data from MDUFA 3 using `mdufa::mdufa3`:

``` r
library(mdufa)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
set.seed(1)
mdufa::mdufa3 %>% 
  select(organization, program, metric_type, performance_metric, fy, value) %>% 
  sample_n(5) %>% 
  print()
#> # A tibble: 5 × 6
#>   organization program metric_type performance_metric                fy    value
#>   <chr>        <chr>   <chr>       <chr>                             <chr> <chr>
#> 1 OIR          PMA     integer     Number of PMAs filed              2014  0    
#> 2 DSD          510(k)  double      Mean FDA days for submissions th… 2017  0    
#> 3 ODE          510(k)  integer     Number of Withdrawals             2014  175  
#> 4 DCTD         510(k)  integer     80th Percentile Total days to MD… 2013  245  
#> 5 CDRH         PMA     integer     Panel-Tracked Supplements (Panel… 2015  0
```

Data from MDUFA 5 is available using `mdufa::mdufa5`. MDUFA 4 is also
available using `mdufa::mdufa4` and data from MDUFA 2 is available using
`mdufa::mdufa2`.

Metrics can be assessed and graphed over time:

``` r
library(ggplot2)
data <- 
  mdufa::mdufa4 %>% 
  mdufa::filter_metrics("double") %>%
  filter(
    report_date == max(report_date),
    organization == "CDRH",
    program == "510(k)",
    performance_metric == "Average Number of Total Days to MDUFA IV Decision"
  ) %>% 
  dplyr::mutate(
    fy = as.integer(fy)
  )
graph_title <- 
  paste(data$performance_metric %>% unique(), "by Fiscal Year Received")
x_label <- "Fiscal Year Received"
y_label <- data$performance_metric %>% unique()
graph_caption <- 
  paste0(
    "Source: ",
    data$report_description %>% unique(),
    " available at ",
    data$report_link %>% unique()
  )
graph <- 
  ggplot(
    data = data,
    mapping = 
      aes(
        x = fy,
        y = value
      )
  ) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA)) + 
  labs(
    title = graph_title,
    y = y_label,
    x = x_label,
    caption = graph_caption
  )
plot(graph)
```

<img src="man/figures/README-example_2-1.png" width="100%" />

# Exporting the data

In many cases, you may wish to locally save the data from a MDUFA report
into a commonly used format, such as excel. This can be accomplished
using the data already included in the package:

``` r
mdufa::export_excel(
  data = mdufa::mdufa4,
  filepath = "mdufa4_quarterly_performance.xlsx"
)
```

# Building the datasets for yourself

Code to build the datasets for yourself can be found in
[data-raw/](data-raw/).
