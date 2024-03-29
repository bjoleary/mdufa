% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_tables.R
\name{collapse_line_breaks}
\alias{collapse_line_breaks}
\title{Collapse Line Breaks}
\usage{
collapse_line_breaks(text_string, number_of_breaks = 2)
}
\arguments{
\item{text_string}{The string to process. Only the first element is
processed.}

\item{number_of_breaks}{Defaults to \code{2}. The lower bound of the number
of breaks to look for and collapse into a single line break. So, by default,
this function collapses two or more line breaks into a single line break.}
}
\value{
The first element of \code{text_string} with line breaks collapsed.
}
\description{
Remove duplicate line breaks from the first element of \code{text_string}.
}
