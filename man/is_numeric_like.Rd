% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formating.R
\name{is_numeric_like}
\alias{is_numeric_like}
\title{Tests if a character vector contains numeric-like values}
\usage{
is_numeric_like(x, elementwise = FALSE)
}
\arguments{
\item{x}{character vector}

\item{elementwise}{if \code{TRUE}, returns a logical vector for each element, otherwise returns a single logical value indicating if all elements are numeric-like (default: \code{FALSE})}
}
\value{
logical vector
}
\description{
Tests if a character vector contains numeric-like values
}
\examples{
is_numeric_like(c("-9", " 2.0", "-1.0 "))
is_numeric_like(c("9-", -1, "10"))
is_numeric_like(c("9", -1.2, "10"))
}
