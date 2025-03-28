% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{.validate_date}
\alias{.validate_date}
\title{Validate a date input (Internal)}
\usage{
.validate_date(date, default, is_start, inicio_vigencia = NULL)
}
\arguments{
\item{date}{A string containing a date in "dd/mm/yyyy" format. If NULL, the \code{default} value is used.}

\item{default}{A date to be used when \code{date} is NULL.}

\item{is_start}{A logical value indicating whether the date is a start date (\code{TRUE}) or an end date (\code{FALSE}).}

\item{inicio_vigencia}{A date object representing the start date. Required when \code{is_start = FALSE} to ensure
the end date is not earlier than the start date.}
}
\value{
A formatted date string in "dd/mm/yyyy".
}
\description{
This function checks if the given date is in the correct format ("dd/mm/yyyy"),
ensures it falls within allowed date ranges, and formats it accordingly.
}
\examples{
.validate_date("15/08/2020", Sys.Date(), TRUE)
.validate_date(NULL, Sys.Date(), FALSE, as.Date("2020-08-15"))
}
\keyword{internal}
