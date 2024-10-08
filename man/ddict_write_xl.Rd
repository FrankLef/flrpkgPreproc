% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddict_xl.R
\name{ddict_write_xl}
\alias{ddict_write_xl}
\title{Write Data from \code{DDict} to Excel}
\usage{
ddict_write_xl(object, path, file = "ddict.xlsx", sheet = "data")
}
\arguments{
\item{object}{Object of class \code{DDict}.}

\item{path}{Path to excel file.}

\item{file}{Name of Excel file. Often comes from \code{ddict@data_fn}.}

\item{sheet}{Name of Excel sheet. Defaults is \emph{data}.}
}
\value{
Full file name of Excel file.
}
\description{
Write data from \code{DDict} to Excel.
}
\details{
An error message will be issued if the data from \code{DDict}.
}
\examples{
\dontrun{
TODO
}
}
