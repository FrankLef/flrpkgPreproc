% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ddict_transform.R
\name{ddict_transform}
\alias{ddict_transform}
\title{Transform Variables with a \code{DDict} and Custom Function}
\arguments{
\item{object}{Object of class \code{DDict}.}

\item{data}{Data frame with date variables.}

\item{fn}{Custom function.}

\item{suffix}{String use as suffix to name the new transformed variables.}

\item{table_nm}{Table name used to filter the dictionary.}

\item{col_nm}{A \pkg{glue} specification for added column. See help for
\code{across} in \pkg{dplyr}. In particular, \code{col_nm = NULL} is
equivalent \code{col_nm = "{.col}"}.}
}
\value{
\code{data} with new transformed columns.
}
\description{
Transform variables with a \code{DDict} and custom function.
}
\details{
The data is transformed using a function defined in \code{fn} based on a
filter \code{process == suffix} from a \code{DDict}. The suffix is then
appended to the original name of the variable to create a new column in
\code{data}. When \code{col_nm = NULL}, no new column is created, the
original column is however transformed.
}
\examples{
\dontrun{
TODO
}
}
