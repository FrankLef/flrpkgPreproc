% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CScaler.R
\name{CScaler_do}
\alias{CScaler_do}
\title{Apply conditional scales}
\usage{
CScaler_do(object, ...)
}
\arguments{
\item{object}{Object of class \code{CScaler}.}

\item{...}{Additional arguments used by methods. Such as
\describe{
\item{data}{Data.frame to scale.}
\item{vars}{Columns to scale}
\item{inverse}{Perform inverse scale The suffix \code{suffix} is
not used when \code{inverse=TRUE}}
\item{keep}{Keep the \code{base_var} in the output.}
}}
}
\value{
Data.frame.
}
\description{
Apply conditional scales.
}
\details{
Scale \code{data} depending on the \code{id_vars} from \code{CScaler}.
}
\examples{
\dontrun{
TODO
}
}
