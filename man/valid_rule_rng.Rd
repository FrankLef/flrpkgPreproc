% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valid_rules.R
\name{valid_rule_rng}
\alias{valid_rule_rng}
\title{Write a string for a rule With range}
\usage{
valid_rule_rng(x, rng)
}
\arguments{
\item{x}{String representing the column name.}

\item{rng}{Numeric(2) with the range's boundaries.}
}
\value{
String with the rule usueable by \pkg{validate}.
}
\description{
Write a string for a rule with range.
}
\details{
The rule is used by the \pkg{validate} package and is in the form of
\bold{in_range(column name, minimum, maximum)}.
}
\examples{
rule <- valid_rule_rng("var", rng = c("min" = 0, "max" = 1))
stopifnot(identical(rule, "in_range(var, min = 0, max = 1)"))
}
