% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/par.R
\name{mopar_cts}
\alias{mopar_cts}
\title{Continuous variable}
\usage{
mopar_cts(lower, upper, start = NULL)
}
\arguments{
\item{lower}{Lower}

\item{upper}{Upper}

\item{start}{Start. Defaults to midpoint if not given.}
}
\value{
mixopt_par list
}
\description{
Continuous variable
}
\examples{
mopar_cts(2,8)
mopar_cts(2,8,7)
}
