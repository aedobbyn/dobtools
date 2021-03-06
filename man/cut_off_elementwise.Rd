% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_off.R
\name{cut_off_elementwise}
\alias{cut_off_elementwise}
\title{Cut Off Elementwise}
\usage{
cut_off_elementwise(e, cutoff = 1.96)
}
\arguments{
\item{e}{An element of a vector.}

\item{cutoff}{A numeric cutoff value.}
}
\description{
Throw out elements that exceed a certain absolute value.
}
\examples{

mtcars$carb[3:15] \%>\% purrr::map(cut_off_elementwise, cutoff = 3)
}
\keyword{cutoff}
