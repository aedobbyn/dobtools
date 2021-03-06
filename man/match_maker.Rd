% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_maker.R
\name{match_maker}
\alias{match_maker}
\title{Fuzzy Text Matching: element}
\usage{
match_maker(e, matches, max_dist = 5)
}
\arguments{
\item{e}{An character}

\item{matches}{A vector in which to look for matches with e.}

\item{max_dist}{Set maxDist to be used in stringdist::amatch}
}
\description{
Find the best match (or no match at all) to string inputs.
}
\examples{

iris <- iris \%>\% tibble::as_tibble()
iris \%>\% dplyr::mutate(
  foo = purrr::map_chr(as.character(Species), match_maker,
                matches = c("Virginia", "California", "Sarasota"))
  )

iris \%>\% dplyr::mutate(
  foo = purrr::map_chr(as.character(Species), match_maker,
                matches = c("Virginia", "California", "Sarasota"), max_dist = 20)
)
}
\keyword{match}
