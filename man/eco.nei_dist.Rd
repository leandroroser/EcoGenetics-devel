% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.nei_dist.R
\name{eco.nei_dist}
\alias{eco.nei_dist}
\title{Estimate Nei distance matrix}
\usage{
eco.nei_dist(obj, as_dist = TRUE)
}
\arguments{
\item{obj}{ecopop or genpop  objects, or matrix/data frame with allele frequencies}

\item{as_dist}{Return a dist object or a matrix? default is an object of class "dist"}
}
\description{
Estimate Nei distance matrix. NAs are avoided.
}
\examples{
\dontrun{
data(eco.test)
eco.nei_dist(my_ecopop)
}
}
\author{
Juan Vilardi \email{vilardi@ege.fcen.uba.ar}
}
