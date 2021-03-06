% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.theilsen.R
\name{eco.theilsen}
\alias{eco.theilsen}
\title{Theil-sen regression for a raster time series, 
with parallelization available}
\usage{
eco.theilsen(stacked, dates, adjust = "none", run_parallel = FALSE,
  workers = NULL, physical = FALSE, cl_type = NULL)
}
\arguments{
\item{stacked}{Stacked images ("RasterLayer"  or "RasterBrick").}

\item{dates}{Data vector with decimal dates for each image.}

\item{adjust}{P-values correction method for multiple tests.
passed to \code{\link[stats]{p.adjust}}. Defalut is "none".}

\item{run_parallel}{Run code in parallel? Default FALSE}

\item{workers}{Number of workers used for parallel evaulation. If NULL,
the program uses N - 1, where N is the total number of available 
logical cores.}

\item{physical}{Use only physical cores for parallel evaluation? Default FALSE.}

\item{cl_type}{Cluster type. If not specified, "PSOCK" will be used for windows
and "FORK" otherwise. The value is passed as the parameter "type" 
to the function \code{\link[parallel]{makeCluster}}.}
}
\description{
This function computes the theil-sen estimator and 
the associated P-value, for each pixel over time in a stack of images.
The output consists of two rasters (one for the estimators and one for 
the P-values). It is recommended to use a "RasterBrick", which
is more efficient in memory management. 
The program can compute the result using serial (default) or parallel evaluation.
For parallel evaluation, the program uses PSOCK cluster for windows, and FORK cluster for 
other operative systems.
}
\examples{
\dontrun{
require("raster")
set.seed(6)

temp <- list()
for(i in 1:100) {
temp[[i]] <- runif(36,-1, 1)
temp[[i]] <- matrix(temp[[i]], 6, 6)
temp[[i]] <- raster(temp[[i]])
}

temp <- brick(temp)


writeRaster(temp,"temporal.tif", overwrite=T)
rm(temp)
ndvisim <- brick("temporal.tif")

date <- seq(from = 1990.1, length.out = 100, by = 0.2)


# Parallel evaluation ----

eco.theilsen(ndvisim, date)

slope <- raster("slope.tif")
pvalue <- raster("pvalue.tif")

par(mfrow = c(1, 2))
plot(slope, main = "slope")
plot(pvalue, main = "p-value")

file.remove(c("slope.tif", "pvalue.tif"))


# Serial evaluation ----

eco.theilsen(ndvisim, date)

slope <- raster("slope.tif")
pvalue <- raster("pvalue.tif")

par(mfrow = c(1, 2))
plot(slope, main = "slope")
plot(pvalue, main = "p-value")
file.remove(c("temporal.tif", "slope.tif", "pvalue.tif"))
}

}
\references{
Sen, P. 1968. Estimates of the regression coefficient based on Kendall's tau. 
Journal of the American Statistical Association, Taylor and Francis Group, 63: 1379-1389.

Theil H. 1950. A rank-invariant method of linear and polynomial regression analysis, 
Part 3 Proceedings of Koninalijke Nederlandse Akademie van Weinenschatpen A, 53: 397-1412.
}
\seealso{
\code{\link[rkt]{rkt}}.
}
\author{
Leandro Roser \email{learoser@gmail.com}
}
