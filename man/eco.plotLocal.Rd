% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.plotLocal.R
\name{eco.plotLocal}
\alias{eco.plotLocal}
\title{eco.plotLocal}
\usage{
eco.plotLocal(x, interactivePlot = TRUE, multi = c("d3heatmap", "ggplot"),
  significant = TRUE, alpha = 0.05, rescaled = FALSE, limits = NULL,
  title = NULL, z.name = NULL, grp = NULL, vertical = TRUE,
  legend = TRUE, n = 4, nrow = 2, byrow = TRUE, ...)
}
\arguments{
\item{x}{Result of eco.lsa analysis}

\item{interactivePlot}{Show an interactive plot via plotly? (default: TRUE)}

\item{multi}{for multivariable plot, use d3heatmap or ggplot2? (Default: d3heatmap).
In d3heatmap, NA values are set to 0.}

\item{significant}{Show all non significant points with a same colour?}

\item{alpha}{significance (alpha) for P (or P-adjusted) values (Default: 0.05)}

\item{rescaled}{rescale statistics between -1 and 1? (Default: FALSE)}

\item{limits}{When multiple variables are used, values used  as limits for computing the gradient for the plot}

\item{title}{title of the plot}

\item{z.name}{name of the variables axis in multivariable plot (using ggplot2 like plots)}

\item{grp}{groups for multivariable plot (using ggplot2 like plots)}

\item{vertical}{vertical multivariable plot? (Default: true)}

\item{legend}{Show legend?}

\item{n}{number of plot per screen in multivariable plots as list}

\item{nrow}{number of rows in multivariable plots as lists}

\item{byrow}{plot by row in multivariable plots by list}

\item{...}{additional arguments passed to eco.rankplot, eco.forestplot, 
eco.resterplot o grf.seqmultiplot, depending of the selected plot}
}
\description{
For examples see  \code{\link{eco.lsa}} 

SINGLE VARIABLES:

Using permutation test: The function calls eco.rasterplot, 
who generates a plot for a numeric or
factor variable.
The X and Y axes in the plot correspond 
to the rank of the X and Y coordinates, respectively. 
Additional parameters can be passed to eco.rankplot.

Using bootstrap test: The function calls eco.forestplot, 
who computes a forest plot 
for the confidence interval of each individual of the
input data (as row number)
and the corresponding observed value of the statistic.
Additional parameters can be passed to eco.forestplot.


MULTIPLE VARIABLES: 
multiple output format results. "list" for object with a list 
of individual test for each variable, or "matrix" for results as matrices
of multiples variables.

For results as matrices (option multi = "matrix" in eco.lsa): 
The function class eco.rasterplot, who generates a multivariate plot for 
a data matrix (raster). Additional parameters can be passed to eco.rasterplot.
The resterplot graph is a flexible tools for multiple data sources 
(environmental, genetic, phenotypic, etc.).

For results as list (option multi = "list" in eco.lsa):
The function generates plots for individual variables calling eco.rankplot.
Additional parameters can be passed to eco.rankplot.
}
\author{
Leandro Roser \email{learoser@gmail.com}
}
