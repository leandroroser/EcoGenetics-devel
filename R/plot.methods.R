
##############################################################################
#                             PLOT METHODS
##############################################################################

#-------------------------------------------------------------------#
#' globalplot
#' @description Plot method for correlograms and variograms
#' @param x Result of correlogram or variogram analysis.
#' @param var Variable to plot for multiple analyses with \code{\link{eco.correlog}}
#' (see examples).
#' @seealso  \code{\link{eco.correlog}} \code{\link{eco.cormantel}}  \code{\link{eco.variogram}}
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' # single Moran's I correlogram analysis
#' moran.example <- eco.correlog(Z=eco[[P]][,1], eco$XY, smax=10, size=1000)
#' plot(moran.example)
#' 
#' # multiple Moran's I correlogram analysis
#' moran.example2 <- eco.correlog(Z=eco[[P]], eco$XY, smax=10, size=1000)
#' plot(moran.example2, var ="P2")
#' plot(moran.example2, var ="P3")
#' 
#' corm <- eco.cormantel(M = dist(eco[[P]]), size=1000,smax=7, XY = eco$XY,
#' nsim = 99)
#' plot(corm)
#' 
#' variog <- eco.variogram(Z = eco[[P]][, 2],XY =  eco$XY)
#' plot(variog)
#' }
#' 
#' @rdname eco.correlog-methods
#' 
#' @aliases plot,eco.correlog-method
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 


setMethod("plot", "eco.correlog", function(x) cat("Use the function eco.plotCorrelog to plot this object")
)
#-------------------------------------------------------------------#
#' plot eco.lsa
#' @rdname eco.lsa-methods
#' @aliases plot,eco.lsa-method
#' @keywords internal

setMethod("plot", "eco.lsa", function(x) cat("Use the function eco.plotLocal to plot this object")
)


#-------------------------------------------------------------------#
#' plot eco.IBD
#' @keywords internal
#' @rdname eco.IBD 
#' @aliases plot,eco.IBD-method


setMethod("plot", "eco.IBD",
          function(x) cat("Use the function eco.plotCorrelog to plot this object")
          )

#-------------------------------------------------------------------#
#' plot eco.multilsa
#' @param x eco.multilsa object
#' @description Plot method for local spatial analysis
#' @rdname eco.multilsa-method
#' @aliases plot,eco.multilsa-method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @seealso  \code{\link{eco.lsa}}

setMethod("plot", 
          "eco.multilsa", 
          function(x) cat("Use the function eco.plotLocal to plot this object")
          )


#-------------------------------------------------------------------#
#' listplot
#' @param x list of ggplot objects
#' @param n number of plot in layout
#' @param nrow Number of rows in layout
#' @param byrow plot by row?
#' @param significant plot only significant individuals?
#' @description Plot method for local spatial analysis as list
#' @rdname eco.listlsa-method
#' @aliases plot,eco.listlsa-method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @seealso  \code{\link{eco.lsa}}
#' @keywords internal

setMethod("plot", "eco.listlsa", 
          function(x) cat("Use the function eco.plotLocal to plot this object")
          )


#-------------------------------------------------------------------#
#' Plot for a connection network
#' @param con connection network
#' @description Plot method for an eco.weight object. This function
#' makes a graph for the original coordinates and transformed as ranks.
#' @aliases plot,eco.weight-method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

setMethod("plot", "eco.weight", 
          function(x) cat("Use the function eco.plotWeight to plot this object")
          )

#-------------------------------------------------------------------#
#' Plot for eco.gsa objects
#' @param x eco.gsa object
#' @description Plot method for an eco.gsa objects object. 
#' @aliases plot,eco.gsa-method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

setMethod("plot", "eco.gsa", 
          function(x) cat("Use the function eco.plotGlobal to plot this object")
          )
