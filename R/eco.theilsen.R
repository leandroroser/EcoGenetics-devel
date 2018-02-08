
#' Theil-sen regression for a raster time series
#' 
#' @description This function computes the theil-sen estimator and 
#' the associated P-value, for each pixel over time in a stack of images.
#' The output consists of two rasters (one for the estimators and one for 
#' the P-values). It is recommended to use a "RasterBrick", which
#' is more efficient in memory management.
#' 
#' @param stacked Stacked images ("RasterLayer"  or "RasterBrick").
#' @param date data vector with decimal dates for each image.
#' @param adjust P-values correction method for multiple tests.
#' passed to \code{\link[stats]{p.adjust}}. Defalut is "none".
#' 
#' @seealso \code{\link[rkt]{rkt}}.
#' 
#' @examples
#' \dontrun{
#' require("raster")
#' set.seed(6)
#' 
#' temp <- list()
#' for(i in 1:100) {
#' temp[[i]] <- runif(36,-1, 1)
#' temp[[i]] <- matrix(temp[[i]], 6, 6)
#' temp[[i]] <- raster(temp[[i]])
#'}
#'
#'temp <- brick(temp)
#'
#'
#'writeRaster(temp,"temporal.tif", overwrite=T)
#'rm(temp)
#'ndvisim <- brick("temporal.tif")
#'
#'date <- seq(from = 1990.1, length.out = 100, by = 0.2)
#'
#'eco.theilsen(ndvisim, date)
#'
#'slope <- raster("slope.tif")
#'pvalue <- raster("pvalue.tif")
#'
#'par(mfrow = c(1, 2))
#'plot(slope, main = "slope")
#'plot(pvalue, main = "p-value")
#'
#'
#' file.remove(c("temporal.tif", "slope.tif", "pvalue.tif"))
#'}
#'
#' @references 
#' Sen, P. 1968. Estimates of the regression coefficient based on Kendall's tau. 
#' Journal of the American Statistical Association, Taylor and Francis Group, 63: 1379-1389.
#' 
#' Theil H. 1950. A rank-invariant method of linear and polynomial regression analysis, 
#' Part 3 Proceedings of Koninalijke Nederlandse Akademie van Weinenschatpen A, 53: 397-1412.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export

setGeneric("eco.theilsen", 
           function(stacked, date, 
                    adjust = "none") {

  adjust <- match.arg(adjust)
             
             
  cat("starting...", "\n\n")
             
  # pre allocate memory
  cellnumber <- ncell(stacked)
  cat("Pre allocating memory...\n")
  ts <- pval <- rep(NA, ncell(stacked))
 
  # compute slope and p value
  
   for(i in seq_len(ncell(stacked))) {
    temporal <- stacked[i]
    this_result <- rkt::rkt(date, temporal)
    ts[i] <- this_result[3]
    pval[i] <- this_result[1]
    
    cat ("\r", ceiling(100 * i / cellnumber), "% ", "completed", sep = "")
   }
  cat("\n")
  
  r <- pout <-  raster(nrow = nrow(stacked), ncol = ncol(stacked), crs = crs(stacked))
  extent(r) <- extent(pout) <- extent(stacked)
  
  r[] <- unlist(ts)
  
  if(adjust != "none") {
    cat(paste("adjusting p values with", adjust, "method"), "\n\n")
    pval <- p.adjust(pval, method = adjust)
  }
  pout[] <- unlist(pval)
  
  # write output
  cat("writing slope image into workspace...", "\n\n")
  raster::writeRaster(r, "slope.tif", overwrite = T)
  cat("writing P-value image into workspace...", "\n\n")
  raster::writeRaster(pout, "pvalue.tif", overwrite = T)
  cat("\n","done!","\n\n" )

})

