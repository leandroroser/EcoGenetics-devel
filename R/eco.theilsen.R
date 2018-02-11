
#' Theil-sen regression for a raster time series, 
#' with parallelization available
#' 
#' @description This function computes the theil-sen estimator and 
#' the associated P-value, for each pixel over time in a stack of images.
#' The output consists of two rasters (one for the estimators and one for 
#' the P-values). It is recommended to use a "RasterBrick", which
#' is more efficient in memory management. 
#' The program can compute the result using parallel (default) or serial evaluation.
#' 
#' @param stacked Stacked images ("RasterLayer"  or "RasterBrick").
#' @param date Data vector with decimal dates for each image.
#' @param adjust P-values correction method for multiple tests.
#' passed to \code{\link[stats]{p.adjust}}. Defalut is "none".
#' @param run_parallel Run code in parallel? Default TRUE
#' @param workers Number of workers used for parallel evaulation. If NULL,
#' the program uses N - 1, where N is the total number of available 
#' logical cores. 
#' @param physical Use only physical cores for parallel evaluation? Default FALSE.
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
#'
#' # Parallel evaluation ----
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
#'file.remove(c("slope.tif", "pvalue.tif"))
#'
#'
#' # Serial evaluation ----
#' 
#'eco.theilsen(ndvisim, date)
#'
#'slope <- raster("slope.tif")
#'pvalue <- raster("pvalue.tif")
#'
#'par(mfrow = c(1, 2))
#'plot(slope, main = "slope")
#'plot(pvalue, main = "p-value")
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
  function(stacked, dates, 
           adjust = "none",
           run_parallel = TRUE,
           workers = NULL,
           physical = FALSE) {
             
    adjust <- match.arg(adjust)
    
    if(run_parallel) {
    detect_workers <- parallel::detectCores(logical =  ifelse(physical, FALSE, TRUE))
    if(is.null(workers)) {
      workers <- detect_workers - 1
    } else {
      if(workers > detect_workers) {
        stop("The maximum number of workers available is: ", detect_workers, ". 
              It is recommended to use ", detect_workers - 1, "cores")
      }
    }
    if(workers == 1) run_parallel <- FALSE
    }
   
    cat("Starting...", "\n")
    
    # pre allocate memory
    cellnumber <- raster::ncell(stacked)
   
 
    # compute slope and p value  in parallel
if(run_parallel) {

  # cat("Using ", foreach::getDoParWorkers(), " workers\n") # check the number of cores now running
  # cat("Using parallel backend: ", foreach::getDoParName(), "\n")

      # avoid exportation of all object to cluster with a local environment

        data <- new.env(parent=emptyenv())
        data$stacked <- stacked
        data$dates <- dates
        data$cellnumber <- cellnumber
        data$theilsen <- rkt::rkt

      cl <- parallel::makeCluster(workers, outfile = "")
      doParallel::registerDoParallel(cl, cores = workers)
      
      on.exit((function(){
        cat("Stopping cluster...\n")
        parallel::stopCluster(cl)
        doParallel::stopImplicitCluster()
        gc(); cat("done!\n")
        })())
  
      cat("Pre allocating memory...\n")
      cat("Exporting data to cluster and starting parallel evaluation...\n")
      
      parallel_ts <- function(data_) { 
      foreach::foreach(i=seq_len(data_$cellnumber),
                                 .combine = "rbind",
                                 .packages = NULL) %dopar% {
                    
        temp <- data_$stacked[i]
        if(sum(!is.na(temp)) < 4) {
          NA
        } else {
          cat("\r", ceiling(100 * i / data_$cellnumber), "% ", "completed\n", sep = "")
          (data_$theilsen(data_$dates, temp))[c(1,3)]
        }}
      }
      output <- parallel_ts(data)
      ts <- pval <- raster(nrow=nrow(data$stacked), ncol = ncol(data$stacked), crs=crs(data$stacked))
      raster::extent(ts) <- raster::extent(pval) <- raster::extent(data$stacked) 
      ts[] <- unlist(output[, 2])
        
      if(adjust != "none") {
          cat(paste("\nAdjusting p values with ", adjust, " method"), "\n")
          output[, 1] <- p.adjust(output[, 1], method = adjust)
        }
        
    pval[] <- unlist(output[, 1])

    } else {
      on.exit(cat("done!","\n"))
      cat("Using 1 worker\n")
      estimates <- pvalues <- rep(0, cellnumber)
      # compute slope and p value in series
      for(i in seq_len(cellnumber)) {
        temp <- stacked[i]
        if(sum(!is.na(temp)) < 4) {
          estimates[i] <- NA
          pvalues[i] <- NA
        } else {
          this_result <- rkt::rkt(dates, temp)
          estimates[i] <- this_result[[3]]
          pvalues[i] <- this_result[[1]]
        }
        cat ("\r", ceiling(100 * i / cellnumber), "% ", "completed", sep = "")
      }
      cat("\n")
      
      ts <- pval <- raster(nrow=nrow(stacked), ncol =ncol(stacked), crs=crs(stacked))
      extent(ts) <- extent(pval) <- extent(stacked) 
      
      if(adjust != "none") {
        cat(paste("Adjusting p values with ", adjust, " method"), "\n")
        pvalues <- p.adjust(pvalues, method = adjust)
      }
      
      
      ts[] <- estimates
      pval[] <- pvalues
    }
    
    
    # write output
    cat("Writing slope image into workspace...", "\n")
    raster::writeRaster(ts, "slope.tif", overwrite = T)
    cat("Writing P-value image into workspace...", "\n")
    raster::writeRaster(pval, "pvalue.tif", overwrite = T)
   
})

