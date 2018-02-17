
#' Conversion form ecogen to ecopop 
#' 
#' @description This function creates an ecopop object from an ecogen object
#' @param from Object of class "ecogen"
#' @param hier Name of the level of the slot S with hierarchies
#' @param factor_to_counts Convert factors into counts for each level?
#' @param aggregator Function used to aggregate data
#' @rdname ecogen2ecopop
#' @examples
#' 
#' \dontrun{
#'
#' data(eco.test)
#' ecogen2ecopop(eco, hier = "pop")
#'
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

setGeneric("ecogen2ecopop", function(from, hier, 
                                     factor_to_counts = TRUE,
                                     aggregator = function(x) mean(x, na.rm = TRUE)) { 
  
which_pop <- which(colnames(from@S) == hier)
if(length(which_pop) == 0) {
  stop("non matching S column name")
}

pop <- from@S[, hier]

to <- new("ecopop", ploidy = from@INT@ploidy, type = from@INT@type)

to@XY <-  aue.aggregated_df(from@XY, pop, aggregator, factor_to_counts = FALSE)

to@P <-   aue.aggregated_df(from@P, pop, aggregator, factor_to_counts = factor_to_counts )

if(from@INT@type == "codominant") {
  to@AF <- as.matrix(apply(from@A, 2, tapply, pop, sum, na.rm = TRUE))
} else {
  to@AF <-  as.matrix(apply(from@G, 2, tapply, pop, sum, na.rm = TRUE))
}

to@E <-  aue.aggregated_df(from@E, pop, aggregator, factor_to_counts = factor_to_counts)

to@S <-  data.frame(pop = factor(levels(pop)))

to@C <-   aue.aggregated_df(from@C, pop, aggregator, factor_to_counts = factor_to_counts)


popdat <- new("int.popdata")
popdat@ploidy <- from@INT@ploidy
popdat@type <- from@INT@type
popdat@aggregator <- aggregator
popdat@factor_to_counts <- factor_to_counts
popdat@loc.fac <- from@INT@loc.fac
popdat@all.names <- from@INT@all.names
to@INT <- popdat

# set attributes
to@ATTR$names <- levels(pop)
to@ATTR$whereIs <- parent.frame()
to@ATTR$.call <- match.call()

to
})


#' Conversion form ecopop to genpop and genpop to ecopop
#' 
#' @description These functions export from ecopop to genpop and viceversa
#' @param from Object of class "ecopop" / "genpop"
#' @rdname ecopop2genpop
#' @examples
#' 
#' \dontrun{
#' data(eco.test)
#' my_ecopop <- ecogen2ecopop(eco, hier = "pop")
#' my_genpop <- ecopop2genpop(my_ecopop)
#' my_ecopop2 <- genpop2ecopop(my_genpop)
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("ecopop2genpop", function(from) { 
  
  if(!require(adegenet)) stop("Please install the adegenet package first")
  
  to <- adegenet::genpop()
  
  if(!any(dim(from@XY) == 0)) {
    to@other$xy <- from@XY
  }
  
  if(!any(dim(from@AF) == 0)) {
    to@tab <- from@AF
    to@loc.fac <- from@INT@loc.fac
    nomloc <- levels(from@INT@loc.fac)
    temp<- tapply(rep(1, length(from@INT@loc.fac)), from@INT@loc.fac, sum)
    #array to list ("as.list()" do not works with the array)
    temp <- as(temp, "numeric")
    names(temp) <- nomloc
    to@loc.n.all <- temp
    temp <-  tapply(from@INT@all.names, names(from@INT@all.names), function(x) return(unname(x)),simplify = FALSE)
    #reorder temp an convert to list
    temp <- temp[pmatch(nomloc, names(temp))]
    nomloc <- names(temp)
    temp <- as(temp, "list")
    names(temp) <- nomloc
    to@all.names <- temp
    to@ploidy <- rep(from@INT@ploidy, nrow(from@AF))
    to@type <- ifelse(from@INT@type == "codominant", "codom", "PA")
  }

  to
})

#' genpop2ecpop
#' @rdname ecopop2genpop
#' @export


setGeneric("genpop2ecopop", function(from) { 
  
  if(!require(adegenet)) stop("Please install the adegenet package first")
  
  this_ploidy <- unique(from@ploidy)
  if(length(this_ploidy) > 1) {
    stop("multiple ploidy levels are not supported by ecopop objects")
  }
  
  to <- ecopop(AF = from@tab, S = data.frame(pop = as.factor(rownames(from@tab))), 
               ploidy = this_ploidy,
               type =  ifelse(from@type == "codom", "codominant", "dominant"))
  to@INT@loc.fac <- from@loc.fac
  
  counts <- lapply(from@all.names, length)
  xnames <- names(from@all.names)
  xnames <- rep(xnames, counts)
  to@INT@all.names <- unlist(from@all.names)
  names(to@INT@all.names) <- xnames

  if(!is.null(from@other$xy)) {
    to@XY <- from@other$xy
  }
  
  to@ATTR$names <- rownames(from@tab)
  to@ATTR$whereIs <- parent.frame()
  to@ATTR$.call <- match.call()

  to
})
