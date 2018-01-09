
################################################
#### GETTERS AND SETTERS
################################################

#--------------------------------------------------------------------#

#' Generic accessors for EcoGenetics objects
#' @name  EcoGenetics accessors
#' @rdname EcoGenetics-accessors
#' @param object Object of class ecogen.
#' @param value Single object or a list of objects to assign. Multiple 
#' objects v1, v2, ...vn must be passed as a list : list(v1, v2, ...vn).
#' @param ... Arguments for G or OUT slots of ecogen objects (see Details).
#' @param G.processed If TRUE, the slot G will include a processed data frame (
#' removed non informative loci (the data non available for all the individuals),
#' removed non polymorphic loci (for dominant data) and ordered alleles in ascending
#' order. 
#' @param order.G Genotypes must be ordered in G slot? Default FALSE.
#' @param type Marker type: "codominant" or "dominant".
#' @param ploidy Ploidy of the G data frame. Default ploidy = 2.
#' @param sep Character separating alleles. Default option is no
#' character separating alleles. 
#' @param ncod Number of characters coding each allele. 
#' @param missing Missing data treatment ("0", "NA", or "MEAN") for the A
#' slot. Missing elements are set to 0 in the default option. missing elements
#' are recoded as "NA" or the mean allelic frequency across individuals in "NA" 
#' and "MEAN" options, respectively. 
#' @param NA.char Character simbolizing missing data in the input. Default is "NA".
#' @param poly.level Polymorphism threshold in percentage (0 - 100), 
#' for remotion of non polymorphic loci (for dominant data). Default is 5 (5\%).
#' @param rm.empty.ind Remotion of noninformative individuals (rows of "NAs").
#' Default if FALSE.
#' @param use.object.names Logical. Use the names stored in the object 
#' for the assigned data frame? This argument can be combined with order.rows 
#' if the order of the individuals do not match to the order of the 
#' other elements in the object. If the names stored in the object are empty, 
#' in all the cases they will be set to the names of the assigned data frame.
#' @param order.rows Order rows of the object after assignnment to align individuals?
#' This argument can be used when use.object.names is TRUE. Otherwise, it will not have
#' effect in the function.
#' @author Leandro Roser \email{learoser@@gmail.com}
NULL


#' @rdname EcoGenetics-accessors
#' @export 
setGeneric("ecoslot.XY", function(X) standardGeneric("ecoslot.XY"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.XY<-", function(object, ..., value) standardGeneric("ecoslot.XY<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.P", function(X) standardGeneric("ecoslot.P"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.P<-", function(object, ...,  value) standardGeneric("ecoslot.P<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.G", function(X) standardGeneric("ecoslot.G"))


#' @rdname EcoGenetics-accessors
#' @export ecoslot.G<-

setGeneric("ecoslot.G<-", function(object, ..., value) 
  standardGeneric("ecoslot.G<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.A", function(X) standardGeneric("ecoslot.A"))

#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.A<-", function(object, value) standardGeneric("ecoslot.A<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.AF", function(X) standardGeneric("ecoslot.AF"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.AF<-", function(object, ..., value) standardGeneric("ecoslot.AF<-"))


#' @rdname EcoGenetics-accessors
#' @export

#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.E", function(X) standardGeneric("ecoslot.E"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.E<-", function(object,  ...,value) standardGeneric("ecoslot.E<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.S", function(X) standardGeneric("ecoslot.S"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.S<-", function(object, ...,  value) standardGeneric("ecoslot.S<-"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.C", function(X) standardGeneric("ecoslot.C"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.C<-", function(object,  ..., value) standardGeneric("ecoslot.C<-"))


#' @rdname EcoGenetics-accessors
#' @export 
  
setGeneric("ecoslot.OUT", function(X, ...) X@OUT)


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.OUT<-", function(object, value) standardGeneric("ecoslot.OUT<-"))


#' @rdname EcoGenetics-accessors
#' @keywords internal

# slot INT-Internal- non public accessor

setGeneric("int.ecoslot.INT", function(X) standardGeneric("int.ecoslot.INT"))
