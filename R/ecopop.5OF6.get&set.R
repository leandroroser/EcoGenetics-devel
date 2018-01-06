################################################
#### GETTERS AND SETTERS
################################################

#--------------------------------------------------------------------#

#' Generic accessors for EcoGenetics objects
#' @name  EcoGenetics accessors
#' @rdname EcoGenetics-accessors
#' @param object Object of class ecopop.
#' @param value Single object or a list of objects to assign. Multiple 
#' objects v1, v2, ...vn must be passed as a list : list(v1, v2, ...vn).
#' @param ... Arguments for G or OUT slots of ecopop objects (see Details).
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
#' @export 

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.XY

setMethod("ecoslot.XY", "ecopop", function(X) X@XY)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.XY<-

setReplaceMethod("ecoslot.XY", "ecopop", function(object, value, order.rows = FALSE) {
  
  
  object@XY <- as.data.frame(value)
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  } else {
    object@S <- factor(rownames(value))
  }
  
  #check validity
  validObject(object)
  
  object
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.P

setMethod("ecoslot.P", "ecopop", function(X) X@P)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.P<-

setReplaceMethod("ecoslot.P", "ecopop", function(object, value, order.rows = FALSE) {
  
  
  object@P <- as.data.frame(value)
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  } else {
    object@S <- factor(rownames(value))
  }
  
  #check validity
  validObject(object)
  
  object
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.AF

setMethod("ecoslot.AF", "ecopop", function(X) X@AF)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.AF<-

setReplaceMethod("ecoslot.AF", "ecopop", function(object, value, 
                                                  type = c("codominant", "dominant"), 
                                                  ploidy, order.rows = FALSE) {
  
  type <- match.arg(type)
  object@AF <- as.matrix(value)
  mode(object@AF) <- "integer"
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  } else {
    object@S <- factor(rownames(value))
  }
  
  object@INT@type <- type
  object@INT@ploidy <- ploidy
  #check validity
  validObject(object)
  
  object
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.E

setMethod("ecoslot.E", "ecopop", function(X) X@E)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.E<-

setReplaceMethod("ecoslot.E", "ecopop", function(object, value, order.rows = FALSE) {
  
  
  object@E <- as.data.frame(value)
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  } else {
    object@S <- factor(rownames(value))
  }
  
  #check validity
  validObject(object)
  
  object
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.S

setMethod("ecoslot.S", "ecopop", function(X) X@S)

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.S<-


# ACA HAY QUE VER QUE SE HACE 
# PARA QUE NO CAMBIE TODO PORQUE ES EL NOMBRE!!
setReplaceMethod("ecoslot.S", "ecopop", function(object, value, order.rows = FALSE) {
  
  object@S <- as.factor(value)
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  }
  
  #check validity
  validObject(object)
  
  object
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.C

setMethod("ecoslot.C", "ecopop", function(X) X@C)

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.C<-

setReplaceMethod("ecoslot.C", "ecopop", function(object, value, order.rows = FALSE) {
  
  
  object@C <- as.data.frame(value)
  
  if(length(object@S) != 0) {
    if(order.rows) object <- int.order(object)
  } else {
    object@S <- factor(rownames(value))
  }
  
  #check validity
  validObject(object)
  
  object
})

