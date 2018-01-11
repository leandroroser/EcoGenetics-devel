################################################
#### GETTERS AND SETTERS
################################################
t 

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

setReplaceMethod("ecoslot.AF", "ecopop", function(object, value, order.rows = FALSE) {
  
  object@AF <- as.matrix(value)
  mode(object@AF) <- "integer"
  
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

