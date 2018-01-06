
#' @rdname EcoGenetics-accessors
#' @export 
 
setGeneric("ecoslot.XY", function(X) standardGeneric("ecoslot.XY"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.XY<-", function(object, value, ...) standardGeneric("ecoslot.XY<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.P", function(X) standardGeneric("ecoslot.P"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.P<-", function(object, value, ...) standardGeneric("ecoslot.P<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.G", function(X) standardGeneric("ecoslot.G"))


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.G

setGeneric("ecoslot.G<-", function(object, value, ...) 
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

setGeneric("ecoslot.AF<-", function(object, value, ...) standardGeneric("ecoslot.AF<-"))


#' @rdname EcoGenetics-accessors
#' @export

#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.E", function(X) standardGeneric("ecoslot.E"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.E<-", function(object, value, ...) standardGeneric("ecoslot.E<-"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.S", function(X) standardGeneric("ecoslot.S"))


#' @rdname EcoGenetics-accessors
#' @export 

setGeneric("ecoslot.S<-", function(object, value, ...) standardGeneric("ecoslot.S<-"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.C", function(X) standardGeneric("ecoslot.C"))


#' @rdname EcoGenetics-accessors
#' @export

setGeneric("ecoslot.C<-", function(object, value, ...) standardGeneric("ecoslot.C<-"))


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
