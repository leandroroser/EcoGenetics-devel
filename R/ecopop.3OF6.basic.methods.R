################################################
#### BASIC METHODS
################################################

# Initialize ------------------------------------------------------------------#

setMethod("initialize", "ecopop", 
          function(.Object, ploidy, type) {
            .Object@INT@ploidy <- as.integer(ploidy)
            .Object@INT@type <- type
            .Object
          })

# Names -----------------------------------------------------------------------#

#' names 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases names,ecopop-method

setMethod("names", "ecopop",
          function(x){
            return(x@S)
          })



# is --------------------------------------------------------------------------#
#' is.ecopop 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases is,ecopop-method
#' @export is.ecopop

is.ecopop <- function(x) {
  value <- is(x, "ecopop")
  value
}

# nrow -----------------------------------------------------------------------#

#' nrow
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases nrow,ecopop-method

setMethod("nrow", "ecopop",
          function(x){
            c(XY=nrow(x@XY), P=nrow(x@P), AF=nrow(x@AF), E=nrow(x@E), C=nrow(x@C))
          })

# ncol -----------------------------------------------------------------------#

#' ncol
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases ncol,ecopop-method

setMethod("ncol", "ecopop",
          function(x){
            c(XY=ncol(x@XY), 
              P=ncol(x@P), 
              AF=ncol(x@AF), 
              E=ncol(x@E),
              C=ncol(x@C))
          })

# dim -----------------------------------------------------------------------#

#' dim
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases dim,ecopop-method

setMethod("dim", "ecopop",
          function(x){
            list(XY=c(nrow(x@XY), ncol(x@XY)), 
                 P=c(nrow(x@P), ncol(x@P)), 
                 AF=c(nrow(x@AF), ncol(x@AF)), 
                 E=c(nrow(x@E), ncol(x@E)), 
                 C=c(nrow(x@C), ncol(x@C)))
          })



# as.list----------------------------------------------------------------------#
#' as.list
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @param x Object of class ecopop
#' @rdname ecopop-methods 
#' @aliases as.list,ecopop-method
#' @export
#' @method as.list ecopop

setMethod("as.list", 
          signature(x = "ecopop"), 
          function(x) {
            to <- list()
            to$XY <- x@XY
            to$P <- x@P
            to$AF <- x@AF
            to$E <- x@E
            to$C <- x@C
            return(to)
          })

# Show-------------------------------------------------------------------------#
#' show method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal 
#' @rdname ecopop-methods
#' @aliases show,ecopop-method

setMethod("show", 
          "ecopop", 
          function(object) {
            # check validity using a temporal element to pass environment
            validObject(object)

            
            #--space function--------------------------------------#
            
            e <- function(x) rep("", x)
            
            #-----------------------------------------------------#
            
            #SLOTS CONFIGURATION----------------------------------#
            #---XY slot---
            l1 <- paste(nrow(object@XY), "x", ncol(object@XY))
            l1.1 <- ifelse(ncol(object@XY) > 1 || ncol(object@XY) == 0 , "coordinates", "coordinate")
            #---P slot---
            l2 <- paste(nrow(object@P), "x", ncol(object@P))
            l2.1 <- ifelse(ncol(object@P) > 1 || ncol(object@P) == 0, "phenotypic variables", "phenotypic variable")
            #---G slot---
            l3 <- paste(nrow(object@AF), "x", ncol(object@AF))
            l3.1 <- ifelse(ncol(object@AF) > 1 || ncol(object@AF) == 0, "loci", "locus")
            
            if(ncol(object@AF) != 0) {
              ploidy <- object@INT@ploidy
              type <- object@INT@type 
              g.info <- paste(">>", "ploidy:", ploidy,"||", type)
            } else {
              g.info <- ""
            }
            
            #---E slot---
            l4 <- paste(nrow(object@E), "x", ncol(object@E))
            l4.1 <-  ifelse(ncol(object@E) > 1 || ncol(object@E) == 0, 
                            "environmental variables", "environmental variable")
            #---S slot---
            l5 <- paste(length(object@S))
            l5.1 <- paste(">>", length(object@S), 
                            ifelse(length(object@S) > 1, 
                                   "populations found", 
                                   "population found"))
      
            #---C slot---
            l6 <- paste(nrow(object@C), "x", ncol(object@C))
            l6.1 <-  ifelse(ncol(object@C) > 1 || ncol(object@C) == 0, "variables", "variable")
            
  
            
            .msgaccess <- function() {
              cat("****************************************************************************\n")
              cat(" Access to slots:",
                  "<ecoslot.> + <name of the slot> + <(name of the object)>","\n",
                  "See: help(\"EcoGenetics accessors\")\n")
              cat("****************************************************************************\n")
              
            }
            
            #------#
            
            #GRAPHICAL INTERFACE----------------------------------#
            cat("\n")
            cat("                   << ECOPOP CLASS OBJECT >>")
            cat("\n")
            .msgaccess()
            cat( "\n", "#", "slot XY:", e(3), "#","=> ", l1, e(1), e(13 - nchar(l1)), l1.1)
            cat( "\n", "#", "slot P:", e(4), "#", "=> ", l2, e(1), e(13 - nchar(l2)), l2.1)
            cat( "\n", "#", "slot AF:", e(3), "#", "=> ", l3, e(1), e(13 - nchar(l3)), l3.1,  
                 e(1), e(11 - nchar(l3.1)), g.info)
            cat( "\n", "#", "slot E:", e(4), "#", "=> ", l4, e(1), e(13 - nchar(l4)), l4.1)
            cat("\n", "#", "slot S:", e(4), "#", "=> ", l5, e(1), e(13 - nchar(l5)), l5.1)
            cat("\n", "#", "slot C:", e(3), "", "#", "=> ", l6, e(1), e(13 - nchar(l6)),  l6.1)
            cat("\n****************************************************************************\n")
          })
