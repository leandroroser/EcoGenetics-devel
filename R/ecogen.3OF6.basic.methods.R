################################################
#### BASIC METHODS
################################################

# Names -----------------------------------------------------------------------#

#' names 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases names,ecogen-method

setMethod("names", "ecogen",
          function(x){
            return(x@ATTR$names)
          })

#' names<- 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases names,ecogen-method

setReplaceMethod("names", c(x="ecogen", value="character"), function(x, value) {
  x@ATTR$names <- value
  out <- try(int.order(x), silent = TRUE)
  if(attr(out, "class") == "try-error") {
    warning("Names can not be set. Incongruence with row names of data frames")
  }
  out
})


# is --------------------------------------------------------------------------#
#' is.ecogen 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods 
#' @aliases is,ecogen-method
#' @export is.ecogen

is.ecogen <- function(x) {
  value <- is(x, "ecogen")
  value
}

# nrow -----------------------------------------------------------------------#

#' nrow
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases nrow,ecogen-method

setMethod("nrow", "ecogen",
          function(x){
            c(XY=nrow(x@XY), P=nrow(x@P), G=nrow(x@G), A=nrow(x@A), E=nrow(x@E), S=nrow(x@S), C=nrow(x@C))
          })

# ncol -----------------------------------------------------------------------#

#' ncol
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases ncol,ecogen-method

setMethod("ncol", "ecogen",
          function(x){
            c(XY=ncol(x@XY), P=ncol(x@P), G=ncol(x@G), A=ncol(x@A), E=ncol(x@E), S=ncol(x@S), C=ncol(x@C))
          })

# dim -----------------------------------------------------------------------#

#' dim
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases dim,ecogen-method

setMethod("dim", "ecogen",
          function(x){
            list(XY=c(nrow(x@XY), ncol(x@XY)),  P=c(nrow(x@P), ncol(x@P)), 
            G=c(nrow(x@G), ncol(x@G)), A=c(nrow(x@A), ncol(x@A)), 
            E=c(nrow(x@E), ncol(x@E)), S=c(nrow(x@S),ncol(x@S)),
            C=c(nrow(x@C), ncol(x@C)))
          })



# as.list----------------------------------------------------------------------#
#' as.list
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @param x Object of class ecogen
#' @rdname ecogen-methods 
#' @aliases as.list,ecogen-method
#' @export
#' @method as.list ecogen

setMethod("as.list", 
          signature(x = "ecogen"), 
          function(x) {
            to <- list()
            to$XY <- x@XY
            to$P <- x@P
            to$G <- x@G
            to$A <- x@A
            to$E <- x@E
            to$S <- x@S
            to$C <- x@C
            to$OUT <- x@OUT
            return(to)
          })


# as.int.list------------------------------------------------------------------#
setGeneric("as.int.list", function(x, ...) standardGeneric("as.int.list"))

#' as.int.list
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @name as.int.list
#' @param x Object of class ecogen
#' @rdname ecogen-methods 
#' @keywords internal

# THIS IS AN INTERNAL METHOD. IT INCLUDES THE SLOT INT.

setMethod("as.int.list", 
          signature(x = "ecogen"), 
          function(x) {
            to <- list()
            to$XY <- x@XY
            to$P <- x@P
            to$G <- x@G
            to$A <- x@A
            to$E <- x@E
            to$S <- x@S
            to$C <- x@C
            to$OUT <- x@OUT
            to$INT <- x@INT
            return(to)
          })


# Show-------------------------------------------------------------------------#
#' show method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal 
#' @rdname ecogen-methods
#' @aliases show,ecogen-method

setMethod("show", 
          "ecogen", 
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
            l3 <- paste(nrow(object@G), "x", ncol(object@G))
            l3.1 <- ifelse(ncol(object@G) > 1 || ncol(object@G) == 0, "loci", "locus")
            
            if(ncol(object@G) != 0) {
              ploidy <- object@INT@ploidy
              type <- object@INT@type 
              g.info <- paste(">>", "ploidy:", ploidy,"||", type)
            } else {
              g.info <- ""
            }
            
            #---A slot---// for dominant data is empty and is not shown
            if(object@INT@type == "codominant") {
            l6 <- paste(nrow(object@A), "x", ncol(object@A))
            l6.1 <- ifelse(ncol(object@A) > 1 || ncol(object@A) == 0, "alleles", "allele")
            } else {
            l6 <- ""
            l6.1 <- ""
            }
            
            #---E slot---
            l4 <- paste(nrow(object@E), "x", ncol(object@E))
            l4.1 <-  ifelse(ncol(object@E) > 1 || ncol(object@E) == 0, 
                            "environmental variables", "environmental variable")
            #---S slot---
            l5 <- paste(nrow(object@S), "x", ncol(object@S))
            l5.1 <- ifelse(ncol(object@S) > 1 || ncol(object@S) == 0, "structures", "structure")
            if(ncol(object@S) != 0) {
              l5.2 <- paste(">>", ncol(object@S), 
                            ifelse(ncol(object@S) > 1, 
                                   "structures found", 
                                   "structure found"))
            } else { 
              l5.2 <- ""
            }
            
            #---C slot---
            l7 <- paste(nrow(object@C), "x", ncol(object@C))
            l7.1 <-  ifelse(ncol(object@C) > 1 || ncol(object@C) == 0, "variables", "variable")
            #---OUT slot---
            l8 <- paste(length(object@OUT))
            l8.1 <- ifelse(length(object@OUT) > 1 || length(object@OUT) == 0 , "results", "result")
            out.len<-length(object@OUT)
            
            
            if(out.len != 0) {
              
              l9 <- ifelse(length(object@OUT) > 1L, 
                           paste(length(object@OUT), "Results stored"),
                           "1 Result stored:")
              
            } else {
              l9 <- ""
            }
            
            .msgaccess <- function() {
              cat("----------------------------------------------------------------------------\n")
              cat(" Access to slots:",
                  "<ecoslot.> + <name of the slot> + <(name of the object)>","\n",
                  "See: help(\"EcoGenetics accessors\")\n")
              cat("----------------------------------------------------------------------------\n")
              
            }
            
            #------#
            
            #GRAPHICAL INTERFACE----------------------------------#
            cat("\n")
            cat("                   || ECOGEN CLASS OBJECT ||")
            cat("\n")
            .msgaccess()
            cat( "\n", "|", "slot XY:", e(3), "|","-->", l1, e(1), e(13 - nchar(l1)), l1.1)
            cat( "\n", "|", "slot P:", e(4), "|", "-->", l2, e(1), e(13 - nchar(l2)), l2.1)
            cat( "\n", "|", "slot G:", e(4), "|", "-->", l3, e(1), e(13 - nchar(l3)), l3.1,  
                 e(1), e(11 - nchar(l3.1)), g.info)
            if(object@INT@type == "codominant") { ## in dominant case A is empty
            cat( "\n", "|", "slot A:", e(4), "|", "-->", l6, e(1), e(13 - nchar(l6)), l6.1)
            }
            cat( "\n", "|", "slot E:", e(4), "|", "-->", l4, e(1), e(13 - nchar(l4)), l4.1)
            cat("\n", "|", "slot S:", e(4), "|", "-->", l5, e(1), e(13 - nchar(l5)), l5.1, 
                e(1), e(11 - nchar(l5.1)), l5.2)
            cat("\n", "|", "slot C:", e(3), "", "|", "-->", l7, e(1), e(13 - nchar(l7)),  l7.1)
            cat("\n", "|", "slot OUT:", e(2), "|", "-->", l8, e(1), e(13 - nchar(l8)), l8.1)
            cat("\n----------------------------------------------------------------------------\n")
          })


# Summary----------------------------------------------------------------------#
# UNDER DEVELOPMENT-------#

#' Summary for ecogen objects
#' @param object Object of class "ecogen".
#' @param x.in The name of the S slot column with the grouping factor 
#' when a summary taking in account groups is required.
#' @return Mean and sd table for the P data frame.
#' @return allelic frequencies for the genetic data.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' summary(eco, "structure")
#' }
#' 
# @rdname ecogen-summary
# @aliases summary,ecogen,ANY-method


#setMethod("summary", "ecogen", function(object, grp = NULL) { 
#cat("\n\n")

#cat(paste("==================================="), "\n")
#if(is.null(grp)) {
#cat(paste("P mean and sd","\n"))
#}
#cat(paste("==================================="), "\n\n")
#Pstats <- eco.meansd(object)
#print(Pstats$tab)
#cat(paste("-------------------------------------------"), "\n\n")


#cat(paste("========================================="), "\n")
#cat(paste("Allelic frequencies", "\n")
#cat(paste("========================================="), "\n\n")

#gensum <- aue.fqal(object, grp)
#print(gensum)


#resultados <- list(P_mean = Pstats$Pmean, P_sd = Pstats$Psd, 
#                   P_mean.and.sd = Pstats$tab, G_frequencies = gensum)
#
#invisible(resultados)
#})
