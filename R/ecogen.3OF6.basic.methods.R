################################################
#### BASIC METHODS
################################################

# Initialize ------------------------------------------------------------------#

setMethod("initialize", "ecogen", 
          function(.Object) {
            .Object
          })

# Names -----------------------------------------------------------------------#

#' names 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases names,ecogen-method
#' @exportMethod names

setMethod("names", "ecogen",
          function(x){
            if(is.locked(x)) {
            x@ATTR$names
            } else {
              cat("Free rows object, with empty 'names' attribute\n")
              invisible(NULL)
            }
          })

#' names<- 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases names,ecogen-method
#' @exportMethod names<-

setReplaceMethod("names", c(x = "ecogen", value = "character"), function(x, value) {
  
  if(!is.locked(x)) {
    stop("Free rows object, with empty 'names' attribute\n")
  }
  
  if(length(x@ATTR$names) != length(value)) {
    stop("Length of input names different of the length of the names present in the object")
  }
  
  if(length(value) != length(unique(value))) {
    stop("Duplicate values found in the input names")
  }
  
  x@ATTR$names <- value
  nrow_data <- nrow(x)
 
  if(nrow_data["XY"] != 0) {
    rownames(x@XY) <- value
  }
  
  if(nrow_data["P"] != 0) {
    rownames(x@P) <- value
  }
  
  if(nrow_data["G"] != 0) {
    rownames(x@G) <- value
  }
  
  if(x@INT@type == "codominant") {
    if(nrow_data["A"] != 0) {
      rownames(x@A) <- value
    }
  }
  
  if(nrow_data["E"] != 0) {
    rownames(x@E) <- value
  }
  
  if(nrow_data["S"] != 0) {
    rownames(x@S) <- value
  }
  
  if(nrow_data["C"] != 0) {
    rownames(x@C) <- value
  }

  return(x)
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
#' @exportMethod nrow

setMethod("nrow", "ecogen",
          function(x){
            c(XY=nrow(x@XY), P=nrow(x@P), G=nrow(x@G), A=nrow(x@A), E=nrow(x@E), S=nrow(x@S), C=nrow(x@C))
          })

# ncol -----------------------------------------------------------------------#

#' ncol
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases ncol,ecogen-method
#' @exportMethod ncol

setMethod("ncol", "ecogen",
          function(x){
            c(XY=ncol(x@XY), P=ncol(x@P), G=ncol(x@G), A=ncol(x@A), E=ncol(x@E), S=ncol(x@S), C=ncol(x@C))
          })

# dim -----------------------------------------------------------------------#

#' dim
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecogen-methods
#' @aliases dim,ecogen-method
#' @exportMethod dim

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
#' @exportMethod show

setMethod("show", 
          "ecogen", 
          function(object) {

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


#' Test if rows of an ecogen object are locked
#' @description  Test if rows of an ecogen object are locked 
#' @aliases is.locked,ecogen
#' @exportMethod is.locked

setMethod("is.locked", "ecogen", 
          function(object) {
            if(object@ATTR$ver < '1.2.1-5' || is.null(object@ATTR$ver)) {
              out <- TRUE
            } else {
              if(object@ATTR$lock.rows) {
                out <- TRUE
              } else {
                out <- FALSE
              }
            }
            out
            })


#' Update an old ecogen or ecopop object to version  >= 1.5.0-1
#' @description Update an old ecogen or ecopop object to version  >= 1.5.0-1
#' @aliases eco.old2new,ecogen
#' @exportMethod eco.old2new

setMethod("eco.old2new", "ecogen", 
          function(object) {
            
            ver <- as.numeric(gsub("[.]|-", "", object@ATTR$ver))
            
            if(ver < 1215 || is.null(object@ATTR$ver)) {
              
              out <- new("ecogen")
              out@XY <- object@XY
              out@P <- object@P
              out@G <- object@G
              out@A <- object@A
              out@E <- object@E
              out@S <- object@S
              out@C <- object@C
              out@OUT <- object@OUT
              out@INT <- object@INT
              out@ATTR$names <- object@ATTR$names
              out@ATTR$lock.rows <- TRUE
              out@ATTR$whereIs <- object@ATTR$whereIs 
              out@ATTR$ver <- utils::packageDescription("EcoGenetics", fields = "Version")
              out@ATTR$.call <- match.call()
            } else {
              message("The object was already created with EcoGenetics >= 1.5.0-1")
            }
            out
          })


#' Lock rows in an ecogen object
#' @description  This methods locks the rows in an ecogen object.  When rows are locked,
#' the object requires rows with identical indviduals in the non empty data frames, and
#' identity in the row names of the data frames.
#' @param set.names Character vector with names for the rows of the non-empty data frames. 
#' This argument is incompatible with valid.names
#' @param valid.names Logical. Create valid row names? This argument is incompatible with 
#' set.names. The program will name individuals with valid tags I.1, I.2, etc.
#' @param order.df Order individuals of data frames by row? (all data frames with a same order in row names).
#'  This option is only available when the 'lock.rows' parameter is TRUE. 
#' If the names of the data frames are not used (i.e., set.names and valid.names are not NULL),
#' setting this parameter to TRUE/FALSE has no effect in the function. 
#' Defalut TRUE. If FALSE, the row names of all the data frames must be ordered. The use of data frames 
#' with row names in different order will return an error.
#' In both cases, the program sets an internal names attribute of the object
#' using the row names of the first non-empty data frame found in the following order: 
#' XY, P, G, E, S, C. This attribute is used as reference to order rows when order.df = TRUE. 
#' @export

eco.lock <- function(object, set.names = NULL, valid.names = FALSE, order.df = FALSE) {
  
  object.names <- list(XY=rownames(object@XY), 
                       P=rownames(object@P), 
                       G=rownames(object@G), 
                       A=rownames(object@A),
                       E=rownames(object@E),
                       S=rownames(object@S), 
                       C=rownames(object@C))
  
  # set names--------------------------------------------
  # case: use data frames names---->
  if(is.null(set.names) && !valid.names) {
    
    while(TRUE) {
      
      if(nrow(object@XY) != 0) {
        object@ATTR$names <- object.names$XY
        break
      }
      if(nrow(object@P) != 0) {
        object@ATTR$names <- object.names$P
        break
      }
      if(nrow(object@G) != 0) {
        object@ATTR$names <- object.names$G
        break
      }
      if(nrow(object@E) != 0) {
        object@ATTR$names <- object.names$E
        break
      }
      if(nrow(object@S) != 0) {
        object@ATTR$names <- object.names$S
        break
      }
      if(nrow(object@C) != 0) {
        object@ATTR$names <- object.names$C
        break
      }
      object@ATTR$names <- character(0)
      break
    }
    
    # order rows
    if(order.df) {
      object <- int.order(object)
    }
    
    # case: use set.names or valid.names---->
  } else {
    # use nrow method
    
    object.names <- object.names[unlist(lapply(object.names,
                                               function(x) length(x)  != 0))]
    
    if(length(object.names) != 0) {
      rownumber <- unique(unlist(lapply(object.names, length)))
      # check nrow consistency
      if(length(rownumber)> 1) {
        stop("Non unique row number found")
      }
      
      # set.names case --
      if(!is.null(set.names)) {
        
        #check length consistency
        if(length(set.names) != rownumber) {
          stop("the length of valid.names do not match 
               with the number of rows in the object")
        }
        
        the.names <- set.names
        
        # valid.names case --
        } else if(valid.names) {
          the.names <- paste0("I.", seq_len(rownumber))
        }
      
      # set data frames names and object names --
      for(i in names(object.names)) {
        eval(expr = parse(text=paste0("rownames(object@", i, ") <- the.names")))
      }
      
      object@ATTR$names <- list(the.names)
      
    } 
  }
  
  object@ATTR$lock.rows <- TRUE
  # check validity 
  validObject(object)
  
  object
}

#' Unlock rows in an ecogen object
#' @description  This methods unlocks the rows in an ecogen object. This means that 
#' different data frames in the object can have different rows, with different row names.
#' @export

eco.unlock <- function(object) {
  object@ATTR$names <- list(character(0))
  object@ATTR$lock.rows <- FALSE
  object
}