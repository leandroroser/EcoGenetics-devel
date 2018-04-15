
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
#' @exportMethod names

setMethod("names", "ecopop",
          function(x){
            if(is.locked(x)) {
              x@ATTR$names
            } else {
              cat("Free rows object, with empty 'names' attribute\n")
              invisible(NULL)
            }
          })


# names<- -----------------------------------------------------------------------#
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases names,ecopop-method
#' @exportMethod names<-

setReplaceMethod("names", c(x = "ecopop", value = "any_vector"), function(x, value) {
  
    if(!is.locked(x)) {
      stop("Free rows object, with empty 'names' attribute\n")
    }
      
    if(length(x@ATTR$names) != length(value)) {
      stop("Length of input names different of the length of the names present in the object")
    }
    
    if(length(value) != length(unique(value))) {
      stop("Duplicate values found in the input names")
    }
    
    nrow_data <- nrow(x)
    x@ATTR$names <- value
    
    if(nrow_data["XY"] != 0) {
      rownames(x@XY) <- value
    }
    
    if(nrow_data["P"] != 0) {
      rownames(x@P) <- value
    }
    
    if(nrow_data["AF"] != 0) {
      rownames(x@AF) <- value
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
#' @exportMethod nrow

setMethod("nrow", "ecopop",
          function(x){
            c(XY = nrow(x@XY), 
              P = nrow(x@P),
              AF = nrow(x@AF), 
              E = nrow(x@E),  
              S = nrow(x@S), 
              C = nrow(x@C))
          })

# ncol -----------------------------------------------------------------------#

#' ncol
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases ncol,ecopop-method
#' @exportMethod ncol

setMethod("ncol", "ecopop",
          function(x){
            c(XY=ncol(x@XY), 
              P=ncol(x@P), 
              AF=ncol(x@AF), 
              E=ncol(x@E),
              S = ncol(x@S),
              C=ncol(x@C))
          })

# dim -----------------------------------------------------------------------#

#' dim
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases dim,ecopop-method
#' @exportMethod dim

setMethod("dim", "ecopop",
          function(x) {
            list(XY=c(nrow(x@XY), ncol(x@XY)), 
                 P=c(nrow(x@P), ncol(x@P)), 
                 AF=c(nrow(x@AF), ncol(x@AF)), 
                 E=c(nrow(x@E), ncol(x@E)), 
                 S=c(nrow(x@S), ncol(x@S)), 
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
            to$S <- x@S
            to$C <- x@C
            return(to)
          })

# Show-------------------------------------------------------------------------#
#' show method
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal 
#' @rdname ecopop-methods
#' @aliases show,ecopop-method
#' @exportMethod show

setMethod("show", 
          "ecopop", 
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
            l5 <- paste(nrow(object@S), "x", ncol(object@S))
            l5.1 <-  ifelse(ncol(object@S) > 1 || ncol(object@S) == 0, 
                            "populations found", 
                            "population found")
      
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


#' Test if rows of an ecopop object are locked
#' @description  Test if rows of an ecopop object are locked 
#' @param object ecopop object
#' @aliases is.locked,ecopop
#' @examples
#' \dontrun{
#' data(eco.test)
#' is.locked(my_ecopop) 
#' eco2 <- eco.unlock(eco)
#' is.locked(eco2) 
#' }
#' @exportMethod is.locked

setMethod("is.locked", "ecopop", 
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

#' Update an old ecogen or ecopop object to a version compatible with EcoGenetics >= 1.5.0-1
#' @description Update an old ecogen or ecopop object to a version compatible with EcoGenetics >= 1.5.0-1
#' @param object ecopop object
#' @aliases eco.old2new,ecopop
#' @exportMethod eco.old2new

setMethod("eco.old2new", "ecopop", 
          function(object) {
            
            ver <- as.numeric(gsub("[.]|-", "", object@ATTR$ver))
            
            if(ver < 1215 || is.null(object@ATTR$ver)) {
              
              out <- new("ecopop", ploidy = object@INT@ploidy, type = object@INT@type)
              out@XY <- object@XY
              out@P <- object@P
              out@AF <- object@AF
              out@E <- object@E
              out@S <- object@S
              out@C <- object@C
              out@INT <- object@INT
              out@ATTR$names <- object@ATTR$names
              out@ATTR$lock.rows <- TRUE
              out@ATTR$whereIs <- object@ATTR$whereIs 
              out@ATTR$ver <- utils::packageDescription("EcoGenetics", fields = "Version")
              out@ATTR$.call <- match.call()
            } else {
              message("The object is already compatible with EcoGenetics >= 1.5.0-1")
            }
            out
          })



#' Lock rows in an ecogen object
#' @description  This methods locks the rows in an ecogen object.  When rows are locked,
#' the object requires rows with identical indviduals in the non empty data frames, and
#' identity in the row names of the data frames.
#' @param object ecopop object
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
#' @aliases eco.lock,ecopop
#' @examples
#' \dontrun{
#' data(eco.test)
#' my_ecopop2 <- eco.unlock(my_ecopop)
#' is.locked(my_ecopop2) 
#' my_ecopop3 <- eco.lock(my_ecopop)
#' is.locked(my_ecopop3) 
#' }
#' 
#'@exportMethod eco.lock

setMethod("eco.lock", "ecopop", 
          function(object, set.names = NULL, valid.names = FALSE, order.df = FALSE) {
  
  object.names <- list(XY=rownames(object@XY), 
                       P=rownames(object@P), 
                       AF=rownames(object@AF), 
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
      if(nrow(object@AF) != 0) {
        object@ATTR$names <- object.names$AF
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
      
      object@ATTR$names <- the.names
      
    } 
  }
  
  object@ATTR$lock.rows <- TRUE
  # check validity 
  validObject(object)
  
  object
})

#' Unlock rows in an ecogen object
#' @description  This methods unlocks the rows in an ecogen object. This means that 
#' different data frames in the object can have different rows, with different row names.
#' @param object ecopop object
#' @aliases eco.unlock,ecopop
#' @examples
#' \dontrun{
#' data(eco.test)
#' my_ecopop2 <- eco.unlock(my_ecopop)
#' is.locked(my_ecopop2) 
#' my_ecopop3 <- eco.lock(my_ecopop)
#' is.locked(my_ecopop3) 
#' }
#' @exportMethod eco.unlock

setMethod("eco.unlock", "ecopop", function(object) {
  object@ATTR$names <- list(character(0))
  object@ATTR$lock.rows <- FALSE
  object
})
