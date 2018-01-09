################################################
#### GETTERS AND SETTERS
################################################

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.XY

setMethod("ecoslot.XY", "ecogen", function(X) X@XY)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.XY

setReplaceMethod("ecoslot.XY", "ecogen", function(object, 
                                                 value,
                                                 use.object.names = FALSE, 
                                                 order.rows = FALSE) {
  
  
  object@XY <- as.data.frame(value)
  
  if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
    rownames(object@XY) <- object@ATTR$names
  } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
    object <- int.order(object)
  } else if(length(object@ATTR$names) == 0) {
    object@ATTR$names <- rownames(value)
  }
  #check validity
  validObject(object)
  
  object
  
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.P

setMethod("ecoslot.P", "ecogen", function(X) X@P)

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.P

setReplaceMethod("ecoslot.P", "ecogen", function(object, 
                                                 value,
                                                 use.object.names = FALSE, 
                                                 order.rows = FALSE) {
  
  
  object@P <- as.data.frame(value)
  
  if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
    rownames(object@P) <- object@ATTR$names
  } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
    object <- int.order(object)
  } else if(length(object@ATTR$names) == 0) {
    object@ATTR$names <- rownames(value)
  }
  #check validity
  validObject(object)
  
  object
  
})

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.G

setMethod("ecoslot.G", "ecogen", function(X) X@G)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.G

setReplaceMethod("ecoslot.G", "ecogen",
                 function(object, value, G.processed = TRUE, order.G = FALSE, 
                          type = c("codominant", "dominant"),
                          ploidy = 2,sep,  ncod = NULL, missing = c("0", "NA", "MEAN"),
                          NA.char = "NA", poly.level = 5, rm.empty.ind = FALSE, 
                          use.object.names = FALSE, order.rows = FALSE) {
                   
                   # give flexibility to missing argument
                   if(length(missing) == 1 && is.na(missing)) {
                     missing <- "NA"
                   } 
                   if(length(missing) == 1 && missing == 0) {
                     missing <- "0"
                   }
                   missing <- match.arg(missing)
                   
                   type <- match.arg(type)
                   
                   if(missing(sep)) {
                     sep <- ""
                   }
                   
                   # coherence between data ploidy and ncod is checked for int.df2genind
                   
                   if(any(dim(value) == 0)) { # empty G
                     object@G <- data.frame()
                     object@A <- matrix(nrow = 0, ncol = 0)
                     object@INT <- new("int.gendata")
                     
                     
                   } else { # non empty G
                     
                     ## coherence between data ploidy and ncod is checked for int.df2genind
                     
                     tempo <- int.df2genind(as.data.frame(value), 
                                            sep = sep, 
                                            ncod =  ncod,
                                            NA.char = NA.char, 
                                            ploidy = ploidy, 
                                            type = type,
                                            missing = missing,
                                            rm.empty.ind = rm.empty.ind,
                                            poly.level = poly.level)
                     
                     # unfolding tempo
                     
                     ## if marker type is "dominant", A is a pointer to G for assignments
                     ## and extraction methods, and the slot is empty
                     if(tempo@type == "codominant") {
                       object@A <- tempo@tab
                     }
                   
                     object@INT <- int.genind2gendata(tempo)
                     
                     ncod <- tempo@ncod
                     ploidy <- tempo@ploidy
                     
                     # G processed case ~-~-~-~-~~-~-~-~-~
                     if(G.processed) {
                       tmp <- int.genind2df(tempo)
                       # order data
                       if(order.G) {
                         tmp <- aue.sort(tmp, 
                                         ncod = ncod,
                                         ploidy = ploidy, 
                                         chk.plocod = FALSE)
                       } 
                       
                       # G processed data frame
                       G <- as.data.frame(tmp, stringsAsFactors = FALSE)
                       
                       # G changes messages 
                       if(dim(tmp)[1] != dim(G)[1]) {
                         message("Note: removed noninformative individuals in slot G")
                       }
                       if(dim(tmp)[2] != dim(G)[2]) {
                         message("Note: removed noninformative loci in slot G")
                       }
                       if(order.G) {
                         message("Note: ordered genotypes in slot G")
                       }
                     } 
                     # END G processed case ~-~-~-~-~~-~-~-~-~
                     
                     # fill now the G slot
                     object@G <- G
                   }
                   
                     
                     if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
                       rownames(object@G) <- object@ATTR$names
                     } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
                       object <- int.order(object)
                     } else if(length(object@ATTR$names) == 0) {
                       object@ATTR$names <- rownames(value)
                     }
                     #check validity
                     validObject(object)
                     
                     object
                     
                   })


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.A

setMethod("ecoslot.A", "ecogen", function(X) {
  # DOMINANT / CODOMINANT MARKER DEPENDENT
  if(X@INT@type == "codominant") {
    return(X@A)
  } else {
    return(NULL)
  }
})



#' @rdname EcoGenetics-accessors
#' @keywords internal

setReplaceMethod("ecoslot.A", "ecogen", function(object, value) {
  stop("<A> slots can not be directly replaced. The <A> slot content
          is generated when a new (codominant) data frame is assigned to 
          the slot <G>")
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.E

setMethod("ecoslot.E", "ecogen", function(X) X@E)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.E

setReplaceMethod("ecoslot.E", "ecogen", function(object, 
                                                 value,
                                                 use.object.names = FALSE, 
                                                 order.rows = FALSE) {
  
  
  object@E <- as.data.frame(value)
  
  if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
    rownames(object@E) <- object@ATTR$names
  } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
    object <- int.order(object)
  } else if(length(object@ATTR$names) == 0) {
    object@ATTR$names <- rownames(value)
  }
  #check validity
  validObject(object)
  
  object
  
})


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.S

setMethod("ecoslot.S", "ecogen", function(X) X@S)


#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.S

setReplaceMethod("ecoslot.S", "ecogen", function(object, 
                                                 value,
                                                 use.object.names = FALSE, 
                                                 order.rows = FALSE) {
  
  value <- as.data.frame(value)
  if(dim(value)[1] != 0) {
    # better this way. 2016/04/01 L.R.
    value[] <- lapply(value, factor)
  #  for(i in 1:(ncol(value))) {
  #    value[, i] <- factor(value[, i])
  #  }
  }
    
    object@S <- as.data.frame(value)
    
    if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
      rownames(object@S) <- object@ATTR$names
    } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
      object <- int.order(object)
    } else if(length(object@ATTR$names) == 0) {
      object@ATTR$names <- rownames(value)
    }
    #check validity
    validObject(object)
    
    object
    
  })



#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.C

setMethod("ecoslot.C", "ecogen", function(X) X@C)



#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.C

setReplaceMethod("ecoslot.C", "ecogen", function(object, 
                                                 value,
                                                 use.object.names = FALSE, 
                                                 order.rows = FALSE) {
  
  
  object@C <- as.data.frame(value)
  
  if(length(object@ATTR$names) != 0 && use.object.names && !order.rows) {
    rownames(object@C) <- object@ATTR$names
  } else if(length(object@ATTR$names) != 0 && use.object.names && order.rows) {
    object <- int.order(object)
  } else if(length(object@ATTR$names) == 0) {
    object@ATTR$names <- rownames(value)
  }
  #check validity
  validObject(object)
  
  object
  
})

#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.OUT

setMethod("ecoslot.OUT", "ecogen", 
          function(X, ...) {
            
            #convert dots into characters
            u <- substitute(list(...))[-1]
            u <- sapply(u, deparse)
            u <- gsub("\"", "", u)
            
            if(length(u) == 0) {
              if(length(X@OUT) != 0) {
                out.clas <- character()
                
                for(i in seq(along = X@OUT)) { 
                  out.clas[i] <- class(X@OUT[[i]])[1]
                }
                
                out.names <- data.frame(names(X@OUT), 
                                        rep("|", length(X@OUT)), 
                                        out.clas)
                colnames(out.names) <- c("OBJECTS","", "CLASSES")
                cat("\n")
                return( out.names)
              } else {
                message("OUT is empty")
                return(invisible(NULL))
              }
            }
            
            cual <- which(names(X@OUT) %in% u)
            if(length(cual) == 0) {
              return(logical(0))
            }
            out <- X@OUT[cual]
            
            out
          })




#' @rdname EcoGenetics-accessors
#' @exportMethod ecoslot.OUT

setReplaceMethod("ecoslot.OUT", "ecogen", function(object, value) {
  
  #empty list -> new slot OUT empty
  if(is.list(value) && length(value) == 0) {
    object@OUT <- list()
    return(object)
  } # return object
  
  # obtention of names of the argument <value>
  ## split arguments if several present
  if(is.list(value)) {
    res.names <- substitute(value)
    res.names <- lapply(res.names, deparse)[-1]
    res.names <- unlist(res.names)
  } else {
    ## one argument
    res.names <- deparse(substitute(value))
  }
  
  # if vector -> conversion into list
  if(!is.list(value)) {
    value <- list(value)
  }
  
  # remotion of quotation marks and spaces
  res.names <- gsub("[\"]| ", "", res.names)
  
  #abbreviate names if required 
  # if(!is.null(abbr)) {
  # res.names <- as.vector(abbreviate(res.names, abbr))
  # }
  
  #-------------------------------#
  Z <- object
  
  # fill OUT slot
  
  # original names 
  tmp <- names(Z@OUT)
  # add elements to Z
  Z@OUT <- c(Z@OUT, value)
  # add names to Z. Names must be unique
  names(Z@OUT)<- make.unique(c(tmp, eval(res.names)))
  # order names
  orden <- order(names(Z@OUT))
  Z@OUT <- Z@OUT[orden]
  
  Z
  
})

setMethod("int.ecoslot.INT", "ecogen", function(X) X@INT)

#' @rdname EcoGenetics-accessors
#' @keywords internal

setGeneric("int.ecoslot.INT<-", function(object, value) standardGeneric("int.ecoslot.INT<-"))
setReplaceMethod("int.ecoslot.INT", "ecogen", function(object, value) { 
  slot(object, "INT") <- value
  object
})
#--------------------------------------------------------------------#
