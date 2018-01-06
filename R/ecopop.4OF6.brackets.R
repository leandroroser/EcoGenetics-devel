
################################################
#### $, $<-, "[", "[<-", "[[, AND "[[<-" METHODS
################################################


## "$"-------------------------------------------------------------------------#
#' $
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods
#' @aliases $,ecopop,character-method 
#' 

# DEPRECTED

setMethod("$","ecopop",
          function(x, name) {	
mess <- message(
paste("Method not available for ecopop objects\n",
"     Use instead the accessor", aue.access(name, deparse(substitute(x))), "or double square brackets,\n",
"    ", paste(deparse(substitute(x)), "[[", deparse(substitute(name)),"]].", sep = ""), 
"See help('EcoGenetics accessors')."))
return(mess)
          })


## "$<-"-----------------------------------------------------------------------#
#' $<-
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases $<-,ecopop,character,ANY-method 

# DEPRECTED

setMethod("$<-","ecopop",
          function(x,name,value) {
            tmp <- "name_of_this_object"
            mess <- message(
              paste("Method not available for ecopop objects\n",
                    "     Use instead the accessor, ", 
                    paste(aue.access(name, tmp), "<-,\n", sep = ""),
                    "     or double square brackets,",
                    "", paste(tmp, "[[", deparse(substitute(name)),"]]<-.", sep = ""), 
                    "\n      See help('EcoGenetics accessors')."))
            print(mess)
            return(x)
          })


## "["-------------------------------------------------------------------------#
#' [ 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases [,ecopop,numeric,missing,ANY-method 

setMethod("[", c("ecopop", "numericORmissing", "missing", "ANY"), 
          
          function(x, i, j, ..., drop = FALSE) {
            
            # empty i, return x
            if(missing(i)) {
              return(x)
            }
            
            # length(i) == 0 or i == 0, return empty object
            if(length(i) == 0 || i == 0) {
             return(new("ecopop"))
            }
            
      
            z <- new("ecopop")
            
            # if(all...) condition required because subsetting over matrices of 
            # dimension 0 generates a matrix of dimension i x 0 (undesired)
            if(all(dim(x@XY) != 0)) {
            z@XY <- x@XY[i, , drop =FALSE]
            }
            if(all(dim(x@P) != 0)) {
            z@P <- x@P[i, , drop =FALSE]
            }
            
            if(all(dim(x@AF) != 0)) {
            z@AF <- x@AF[i, , drop =FALSE]
            }
            
            if(all(dim(x@E) != 0)) {
            z@E <- x@E[i, , drop =FALSE]
            }
            
            # all S columns as factors
            if(length(x@S) != 0) z@S <- x@S[i]

            if(all(dim(x@C) != 0)) {
            z@C  <- x@C[i, , drop =FALSE]
            }
            z@ATTR$names <- x@ATTR$names[i]
            
            # check validity
            validObject(z)
            
           z
          })


#' [ 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases [,ecopop,logical,missing,ANY-method 

setMethod("[", c("ecopop", "logicalORmissing", "missing", "ANY"), 
          
          function(x, i, j, ..., drop = FALSE) {
            
            # empty i, return x
            if(missing(i)) {
              return(x)
            }
            
            # length(i) == 0 or all i == FALSE, return empty object
            if(length(i) == 0 || all(i == FALSE)) {
              return(new("ecopop"))
            }
            
            # check row number with the nrow ecopop method
            nrow_x <- nrow(x)
            nrow_x <- unique(nrow_x)
            
            # if empty, return an empty object
            if(length(nrow_x == 1) && nrow(x) == 0) {
              return(x)
            # else, check if length i is adequate
            } else {
              len_i <- length(i)
              nrow_x <- max(nrow_x)
              if(nrow_x != len_i) {
                stop(paste0("invalid logical vector of length = ", len_i,", but 
                            non empty slots with nrow = ", nrow_x))  
              }
            }
              
            # create an int.genind object if nrow(G) != 0
          
            z <- new("ecopop")
            
            # if(all...) condition required because subsetting over matrices of 
            # dimension 0 generates a matrix of dimension i x 0 (undesired)
            if(all(dim(x@XY) != 0)) {
              z@XY <- x@XY[i, , drop =FALSE]
            }
            if(all(dim(x@P) != 0)) {
              z@P <- x@P[i, , drop =FALSE]
            }
            
            if(all(dim(x@AF) != 0)) {
              z@AF <- x@AF[i, , drop =FALSE]
            }
            
            if(all(dim(x@E) != 0)) {
              z@E <- x@E[i, , drop =FALSE]
            }
            
            # all S columns as factors
            if(length(x@S) != 0) z@S <- x@S[i]
 
            if(all(dim(x@C) != 0)) {
              z@C  <- x@C[i, , drop =FALSE]
            }
          
            z@ATTR$names <- x@ATTR$names[i]
            
            # check validity
            validObject(z)
          
            z
          })



## [[--------------------------------------------------------------------------#
#' [[
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases [[,ecopop,character,missing-method


setMethod("[[", c("ecopop","character", "missing"), function(x, i, j) {
  if(toupper(i) ==  "XY") return(ecoslot.XY(x))
  if(toupper(i) == "P") return(ecoslot.P(x))
  if(toupper(i) == "AF") return(ecoslot.AF(x))
  if(toupper(i) == "E") return(ecoslot.E(x))
  if(toupper(i) == "S") return(ecoslot.S(x))
  if(toupper(i) == "C") return(ecoslot.C(x))
  if(!toupper(i) %in% c("XY", "P", "AF", "E", "S", "C")) {
    message(paste(paste("<", i, ">", sep = ""), "is an undefined ecopop slot"))
  }
})


## [[<- internal---------------------------------------------------------------#
#' [[<-
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

#' [[<- -----------------------------------------------------------------------#
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @rdname ecopop-methods 
#' @aliases [,ecopop,character,missing-method

setReplaceMethod("[[",c("ecopop","character", "missing"),  function (x, i, j,..., value) {
  
  if(toupper(i) == "XY") ecoslot.XY(x) <- value
  if(toupper(i) == "P") ecoslot.P(x) <- value
  if(toupper(i) == "AF") ecoslot.AF(x, ...) <- value
  if(toupper(i) == "E") ecoslot.E(x) <- value
  if(toupper(i) == "S") ecoslot.S(x) <- value
  if(toupper(i) == "C") ecoslot.C(x) <- value

  if(!toupper(i) %in% c("XY", "P", "AF", "E", "S", "C")) {
    message(paste(paste("<",i,">", sep = ""), "is an undefined ecopop slot"))
  }
  return(x)
})
