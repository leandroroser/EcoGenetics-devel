
## validator-----------------------------------------------------------------#
#' check_ecogen
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

check_ecogen <- function(object) {
  
  errors <- character()
  
  locked <- is.locked(object)
  
  if(locked) {
  
  # check number of rows  = 0 or unique -----
  
  dim_eco <- list(dim(object@XY), dim(object@P), dim(object@G), 
                  dim(object@A), dim(object@E), dim(object@S), 
                  dim(object@C))
  
  nrow_eco <- unique(sapply(dim_eco, "[[",1))
  nrow_eco <- nrow_eco[nrow_eco != 0]
  
  if(length(nrow_eco) > 1) {
    msg <- "number of rows differ for non empty data frames"
    errors <- c(errors, msg)
  }
  
  names_object <- list(rownames(object@XY), rownames(object@P), 
                       rownames(object@G), rownames(object@A), 
                       rownames(object@E), rownames(object@S), 
                       rownames(object@C))
  
  names_object <- names_object[vapply(names_object, function(i) length(i) != 0, logical(1))]
  # check valid length of names 
  n_length <- length(object@ATTR$names)
  
  check_n_length <- vapply(names_object, function(i) n_length == length(i), logical(1))
  
  if(!all(check_n_length)) {
    msg <- "invalid length in object names"
    errors <- c(errors, msg)
  }
  
  # check equality in names
  if(all(check_n_length)) {
    check_names <- vapply(names_object, function(i) all(i == object@ATTR$names), logical(1))
    
    if(!all(check_names)) {
      msg <- "data frames with invalid row names"
      errors <- c(errors, msg)
    }
  }
  
  }
  
  if(length(errors) == 0) TRUE else errors
}

################################################
#### ECOGEN CLASS DEFINITION
################################################

#' ecogen class
#' @name ecogen-class
#' @keywords internal
#' @slot XY P data frame
#' @slot P P data frame
#' @slot G G data frame
#' @slot A A matrix
#' @slot E E data frame
#' @slot S S data frame
#' @slot C C data frame
#' @slot OUT results
#' @slot INT int.gendata slot
#' @slot ATTR attributes slot
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @aliases ecogen-class


setClass("ecogen",
         
         representation(XY = "data.frame",
                        P = "data.frame",
                        G = "data.frame",
                        A = "matrix",
                        E = "data.frame",
                        S = "data.frame",
                        C = "data.frame",
                        OUT = "list",
                        INT = "int.gendata",
                        ATTR = "list"),
         
         prototype(XY = data.frame(), 
                   P = data.frame(),
                   G = data.frame(),
                   A = matrix(nrow = 0, ncol = 0),
                   E = data.frame(),
                   S = data.frame(), 
                   C = data.frame(),
                   OUT = list(),
                   INT = new("int.gendata"),
                   ATTR = list(names = character(0),
                               lock.rows = TRUE,
                               whereIs = new.env(emptyenv()), 
                               .call = call("."),
                               ver = utils::packageDescription("EcoGenetics", fields = "Version"))
                   ),
         validity = check_ecogen
)

