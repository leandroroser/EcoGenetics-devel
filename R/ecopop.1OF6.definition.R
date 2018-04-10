
################################################
#### INT.POPDATA CLASS DEFINITION
################################################

#' int.popdata class
#' @name int.popdata-class
#' @keywords internal
#' @slot  ploidy ploidy
#' @slot type type of data ("codominant" or "dominant")
#' @slot NA.char NA character
#' @slot ncod number of digits coding each allele (codominant data)
#' @slot aggregator function used to aggregate data
#' @slot factor_to_counts Logical. Factors splitted into counts for each level?
#' @slot loc.fac locus of each allele
#' @slot all.names alleles names
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @aliases int.popdata-class


setClass("int.popdata", 
         representation(ploidy = "intORnull",
                        type = "character",
                        aggregator = "function",
                        factor_to_counts = "logical",
                        loc.fac = "factorORnull",
                        all.names = "characterORnull",
                        allele_data = "characterORnull"
         ), 
                       
         prototype(ploidy = NULL,
                   type = "codominant",
                   aggregator = function(){},
                   factor_to_counts = TRUE,
                   loc.fac = NULL,
                   all.names = NULL,
                   allele_data = NULL
         )
)

## validator-----------------------------------------------------------------#
#' check_ecopop
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

check_ecopop <- function(object) {
  
  errors <- character()
  
  if(object@ATTR$ver != '1.2.1-5' || is.null(object@ATTR$ver)) {
    msg <- "This object was created with an old version of EcoGenetics.
            Please actualize it using the function eco.old2new"
    errors <- c(errors, msg)
  }
  
  if(object@ATTR$lock.rows) {
  # check number of rows  = 0 or unique -----
  
  dim_eco <- list(dim(object@XY), dim(object@P), 
                  dim(object@AF), dim(object@E), 
                  dim(object@C))
  
  nrow_eco <- unique(sapply(dim_eco, "[[",1))
  nrow_eco <- nrow_eco[nrow_eco != 0]
  
  if(length(nrow_eco) > 1) {
    msg <- "number of rows differ for non empty data frames"
    errors <- c(errors, msg)
  }
  
  names_object <- list(rownames(object@XY), rownames(object@P), 
                       rownames(object@AF), rownames(object@E),
                       rownames(object@S), rownames(object@C))
  
  names_object <- names_object[vapply(names_object, function(i) length(i) != 0, 
                                      logical(1))]
  # # check valid length of names 
  # n_length <- length(object@S)
  # 
  # check_n_length <- vapply(names_object, function(i) n_length == length(i),
  #                          logical(1))
  
  # if(!all(check_n_length)) {
  #   msg <- "invalid length in object names"
  #   errors <- c(errors, msg)
  # }
  
    check_names <- vapply(names_object, function(i) all(i == object@ATTR$names), 
                          logical(1))
    
    if(!all(check_names)) {
      msg <- "data frames with different row names"
      errors <- c(errors, msg)
    }
    
  }
    
  if(is.null(object@INT@ploidy)) {
    msg <- "null ploidy in object"
    errors <- c(errors, msg)
  }
  
  if(length(errors) == 0) TRUE else errors
}


################################################
#### ECOPOP CLASS DEFINITION
################################################

#' ecopop class
#' @name ecopop-class
#' @keywords internal
#' @slot XY P data frame
#' @slot P P data frame
#' @slot AF AF data frame
#' @slot E E data frame
#' @slot S S data frame
#' @slot C C data frame
#' @slot INT int.popdata slot
#' @slot ATTR attributes slot
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @aliases ecopop-class


setClass("ecopop",
         
         representation(XY = "data.frame",
                        P = "data.frame",
                        AF = "matrix",
                        E = "data.frame",
                        S = "data.frame",
                        C = "data.frame",
                        INT = "int.popdata",
                        ATTR = "list"
         ),
         
         prototype(XY = data.frame(), 
                   P = data.frame(),
                   AF = matrix(nrow = 0, ncol = 0),
                   E = data.frame(),
                   S = data.frame(), 
                   C = data.frame(),
                   ATTR = list(names = character(0),
                               lock.rows = TRUE,
                               whereIs = new.env(emptyenv()), 
                               .call = call("."),
                               ver = utils::packageDescription("EcoGenetics", fields = "Version"))
         ),
         validity = check_ecopop
)
