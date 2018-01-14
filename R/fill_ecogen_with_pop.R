
#' Importation of data frames to ecogen
#' 
#' @description This function imports into an ecogen object the  population data
#' contained in a series of data frames.
#' These data frames can be used to fill the slots XY, P, E and C.  
#' @param from ecogen object
#' @param pop Name of the column of the slot S with populations.
#' @param pop_levels Vector with the name of the populations for each
#' row of the input data frames. These populations must correspond to
#' the levels of the column of the slot S used for the argument pop. 
#' @param XY Population data for slot XY. Defaul NULL.
#' @param P Population data for slot P. Defaul NULL.
#' @param E Population data for slot E. Defaul NULL.
#' @param C Population data for slot C. Defaul NULL.
#' @param bind_columns Bind columns of the generated tables 
#' with the preexisting in the ecogen slots?
#' Default FALSE (overwrite the slot).
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' data(eco.test)
#' 
#' # create some population data 
#' to_ecopop <- ecogen2ecopop(eco, "pop")
#' XY_pop <- to_ecopop[["XY"]]
#' P_pop <- to_ecopop[["P"]]
#' E_pop <- to_ecopop[["E"]]
#' 
#' # Add only XY data to the ecogen object
#' object_with_pop_data <- eco.fill_ecogen_with_df(eco, "pop", c(1,2,3,4), 
#'                                          XY = XY_pop)
#'                                          
#' # Add all the population data to the ecogen object
#' object_with_pop_data <- eco.fill_ecogen_with_df(eco, "pop", c(1,2,3,4), 
#'                                          XY = XY_pop, P = P_pop, E = E_pop)
#' 
#' }
#' 
#' @seealso eco.fill_ecogen_with_ecopop
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

eco.fill_ecogen_with_df <- function(from, pop, pop_levels,  XY = NULL, P = NULL, 
                                           E = NULL, C = NULL, bind_columns = FALSE) {
  
  if(missing(pop)) {
    pop <- rep(1, length(names(from)))
  } else {
    pop <- match(pop, colnames(from@S), nomatch = 0)
    pop <- pop[pop != 0]
    if(length(pop) == 0) {
      stop("incorrect factor name")
    }
    pop <- from@S[, pop, drop = TRUE]
  }
  
  ind_len <- length(pop)
  pop_unique <- unique(pop)
  pop_len <- length(pop_unique)
  from_ncol <- ncol(from)
  
  if(!all(pop_unique %in% pop_levels)) {
    stop("non matching levels between selected population factor in slot S and the pop_levels vector")
  }
  
  if(!is.null(XY)) {
    if(nrow(XY) != pop_len) {
      stop("XY has a different number of rows than expected for the ecogen object")
    }
    XY_out <- as.data.frame.matrix(matrix(nrow = ind_len, ncol = ncol(XY)))
    colnames(XY_out) <- colnames(XY)
    rownames(XY_out) <- names(from)
  }
  
  if(!is.null(P)) {
    if(nrow(P) != pop_len) {
      stop("P has a different number of rows than expected for the ecogen object")
    }
    P_out <- as.data.frame.matrix(matrix(nrow = ind_len, ncol = ncol(P)))
    colnames(P_out) <- colnames(P)
    rownames(P_out) <- names(from)
  }
  
  if(!is.null(E)) {
    if(nrow(E) != pop_len) {
      stop("E has a different number of rows than expected for the ecogen object")
    }
    E_out <- as.data.frame.matrix(matrix(nrow = ind_len, ncol = ncol(E)))
    colnames(E_out) <- colnames(E)
    rownames(E_out) <- names(from)
  }
  
  if(!is.null(C)) {
    if(nrow(C) != pop_len) {
      stop("C has a different number of rows than expected for the ecogen object")
    }
    C_out <- as.data.frame.matrix(matrix(nrow = ind_len, ncol = ncol(C)))
    colnames(C_out) <- colnames(C)
    rownames(C_out) <- names(from)
  }
  
  for(i in seq_len(pop_len)) {
    if(!is.null(XY)) {
      XY_out[pop == pop_levels[i], ]<- XY[i, ]
      ecoslot.XY(from) <- XY_out
    }
    
    if(!is.null(P)) {
      P_out[pop == pop_levels[i], ]<- P[i, ]
      if(bind_columns && from_ncol["P"] > 0) {
        ecoslot.P(from) <- cbind(ecoslot.P(from) , P_out)
      } else {
        ecoslot.P(from) <- P_out
      }
    }
    
    if(!is.null(E)) {
      E_out[pop == pop_levels[i], ]<- E[i, ]
      if(bind_columns && from_ncol["E"] > 0) {
        ecoslot.E(from) <- cbind(ecoslot.E(from) , E_out)
      } else {
        ecoslot.E(from) <- E_out
      }
    }
    
    if(!is.null(C)) {
      C_out[pop == pop_levels[i], ]<- C[i, ]
      if(bind_columns && from_ncol["C"] > 0) {
        ecoslot.C(from) <- cbind(ecoslot.C(from) , C_out)
      } else {
        ecoslot.C(from) <- C_out
      }
    }
  }
  
  from
}



#' Importation of ecopop to ecogen
#' 
#' @description This function imports into an ecogen object the population data contained in  ecopop object.
#' The function assign the values of the data to each individual, according to the population of the 
#' individual. 
#' @param from ecopop object.
#' @param to ecogen object.
#' @param pop Column in slot S of ecogen object, with the population of each individual.
#' @param what Data frames to add into the the ecogen object. Can be one of c("all", "XY", "P", "E", "C")
#' @param bind_columns Bind columns of the generated tables 
#' with the preexisting in the ecogen slots?
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' # Example 1: add population data to ecogen object
#' result <- eco.fill_ecogen_with_ecopop(my_ecopop, eco, "pop")
#' 
#' # Example 2: Create ecogen object only with population data
#' out <- ecogen(S = eco[["S"]])
#' out <- eco.fill_ecogen_with_ecopop(my_ecopop, out, "pop")
#'
#' # add the allele frequency data into the slot C with the function eco.add_popdata_into_ecogen
#' out <- eco.fill_ecogen_with_df(eco, "pop", c(1,2,3,4),  C = my_ecopop[["C"]])
#' 
#' 
#' 
#' }
#' 
#'@seealso eco.fill_ecogen_with_df
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export
#' 
#' 

eco.fill_ecogen_with_ecopop <- function(from, to, pop, what = c("all", "XY", "P", "E", "C"), bind_columns = FALSE) {
  
  what <- match.arg(what)
  
  if(missing(pop)) {
    pop_levels <- 1
  } else {
    pop_levels <- match(pop, colnames(to@S), nomatch = 0)
    pop_levels <- pop_levels[pop_levels != 0]
    if(length(pop_levels) == 0) {
      stop("incorrect factor name")
    }
    pop_levels <- unique(to@S[, pop_levels, drop = TRUE])
  }
  
  lambda <- function(ecogen_name, data_slot) {
    if((what == "all" || what == ecogen_name) && (nrow(data_slot) != 0)) return(data_slot) else return(NULL)
  }
  eco.fill_ecogen_with_df(from = to, pop = pop, pop_levels = pop_levels, 
                   XY = lambda("XY", from@XY),
                   P =  lambda("P", from@P),
                   E = lambda("E", from@E),
                   C = lambda("C", from@C),
                   bind_columns = bind_columns)
}
