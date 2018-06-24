
#' Formula construction for ecogen objects
#' 
#' @param eco Object of class "ecogen". 
#' @param formula Formula with names of colums from the slots XY, P, G, A, E, or C
#' @param out.mode Output results explicit formula (default) or expression. 
#' @param expand.tables method for tables coertion. Default is "+"
#' @description  When a data frame is present in a slot of an object, any individual column can be 
#' accessed using the notation: my_object@@my_data_frame[, column_to_be_accessed]. The later constitutes an explicit 
#' name for the variable present in the object.  The present function generalizes this concept, 
#' allowing to construct a formula for the variables present in an ecogen object
#' (columns of the data frames in slots). The function creates an explicit formula that can be used
#' to parse ecogen objects into other functions (see examples below). For this purpose, each name in the 
#' formula is substituted with explicit names of columns if:
#' 
#' - The name corresponds to the name of an individual column in the data frames of the ecogen object, or
#' 
#' - The name is surrounded by U() and corresponds to the name of a slot. Complete data frames (as P, E, etc.) 
#'   or subsets (as U[, 1:5]) can be passed with this method and all the columns will be explicitly included in the formula.
#'   
#'   In other situations, names are not replaced.
#'   
#' @examples
#' \dontrun{
#' require(vegan)
#' data(eco.test)
#' 
#' # Note that in this example "Condition" is not replaced; 
#' # the function Condition has a special meaning in rda,
#' # indicating conditioning variables; in eco.formula it is only text.
#' 
#' form <- eco.formula(eco, P1 + P2 + P3 + U(A) ~ E1 + E2 + Condition(X+Y))
#' rda(form)
#' 
#' form2 <- eco.formula(eco, P1 + P2 + P3 + U(A) ~ E1 + E2 + X + Y)
#' lm(form2)
#' 
#' 
#' # parsing with magrittr
#' eco.formula(eco, P1 + P2 + P3 + U(A) ~ U(E) + Condition(X+Y)) %>% rda
#
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.formula", function(eco, formula, 
                                   out.mode = c("formula", "expression"),
                                   expand.tables = "+") {
  
  out.mode <- match.arg(out.mode)
  #extract formula and formula names
  obj_name <- deparse(substitute(eco))
  my_formula <- substitute(formula)
  
  #names in formula
  nombres <- all.names(my_formula, functions = FALSE)
  
  econames <-  list(XY=colnames(eco@XY), P=colnames(eco@P), 
                 G=colnames(eco@G), E=colnames(eco@E), 
                 A=colnames(eco@A), C=colnames(eco@C))
  
  
  #-------------------------------------------------------------------------------
  # testing
  
  # test multiple matches and non valid names #
  
  #find U(...)
  findU <- as.character(my_formula)
  #mask those elements for the following tests
  # use gregexpr for multiple matches
  regmatches(findU, gregexpr("U\\(.*?\\)", findU)) <- "1"
  findU <- parse(text=findU)
  test.names <- all.names(findU, functions = FALSE)
  
  cuales.inverse <- lapply(econames, function(x) match(test.names, x, nomatch = 0))
  test <- do.call("rbind", cuales.inverse)
  matching <- apply(test, 2, function(x) sum(x != 0))
  multimatch <- which(matching > 1)
  #multiple matches not allowed
  if(length(multimatch) > 1) {
    stop(paste0("non unique names found for:", test.names[multimatch]))
  }
  nomatch <- which(matching == 0) 
  if(length(nomatch) > 0) {
    stop(paste0("names not found for:", test.names[nomatch]))
  }
  # end testing
  
  
  # start body of the function 
  #--------------------------------------------------------------------------------
  
  # find first if names match overall tables using gregexpr
 
  newnames <- list()
  findU <-  as.character(my_formula)
  locU <- gregexpr("U\\(.*?\\)", findU)
  
  #if any locU non empty
  if(!all(sapply(locU, function(x)x[[1]] == -1))) {
  
  locU <- regmatches(findU, locU)
  locU <- unlist(locU)
  locU <- gsub("^U\\(|\\)$", "", locU) # ver para caso simple, le puse el [[1]] p/ multi

  # store position of locU and old names. Using unique to solve the
  # situation of duplicated names in formula. 
  
  # remove brackests if present 
  old_locU <- gsub("\\[.*?\\]", "", locU)
  
  oldPos <- which(nombres %in% unique(old_locU)) 
  # match do not works for all cases, because it returns the first match
  #match(unique(locU), nombres)
  oldnames <- paste0(obj_name, "@", locU)

  #set as colnames and obtain the complete name
  newnames <- paste0("colnames(", obj_name, "@", locU, ")")
  newnames <- lapply(newnames, function(x) eval(parse(text=x)))
  
  
  tablenames <- c("eco@XY", "eco@P", "eco@G", "eco@E", "eco@A", "eco@C")
  tablenames <- paste0(tablenames, "[, ")
  cuales <- list()
  for(i in seq_along(newnames)) {
  cuales[[i]] <- lapply(econames, function(x) match(x, newnames[[i]], nomatch = 0))
  names(cuales[[i]]) <- tablenames
  }
  
  cuales <- lapply(cuales, unlist)
  cuales <- lapply(cuales, function(x)x[x != 0])
  cuales <- lapply(cuales, function(x){ names(x) <- paste(names(x), "]", sep =""); x})
  cuales <- lapply(cuales, function(x) x[order(x)])
  newnames<- lapply(cuales, names)
  newnames <- lapply(newnames, function(x) paste(x, collapse = paste0(" ", expand.tables, " ")))
  newnames <- lapply(newnames, function(x) parse(text=x)[[1]])
  
  #names(cuales) <- paste0(oldnames, "[, '")
  #names(newnames) <- paste0(oldnames, "[, '")
  #newnames <- lapply(seq_along(newnames), function(x) paste0(names(newnames)[x], newnames[[x]], "']"))
  #newnames <- lapply(newnames, function(x) paste0(x, collapse = paste0(" ", expand.tables, " ")))
  #newnames <- lapply(newnames, function(x) parse(text = x)[[1]])
  
  #remove U(...)
  my_formula <- gsub("(U\\()(.*?)(\\))", "(\\2)", deparse(my_formula))
  #remove [...]
  my_formula <- gsub("\\[.*?\\]", "", my_formula)
  
  my_formula <- parse(text=my_formula)
  # the next step is needed because expression object has a problem to
  # work in the last call. The step set my_formula with the call slot
  my_formula<- my_formula[[1]]
  
  }
  
  if(length(newnames) != 0) {
    #equivalent to the names obtained above during masking
    nombres_2 <- nombres[-oldPos]
  } else {
    nombres_2 <- nombres
  } # end find overall
  
    
  #-------------------------------------------------------------------------------
  # find now formula names in ecogen object---------------------------------------
  tablenames <- c("eco@XY", "eco@P", "eco@G", "eco@E", "eco@A", "eco@C")
  cuales <- list()
  for(i in seq_along(nombres_2)) {
   cuales[[i]] <- lapply(econames, function(x) match(nombres_2[i], x, nomatch = 0))
  }
  
  #cuales <- lapply(econames, function(x) match(x, nombres_2, nomatch = 0))
  names(cuales) <- rep("", length(cuales))
  
  cuales <- lapply(cuales, function(x) {names(x) <- paste0(tablenames, "[, "); x})
  cuales <- lapply(cuales, unlist)
  
  cuales <- lapply(cuales, function(x) x[x != 0])
  cuales <- lapply(cuales, function(x) {names(x) <-  paste(names(x), x,  "]", sep =""); x})
  #names(cuales) <- paste(names(cuales), "]", sep ="")
  #cuales <- cuales[order(cuales)]

  lista<- lapply(cuales, function(x) parse(text=names(x))[[1]])
  
  # end of search of formula names 
  
  # end body of the function
  
  #-------------------------------------------------------------------------------
  # create the output
  
  #create a list for substitution 
  lista_out <-  list()
  if(length(newnames) != 0) {
    #fill the list with the elements in formula order
    lista_out[oldPos] <- newnames
    posiciones <- which(nombres %in% nombres_2)
    lista_out[posiciones] <- lista
  } else {
    lista_out <- lista
  }
      
  #give names to the list for substitution
  names(lista_out)<- nombres
  
  # substitute in formula the data frame information. The solution to pass the name 
  # for the formula is tricky and requires two substitutions
  
  out <- eval(substitute(substitute(my_formula, as.environment(lista_out)), list(my_formula = my_formula)))
  if(out.mode == "expression") {
    return(out)
  } else {
    return(as.formula(out))
  }
  
})
