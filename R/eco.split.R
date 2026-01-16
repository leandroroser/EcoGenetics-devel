#' Splitting an ecogen object by group
#' 
#' @param eco Object of class "ecogen". 
#' @param hier The name of the S slot column with labels assigning individuals to groups.
#' @param name Name used for the output objects. Default is the name of the input,followed by
#' a suffix (see Description). 
#' @param missing Missing data argument This can take three values ("0", "NA" or "MEAN"),
#' as described in  \code{\link{ecogen}}. 
#' #' Missing elements are treated as zeros in the default option.
#' @param overwrite Overwrite files with the same name of the output
#'  if already present in workspace when asList = FALSE? Default FALSE.
#' @param asList Return a list with the objects instead of creating objects in workspace?
#' Default = TRUE
#' @description  The function splits an ecogen object into the groups defined
#' in the slot S. If asList is TRUE, a list with the objects is created , 
#' that can be assigned to a name with regular rules, using the operator "<-". 
#' Otherwise, the function creates in the workspace an ecogen object for each group 
#' with the following nomenclature: <name of ecogen object>.<name of the group>. 
#' @examples
#' \dontrun{
#' data(eco3)
#' eco3
#' 
#' 
#' # list of objects
#' x <- eco.split(eco3, "structure", asList = TRUE)
#' 
#' 
#' # rebinding
#' eco.bind <- eco.rbind(x)
#' 
#' 
#' # note that different subsets can also be created
#' S1.3 <- eco.rbind(x[[1]], x[[3]])
#' 
#' 
#' # split and create objects with prefix "eco3"
#' eco.split(eco3,"structure", asList = FALSE)
#' 
#' 
#' # split and create objects with prefix "newObjects"
#' eco.split(eco3,"structure", "newObjects", asList = FALSE) 
#' 
#' 
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export

setGeneric("eco.split",
           
           function(eco, 
                    hier, 
                    name = NULL,
                    overwrite = FALSE,
                    missing = c("0", "NA",  "MEAN"),
                    asList = TRUE)  {
             
             # give flexibility to missing argument

             if(length(missing) == 1 && is.na(missing)) {
               missing <- "NA"
             }
             if(length(missing) == 1 && missing == 0) {
               missing <- "0"
             }
             missing <- match.arg(missing, c("0", "NA", "MEAN"))
  
  if(!any(hier %in% colnames(eco@S))) {
    stop("hier do not correspond to any column name in slot S of eco")
  }
  
  if(is.null(name)){
  name <- deparse(substitute(eco))
  } 
             
  myFactor <- eval(call("[[", eco@S, hier))
  
  if(length(levels(myFactor)) == 1) {
    message("Only one factor level in hierarchy. Nothing to split")
    return()
  }
  
  XY <- split(eco@XY, myFactor)
  P <-  split(eco@P, myFactor)
  G <-  split(eco@G, myFactor)
  E <-  split(eco@E, myFactor)
  S <-  split(eco@S, myFactor)
  C <-  split(eco@C, myFactor)
  S <-  split(eco@S, myFactor)
  
  howmuch <- levels(myFactor)
  
  out <- list()
  for(i in seq_along(howmuch)) {
    out[[i]] <- ecogen(XY=XY[[i]], P = P[[i]], G = G[[i]], E = E[[i]], S = S[[i]], C = C[[i]])
  }
  
  # if list, return list of ecogen objects
  if(asList) {
    names(out) <- howmuch
    class(out) <- "ecolist"
    return(out)
  }
  
  #if asList = FALSE, create objects in workspace
  cat("\n")
  for(i in seq_along(howmuch)) {
    objName <- paste0(bquote(.(name)),".", bquote(.(howmuch[i])))
   
    # stop if object exists in workspace and overwrite  is FALSE
     if(overwrite == FALSE && exists(objName, where = parent.frame())) {
      stop(paste0("Object", objName, " exists in workspace, and overwrite option is FALSE.
                  For overwriting, set it as TRUE"))
    }
      
    # create objects in parent frame for each subset
    #this.envir <- environment()
    #eval(parse(text=paste0(objName, " <<- out[[i]]")), envir = this.envir)
    assign(objName, out[[i]], parent.frame())
    cat("New object created in workspace:", paste0(bquote(.(name)),".", bquote(.(howmuch[i]))), "\n")
  }
  
})
