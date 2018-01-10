#' Merging two ecogen objects. Ordering the rows of an ecogen 
#' object according to the rows of another
#' 
#' @details This program generates an ecogen object by binding the columns 
#' of the individuals that have matching row names in the objects e1 and e2. 
#' If the objects have different number of rows, the result is a merged data frame with 
#' the rows in the order of the first object.
#' If the objects have the same number of rows, but in a different order,
#' the product is an object with the rows ordered as the first object.
#' The algorithm matches sequentially the data frame pairs of each slot
#' that the user wishes to merge. 
#' 
#' @param e1 Object of class "ecogen".
#' @param e2 Object of class "ecogen".
#' @param ... Data frames to merge. Could be any combination of
#' the following: "XY",P","G","E" and "C", or "ALL". If a "G" data frame
#' is provided, the program generates also the INT slot by coding the missing
#' data as "0". 
#' 
#' @examples
#' \dontrun{
#' data(eco.test)
#' eco
#' eco1 <- eco[2:20]
#' merged <- eco.merge(eco, eco1)
#' merged
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.merge",
           function(e1, e2, ...) {
             
             u <- unlist(list(...))
             vec <- c("P", "G", "E", "S", "C", "ALL")
             m <- vec %in% u
             
             # if data frame(s) not specified, or ALL specified, default is ALL
             if(!any(m) | m[6] == TRUE) {
               m <- rep(TRUE, 6)
             }
             
             z <- new("ecogen")
             
             # internal merge function
             int.merge <- function(X1, X2, Z, cond) {
               #if X1 or X2 empty, return an empty data frame
               if(any(dim(X1)) == 0 || any(dim(X2)) == 0) {
                 return(data.frame())
               } 
               # non empty data frame
               if(cond) {
                 Z <-  merge(data.frame(rownames(X1), c(1:nrow(X1)), X1),
                             data.frame(rownames(X2), X2), by = 1)
                 Z <- Z[order(Z[, 2]), ]
                 rownames(Z) <- Z[, 1]
                 Z <- Z[, -c(1, 2)]
               }
               return(Z)
             } # end int.merge
             
             
             # fill XY,  P  and G data frames
             howmuchXY <- max(ncol(e1@XY), ncol(e2@XY))
             z@XY <- int.merge(e1@XY, e2@XY, z@XY, TRUE)[, seq_len(howmuchXY)]
             z@P <- int.merge(e1@P, e2@P, z@P, m[1] == TRUE)
             z@G <- int.merge(e1@G, e2@G, z@G, m[2] == TRUE)
             
             # if G is not empty, fill A and INT slots
             if(all(dim(z@G)) != 0) {
               if(e1@INT@ploidy != e2@INT@ploidy) {
                 stop("error: different ploidy levels found")
               }
               
               if(e1@INT@type != e2@INT@type) {
                 stop("error: different type of markers found")
               }
               
               tempo <- int.df2genind(z@G, 
                                      missing = e1@INT@missing,
                                      ncod = e1@INT@ncod,
                                      ploidy = e1@INT@ploidy, 
                                      type =  e1@INT@type)
              
               # slot A is marker type dependent
               if(e1@INT@type == "codominant") {
                 z@A <- tempo@tab
               } else {
                 z@A <- matrix(nrow = 0, ncol = 0)
               }
               
               z@INT@loc.fac <- tempo@loc.fac
               z@INT@all.names <- tempo@all.names
               z@INT@ploidy <- tempo@ploidy
               z@INT@type <- tempo@type
               z@INT@NA.char <- tempo@NA.char
               z@INT@missing <- tempo@missing
               z@INT@removed.image <- tempo@removed.image
               
             } else {
               z@A <- data.frame()
               z@INT <- new("int.gendata")
             }
             
             # fill E, S and C data frames
             z@E <- int.merge(e1@E, e2@E, z@E, m[3] == TRUE)
             z@S <- int.merge(e1@S, e2@S, z@S, m[4] == TRUE)
             z@C <- int.merge(e1@C, e2@C, z@C, m[5] == TRUE)
             
             # set row names
             maxrow <- which(nrow(z) == max(nrow(z)))
             if(any(maxrow) != 0){
             maxrow <- maxrow[1]
             z@ATTR$names <- rownames(z[[maxrow]])
             }
             # check validity
             validObject(z)
             
             z
             
           } )
