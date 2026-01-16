#' Subsetting an ecogen object by group
#' 
#' @param eco Object of class "ecogen". 
#' @param hier The name of the S slot column containing labels assigning individuals to groups.
#' @param grp Label shared by the subset of individuals, contained in hier. 
#' @param missing Missing data argument It can take three values ("0", "NA" or "MEAN"),
#' as described in  \code{\link{ecogen}}.
#' Missing elements are treated as zeros in the default option.
#' 
#' @examples
#' \dontrun{
#' data(eco3)
#' eco3
#' eco.sub <-eco.subset(eco3,"structure", 1) 
#' eco.sub
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export

setGeneric("eco.subset",
           
           function(eco, 
                    hier, 
                    grp, 
                    missing = c("0", "NA",  "MEAN"))  {
             
             grupo <- eco@S
             x <- match(hier, colnames(eco@S), nomatch = 0)
             x <- x[!x == 0]
             
             # give flexibility to missing argument
             if(length(missing) == 1 && is.na(missing)) {
               missing <- "NA"
             }
             if(length(missing) == 1 && missing == 0) {
               missing <- "0"
             }
             missing <- match.arg(missing, c("0", "NA", "MEAN"))
             
             if(length(x) == 0) {
               stop("incorrect name of column in slot S")
             }
             
             if(any(is.na(match(grp, grupo[, x])))){
               stop(sprintf("<%s> does not match any level of <%s> (%s)", grp, hier, paste(levels(grupo[, x]), collapse = ", ")))
             }
             if(length(grp) > max(as.numeric(grupo[, x]))) {
               stop(sprintf("the number of groups (%d) exceeds the number of
                            groups in the data (%d)", grp,
                            max(as.numeric(grupo[, x]))))
             }
             
             grupo <- which(grupo[, x] %in% grp)
             
             z <- ecogen()
             if(all(dim(eco@XY) != 0)) {
             ecoslot.XY(z) <- eco@XY[grupo, , drop = FALSE]
             }
             if(all(dim(eco@P) != 0)) {
             ecoslot.P(z) <- eco@P[grupo, ,  drop = FALSE]
             }
             
             if(all(dim(eco@G) != 0)) {
             ecoslot.G(z, missing = missing, ncod = eco@INT@ncod,
                       ploidy = eco@INT@ploidy, type = eco@INT@type) <- eco@G[grupo, ,  drop = FALSE]
             }
             
             if(all(dim(eco@E) != 0)) {
             ecoslot.E(z) <- eco@E[grupo, , drop = FALSE]
             }
             
             if(all(dim(eco@S) != 0)) {
             ecoslot.S(z) <- eco@S[grupo, , drop = FALSE]
             }
             
             if(all(dim(eco@C) != 0)) {
             ecoslot.C(z) <- eco@C[grupo, , drop = FALSE]
             }
             
             z@OUT <- list()
             z@ATTR$names <- eco@ATTR$names[grupo]
             # check validity
             validObject(z)
             
             z
             
           })
