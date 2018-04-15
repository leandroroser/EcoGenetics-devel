
#' Estimate Nei distance matrix 
#' @description Estimate Nei distance matrix. NAs are avoided. 
#' @param obj ecopop orgenpop  objects, or matrix/data frame with allele frequencies
#' @param as_dist Return a dist object or a matrix? default an object of class "dist".
#' @examples
#' \dontrun{
#' data(eco.test)
#' eco.nei_dist(my_ecopop)
#' }
#' @author Juan Vilardi
#' @export

setGeneric("eco.nei_dist", function(obj, as_dist = TRUE) {
  
  # get matrix matr with 'allele counts'
  
  if (class(obj) == "genpop") {
    matr <- obj@tab
  } else if (class(obj) == "ecopop") {
    if (obj@INT@allele_data == "frequency") {
      stop("ecopop object with genetic data as counts needed, 
           but this object has allele frequencies")
    }
    x <- obj@AF
  } else if (class(obj) == "matrix" || class(obj) == "data.frame") {
    x <- obj
  } else {
    stop("object of invalid class")
  }
  
  nrow_x <- nrow(x)
  neidistan <- matrix(rep(0, nrow_x^2), ncol = nrow_x, nrow = nrow_x) 
  
  for (i in seq_len(nrow_x - 1)) {
    for (j in seq(i + 1, nrow_x)) {
      present <- which(!is.na(x[i, ]) & !is.na(x[j, ]))
      ja <- sum(x[i, present]^2)
      jb <- sum(x[j, present]^2)
      jab <- sum(x[i, present] * x[j, present])
      neidistan[i, j] <- neidistan[j, i] <- -log(jab/sqrt(ja * jb))
    }
  }
  rownames(neidistan) <- colnames(neidistan) <- rownames(x)
  if(as_dist) {
  neidistan <- as.dist(neidistan)
  }
  neidistan
})
