#' Allelic frequency histograms for an ecogen genetic data frame
#' 
#' @description This program computes the frequency of each allele and plots 
#' a histogram for the number of alleles with a given frequency. 
#' The distribution is expected to be L-shaped under mutation-drift equilibrium.
#' When a factor is given, the program plots a histogram for each group.
#' @param eco Object of class "ecogen".
#' @param grp Optional factor (column of the S slot) for plots by group.
#' @examples 
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' eco.alfreq(eco)
#' eco.alfreq(eco, "pop")
#' 
#' }
#' 
#' @references 
#' 
#' Luikart G., F. Allendorf, F. Cornuet, and W. Sherwin. 1998. 
#' Distortion of allele frequency distributions provides a test for recent 
#' population bottlenecks. Journal of Heredity, 89: 238-247.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

setGeneric("eco.alfreq", function(eco, grp = NULL) {
  
  if(!is.null(grp)) {
    cual <- which(colnames(eco@S) == grp)
    grp.num <- as.numeric(levels(eco@S[, cual]))[eco@S[, cual]] 
    nfact <- max(grp.num)
  } else {
    eco@S <- data.frame(dummy = rep(1, nrow(eco@G)))
    cual <- 1
    rownames(eco@S) <- eco@ATTR$names
    grp.num <- rep(1, nrow(eco@G))
    nfact <- 1
  }
  
  out <- list()
  for(i in 1:nfact) {
    eco2 <- eco[which(eco@S[, cual] == i)]
    clases<- as.numeric(eco2@INT@loc.fac)
    tabla <- ecoslot.A(eco2)
    # correction, accordingly to the new format of the slot A. LR, 10/12/16
    #tabla <- 2 * tabla
    frecuencia <- apply(tabla, 2, sum)
    alelos.locus <- tapply(frecuencia, clases, sum)
    for( j in 1:length(clases)) {
      temp <- clases[j]
      clases[j] <- alelos.locus[temp]
    }
    frecuencia <- frecuencia / clases
    
    #tapply(frecuencia, clases, sum) #verification
    
    frecuencia <- as.data.frame(frecuencia)
    if(nfact >1) {
      tit <-paste("Pop",levels(eco@S[, cual])[i])
    } else {
      tit <-""
    }
    
    grafico<- ggplot2::ggplot(frecuencia, ggplot2::aes(frecuencia)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ..density..), fill = "black") +
      ggplot2::geom_density(alpha = 0.5, fill = "red") +
      ggplot2::labs(title = tit) +
      ggplot2::xlab("Frequency class") +
      ggplot2::ylab("Density")
    out[[i]] <- grafico
  }
  out
})
