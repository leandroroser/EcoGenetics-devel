
#' Compute allele frequencies using different methods
#' @param x ecopop or genpop object
#' @param method Method used to compute the allelic frequencies. 
#' Can be one of 'zhivor' ( Zhivotovsky (1999) with uniform prior),  
#' 'zhivonu' (Zhivotovsky (1999) with non uniform prior), 
#' 'rawfreq' (square root method),  'neifreq' (nei distance matrix)
#' @author Juan Vilardi

eco.pop_afreq <- function(x, method = c("zhivor", "zhivonu", "rawfreq", "neifreq")) {
    
    method <- match.arg(method)
    
    
    zhivor <- function(x) {
        
        if (class(x) == "genpop") {
            matr <- x@tab
        } else if (class(x) == "ecopop") {
            if(x@INT@allele_data == "frequency") {
              stop("ecopop object with genetic data as counts needed, but this object has allele frequencies")
            }
            matr <- x@AF
            frsq <- aue.dummy2af(x@AF, x@INT@loc.fac)
        }
        
        qestr <- function(m, n) beta(m + 1, n + 1)/beta(m + 0.5, n + 1)
        s2 <- function(m, n) beta(m + 1.5, n + 1)/beta(m + 0.5, n + 1) - (beta(m + 1, n + 1)/beta(m + 0.5, n + 1))^2
        
        matrs <- matrix(nrow = nrow(matr), ncol = ncol(matr)/2)
        rownames(matrs) <- rownames(matr)
        
        for (i in seq(1, dim(matr)[2] - 1, 2)) {
            for (j in 1:dim(matr)[1]) {
                matr[j, i] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0, 
                                     NA, 
                                     ifelse(frsq[j, i] + frsq[j, i + 1] == 1, 
                                            frsq[j, i], qestr(frsq[j, i], 
                                                                frsq[j, i + 1])
                                            )
                                     )
                matr[j, i + 1] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0,
                                         NA, 
                                         ifelse(frsq[j, i] + frsq[j, i + 1] == 1, 
                                                frsq[j, i + 1], 
                                                (1 - qestr(frsq[j, i], frsq[j, i + 1]))
                                                )
                                         )
                matrs[j, (i + 1)/2] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0,
                                              NA, 
                                              ifelse(frsq[j, i] + frsq[j, i + 1] == 1, 
                                                     0, s2(frsq[j, i], frsq[j, i + 1])
                                                     )
                                              )
            }
        }
        
        result <- list(matr, matrs)
        names(result) <- c("freq", "var")
        result
    }
    
    
    zhivonu <- function(x) {
        
        if (class(x) == "genpop") {
            matr <- x@tab
        } else if (class(x) == "ecopop") {
          if(x@INT@allele_data == "frequency") {
            stop("ecopop object with genetic data as counts needed, but this object has allele frequencies")
          }
            matr <- x@AF
            frsq <- aue.dummy2af(x@AF, x@INT@loc.fac)
        } else if (class(x) == "matrix" || class(x) == "data.frame") {
            matr <- x
        }
        
        
        qestrnu <- function(m, n, a, b) beta(m + a + 0.5, n + b)/beta(m + a, n + b)
        
        s2nu <- function(m, n, a, b) {
          beta(m + a + 1, n + b)/beta(m + a, n + b) - (beta(m + a + 0.5, n + b)/beta(m + a, n + b))^2
        }
        
        matrs <- matrix(nrow = nrow(matr), ncol = ncol(matr)/2)
        rownames(matrs) <- rownames(matr)
        
        Rm <- rowSums(matr[, seq(1, dim(matr)[2] - 1, 2)])/rowSums(matr)
        
        matR <- matr
        matR[, seq(1, dim(matR)[2] - 1, 2)] <- ifelse(matr[, seq(1, dim(matr)[2] - 1, 2)] == 0, 
                                                      0,
                                                      matr[, seq(1, dim(matr)[2] - 1, 2)]/(matr[, seq(1, dim(matr)[2] - 1, 2)] + matr[, seq(2, dim(matr)[2], 2)])
                                                      )
        matR[, seq(2, dim(matR)[2], 2)] <- ifelse(matr[, seq(2, dim(matr)[2], 2)] == 0,
                                                  0,
                                                  matr[, seq(2, dim(matr)[2], 2)]/(matr[, seq(1, dim(matr)[2] - 1, 2)] + matr[, seq(2, dim(matr)[2], 2)])
                                                  )
        
        VR <- rowSums(matR[, seq(1, dim(matR)[2] - 1, 2)]^2 * (matr[, seq(1, dim(matr)[2] - 1, 2)] + 
                                                                 matr[, seq(2, dim(matr)[2], 2)]))/rowSums(matr[, ]) - Rm^2
        
        a <- (Rm * (1 - Rm)/VR - 1) * Rm
        b <- (Rm * (1 - Rm)/VR - 1) * (1 - Rm)
        
        for (i in seq(1, dim(matr)[2] - 1, 2)) {
            for (j in seq_len(dim(matr)[1])) {
                matr[j, i] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0, 
                                     NA, 
                                     ifelse(frsq[j, i] + frsq[j, i + 1] == 1, 
                                            frsq[j, i], qestrnu(frsq[j, i], frsq[j, i + 1], a[j], b[j])
                                            )
                                     )
                matr[j, i + 1] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0,
                                         NA, 
                                         ifelse(frsq[j, i] + frsq[j, i + 1] == 1,
                                                frsq[j, i + 1], 
                                                (1 - qestrnu(frsq[j, i], frsq[j, i + 1], a[j], b[j])))
                                         )
                matrs[j, (i + 1)/2] <- ifelse(frsq[j, i] + frsq[j, i + 1] == 0, 
                                              NA, 
                                              ifelse(frsq[j, i] + frsq[j, i + 1] == 1, 
                                                     0, 
                                                     s2nu(frsq[j, i], frsq[j, i + 1], a[j], b[j])
                                                     )
                                              )
            }
        }
        
        result <- list(matr, matrs)
        names(result) <- c("freq", "var")
        result
    }
    
    
    rawfreq <- function(x) {
        
      if (class(x) == "genpop") {
        frsq <- makefreq(x)
      } else if (class(x) == "ecopop") {
      if(x@INT@allele_data == "counts") {
        frsq <- aue.dummy2af(x@AF, x@INT@loc.fac)
      } else {
        frsq <- x@AF
      }
      } else if (class(x) == "matrix" || class(x) == "data.frame") {
        matr <- x
      }
      
        matrs <- matrix(nrow = nrow(frsq), ncol = ncol(frsq)/2)
        rownames(matrs) <- rownames(frsq)
        
        frsq[, seq(1, dim(frsq)[2] - 1, 2)] <- sqrt(frsq[, seq(1, dim(frsq)[2] - 1, 2)])
        frsq[, seq(2, dim(frsq)[2], 2)] <- 1 - frsq[, seq(1, dim(frsq)[2] - 1, 2)]
        
        matrs[, 1:dim(matrs)[2]] <- (1 - frsq[, seq(1, dim(frsq)[2] - 1, 2)]^2)/(4 * (frsq[, seq(1, dim(frsq)[2] - 1, 2)] + frsq[, seq(2, dim(frsq)[2], 2)]))
        LM <- frsq
        numer <- frsq[, seq(1, dim(frsq)[2] - 1, 2)]
        denom <- 1 - (matrs[, 1:dim(matrs)[2]])/(8 * (frsq[, seq(1, dim(frsq)[2] - 1, 2)]))^4
        LM[, seq(1, dim(frsq)[2] - 1, 2)] <- numer/denom
        LM[, seq(2, dim(frsq)[2], 2)] <- 1 - LM[, seq(1, dim(frsq)[2] - 1, 2)]
        result <- list(frsq, matrs, LM)
        names(result) <- c("freq", "var", "LM")
        result
    }
    
    # NaN results are avoided by removing NAs in each pairwise estimate
    
    neifreq <- function(x) {
      
      if (class(x) == "genpop") {
        x <- x@tab
      } else if (class(x) == "ecopop") {
        if(x@INT@allele_data == "frequency") {
          stop("ecopop object with genetic data as counts needed, but this object has allele frequencies")
        }
        x <- x@AF
      }
        
        neidistan <- matrix(rep(0, dim(x)[1] * dim(x)[1]), ncol = dim(x)[1], nrow = dim(x)[1])
        # defines the nei distance matrix
        
        for (i in seq_len(dim(x)[1] - 1)) {
            for (j in (i + 1):dim(x)[1]) {
                presente <- which(is.na(x[i, ]) != TRUE & is.na(x[j, ]) != TRUE)
                ja <- sum(x[i, presente]^2)
                jb <- sum(x[j, presente]^2)
                jab <- sum(x[i, presente] * x[j, presente])
                neidistan[i, j] <- -log(jab/sqrt(ja * jb))
                neidistan[j, i] <- -log(jab/sqrt(ja * jb))
            }
        }
        
        rownames(neidistan) <- rownames(x)
        
        # returns the result
        neidistan
    }
    
    if (method == "zhivor") {
        zhivor(x)
    } else if (method == "zhivonu") {
        zhivonu(x)
    } else if (method == "rawfreq") {
        rawfreq(x)
    } else if (method == "neifreq") {
        neifreq(x)
    }
    
}
