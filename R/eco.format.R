#' Format tool for genetic data
#' 
#' @details The function can format data with different ploidy levels. 
#' It allows to:
#' - add/remove zeros at the beginning/end of each allele
#' - separate alleles with a character
#' - divide alleles into columns - bind alleles from separate columns
#' - transform character data into numeric data
#' 
#' "NA" is considered special character (not available data).
#' 
#' Note that the function also works with other data, with states equivalent to "alleles".
#'
#' @param data Genetic data frame.
#' @param ncod Number of digits coding each allele in the input file.
#' @param nout Number of digits in the output.
#' @param ploidy Ploidy of the data.
#' @param sep.in Character separating alleles in the input data if present.
#' @param sep.out Character separating alleles in the output data. Default 
#' @param fill.mode Add zeros at the beggining ("fist") or the end ("last")
#' of each allele. Default = "last". 
#' @param recode Recode mode: "none" for no recoding (defalut), "all" for recoding
#' the data considering all the individuals values at once (e.g., protein data), 
#' "column" for recoding the values by column (e.g., microsatellite data), "paired" 
#' for passing the values of allelic states and corresponding replacement values, using 
#' the replace_in and replace_out arguments (e.g. replace_in = c("A", "T", "C", "G"),
#' replace_out = c(1,2,3,4)).
#' @param replace_in vector with states of the data matrix to be replaced, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_out".
#' @param replace_out vector with states of the data matrix used for replacement, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_in".
#' @param show.codes May we returned tables with the equivalence between the old 
#' and new codes when recode = "all" or recode = "column"?
#' 
#' @examples
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' # Adding zeros
#' 
#' example <- as.matrix(genotype[1:10,])
#' mode(example) <- "character"
#' # example data
#' example                
#' recoded <- eco.format(example, ncod = 1, ploidy = 2, nout = 3)
#' # recoded data
#' recoded
#' 
#' 
#' # Tetraploid data, separating alleles with a "/"
#' tetrap <- as.matrix(example)
#' # simulated tetraploid example data
#' tetrap <- matrix(paste(example,example, sep = ""), ncol = ncol(example)) 
#' recoded <- eco.format(tetrap, ncod = 1, ploidy = 4, sep.out = "/")
#' # recoded data
#' recoded
#' 
#' # Example with a single character
#' ex <- c("A","T","G","C")
#' ex <- sample(ex, 100, rep= T)
#' ex <- matrix(ex, 10, 10)
#' colnames(ex) <- letters[1:10]
#' rownames(ex) <- LETTERS[1:10]
#' # example data
#' ex  
#' recoded <- eco.format(ex, ploidy = 1, nout = 1,  recode = "all", show.codes = TRUE) 
#' # recoded data 
#' recoded
#' 
#' # Example using values-replacement pairs
#' samp1 <- sample(c("A","T","G","C"), 100, replace = TRUE)
#' samp2 <- sample(c("A","T","G","C"), 100, replace = TRUE)
#' paired <- matrix(paste0(samp1, samp2), 10, 10)
#' # Generate some NAs
#' paired[sample(1:100, 10)]<-NA
#' out <- eco.format(paired, recode = "paired", replace_in = c("A", "T", "G", "C"),
#' replace_out = c(1, 2, 3, 4))
#' out
#' 
#' 
#' # Example with two strings per cell and missing values:
#' ex <- c("Ala", "Asx", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile", 
#' "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr", 
#' "Val", "Trp")
#' ex1 <- sample(ex, 100, rep= T)
#' ex2 <- sample(ex, 100, rep= T)
#' ex3 <- paste(ex1, ex2, sep="")
#' missing.ex3 <- sample(1:100, 20)
#' ex3[missing.ex3] <-NA
#' ex4 <- matrix(ex3, 10, 10)
#' colnames(ex4) <- letters[1:10]
#' rownames(ex4) <- LETTERS[1:10]
#' # example data
#' ex4
#' recoded <- eco.format(ex4, ncod = 3, ploidy = 2, 
#'                       nout = 2, recode = "column")
#' # recoded data
#' recoded
#' 
#' # Example with a vector, following the latter example:
#' ex1 <- as.data.frame(ex1)
#' # example data
#' ex1
#' recoded <- eco.format(ex1, ploidy = 1, recode = "all")
#' # recoded data
#' recoded
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.format", function(data, 
                                  ncod = NULL, 
                                  nout = 3, 
                                  ploidy = 2, 
                                  sep.in = "",
                                  sep.out = "",
                                  fill.mode = c("last", "first", "none"),
                                  recode = c("none", "all", "column", "paired"),
                                  replace_in = NULL,
                                  replace_out =NULL,
                                  show.codes = FALSE) {
  
  
  fill.mode <- match.arg(fill.mode)
  recode <- match.arg(recode)
  
  rep1 <- !is.null(replace_in)
  rep2 <- !is.null(replace_out)

  # set arguments for paired case
  if((rep1 || rep2)) {
    if(!(rep1 && rep2) || length(rep1) != length(rep2)) {
    stop("both replace_in and replace_out must be non null and of same length
         if one of these arguments is used")
    }
    paired_replacement <- TRUE
    replace_in <- as.character(replace_in)
    replace_out <- as.character(replace_out)
  } else {
    paired_replacement <- FALSE
  }

  data <- as.matrix(data, rownames.force = TRUE)
  
  #stop if <sep.loc> is present in the data
  if((sep.out != "") && any(grep(sep.out, data))) {
    stop(paste(sep.out, "is already in use in the matrix. 
               Choose other <sep.out> character"))
  }
  
  mode(data) <- "character"
  data <- aue.rmspaces(data)
  data <- int.check.colnames(data)
  data <- int.check.rownames(data)
  indnames <- rownames(data)
  locnames <- colnames(data)
  
  #----control---------------------------------------------------------#  
  
  # recode check
  if(any(grep("[^[:digit:]]", data)) && recode == "none") {
    stop("Non numeric characters found. Use the argument 'recode'")
  }
  
  # ploidy check
  if(is.null(ncod)) {
    ncode_in <- FALSE
  } else {
    ncode_in <- TRUE
  }
  ncod <- int.check.ncod(data, ploidy = ploidy, ncod = ncod)
  
  if(recode == "none" && nout < ncod) {
    stop("nout (output number of digits) < ncod 
         (input number of digits per allele) is not valid")
  }
  
  
  ###########------RECODING CASE-----------####################################
  if(recode != "none") {
    
    #----------- function for recoding-----------------#
    singlechar <- function(x) {
      
      y <- as.vector(as.matrix(x))
      y <- as.factor(y)
      y.lev <- levels(y)
      num_states <- length(y.lev)
      original.code <- y.lev[!is.na(y.lev)]
      y <- as.numeric(y)
      ncod.y <- nchar(y, keepNA = TRUE)
      
      if(nout < nchar(num_states)) {
        stop("nout (output number of digits): ", nout, ", but the number of allelic states 
             found are: ", num_states)
      }
      
      #ncod.y[is.na(y)] <- NA
      ncl <- length(y)
      # recoding data
      
      if(fill.mode ==  "last") {
        if(nout>1) {
        add.dig <- paste(rep(1, ncl), paste(rep(0, nout-1), collapse = ""), sep = "")
        add.dig <- as.numeric(add.dig)
        y <- y + add.dig
        }
        y <- as.character(y)
        
      } else {
        y <- as.character(y)
        if(nout>1) {
        add.zeros <- nout-ncod.y
        add.zeros[is.na(add.zeros)] <- 0
        for(i in seq_len(ncl)) {
          zero.init <- paste(rep("0", add.zeros[i]), collapse = "")
          y[i] <- paste(zero.init, y[i],  sep = "")
        }
        }
      }
      
      y <- matrix(y, ncol = ncol(x))
      # codes
      y.cod <- as.factor(y)
      new.code <- levels(y.cod)[which(levels(y.cod)!= "NA")]
      y.tab <- data.frame(original.code, new.code)
      
      list(y, y.tab)
      
      }
    #--------------------------------------------------#
    
    #alleles in columns
    X <- int.loc2al(X = data, ncod = ncod, ploidy = ploidy)
    X <- gsub("(NA)+", NA, X)
    # for recoding all data
    
    if(paired_replacement) {
      nlev <- levels(as.factor(X))
      nlev_len <- length(nlev)
      nin_len <-  length(replace_in)
      if(nlev_len != nin_len) {
        stop(paste0("The number of arguments used for replacement (", nin_len, ") differ of the number of 
           data states (", nlev_len, ")"))
      }
      
      cat(paste0("Levels found in the data: ", paste(nlev, collapse = ", ")), "\n")
      cat(paste0("Replacements: ", paste(replace_in, replace_out, sep = "=", collapse=", ")), "\n")
      conv_list <- matrix( c(replace_in, replace_out), nrow= length(replace_in), dimnames = list(seq_len(nin_len), c("original_code", "new_code")))
      
      
      X[] <- replace_out[match(X, replace_in)]
    
    } else if(recode == "all") {
      recoding <- singlechar(X)
      # create objects with the recoded data
      X <- recoding[[1]]
      conv.list <- recoding[[2]]
      
    } else if(recode == "column") {
      X <- int.al2listal(X, ncod = ncod, ploidy = ploidy)
      recoding <- lapply(X, singlechar)
      X <- lapply(recoding, function(y) y[[1]])
      X <- do.call(cbind, X)
      conv.list <- lapply(recoding, function(y) y[[2]])
    }
    
    ###########------NO RECODING CASE-----------####################################
    
  } else  {
    
    X.list <- int.loc2listal(X = data, ncod = ncod, ploidy = ploidy)
    
    X <- lapply(X.list,     
                function(x) {
                  if(fill.mode ==  "last") {
                    x <-  paste(x, paste(rep("0", nout - ncod), collapse = ""), sep = "")
                    x<- matrix(x, ncol = ploidy)
                  } else {
                    x <-  paste(paste(rep("0", nout - ncod), collapse = ""), x, sep = "")
                    x <- matrix(x, ncol = ploidy)
                  }
                })
    
    
    X <- do.call(cbind, X)
  }
  
  # collapse alleles
  X <- suppressMessages(int.al2loc(X, ploidy = ploidy, sep.out = sep.out))
  
  
  # correction for NA
  X <- gsub("(^NA)(0*$)|(^0*)(NA$)", "NA", X)
  
  rownames(X) <- indnames
  colnames(X) <- locnames
  
  if(recode != "none" && show.codes == TRUE) { 
    X <- list(out = X, codes = conv.list)
  }
  
  X
  
  })

