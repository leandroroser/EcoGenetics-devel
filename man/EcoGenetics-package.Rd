\name{EcoGenetics-package}
\alias{EcoGenetics-package}
\alias{EcoGenetics}
\docType{package}
\title{
 Management and Exploratory Analysis of Spatial Data in Landscape Genetics
}
\description{
Management and exploratory analysis of spatial data in landscape genetics. Easy integration of information from multiple sources with "ecogen" objects.
}


\details{
 \tabular{ll}{
 Package: \tab EcoGenetics\cr
 Type: \tab Package\cr
 Version: \tab 1.2.1-2\cr
 Date: \tab 2017-11-21\cr
 License: \tab GPL (>=2) \cr
 }
 
 
 
 
 ----------------------- o-----------------------
 
  \bold{\emph{I. STRUCTURE OF THE PACKAGE}}

EcoGenetics has four basic modules. The \strong{base module} is composed by general functions (\link[=eco.lmtree]{multiple lm}, \link[=eco.detrend]{detrending spatial data utility}, etc.). 
The \strong{general spatial module} computes global (\link[=eco.gsa]{Moran's I}, \link[=eco.mantel]{Mantel test}, etc.) and local (\link[=eco.lsa]{Getis-Ord's G*}, \link[=eco.lsa]{local Moran's I}, etc.) spatial tests. These analyses use a \link[=eco.weight]{spatial weights matrix}, provided by the  \strong{spatial weights module}.  The \strong{lag analysis module} performs two basic analyses: the obtention  of \link[=eco.variogram]{variograms} and \link[=eco.correlog]{correlograms} (see also \link[=eco.cormantel]{this link}). This module uses the other tool provided by the spatial weights module: a sequence of \link[=eco.lagweight]{spatial weights matrices}. 

The package have also special plot methods, as \link[=eco.rankplot]{rankplot},  
\link[=eco.forestplot]{forestplot}, \link[=eco.rasterplot]{rasterplot} (implemented the last two for 
\link[=eco.lsa]{local spatial analysis}). Several conversor of data from/to other programs are available, (as to \link[=ecogen2genepop]{genepop} - an \link[=genepop2ecogen]{importer} tool is also defined for genepop-, \link[=ecogen2spagedi]{SPAGeDi}, etc.). Basic manipulation of genetic matrices is allowed by \link{eco.convert} and \link{eco.format}. Tools for computation of NDVI in Landsat imaginery, 
post-process of rasters and temporal analysis can be found in \link{eco.NDVI}, \link{eco.NDVI.post} and \link{eco.theilsen}. Other useful functions are \link{aue.sort} (for ordering alleles), \link{eco.alfreq} (to comput histograms of allelic frequencies for detection of bottlenecks), \link{eco.post.geneland} and \link{eco.pairtest}. 

The results obtained with the main functions defined in EcoGenetics are
object of class \link[=Methods]{S4}. As a default characteristic of the package design,
these objects have a "show"" method for a general overview of the results, and methods to extract the results stored in slots (generic \link[=ecoslot.XY]{accessors} and double square brackets (\link[=Extract]{"[["}) definitions). 

For storing and pre-processing the data to analyze, the package 
defines a special class: the class \code{\link{ecogen}}.
 
 \bold{\emph{II. STRUCTURE OF ECOGEN OBJECTS: HANDLING AND INTEGRATING INFORMATION}}


Landscape genetics research requires the integration of data originated in different sources. The class \code{\link{ecogen}} has been designed for handling multidimensional data. Its basic structure is the following:

- An \strong{XY} slot, storing a data frame with geographic coordinates.  

- A \strong{P} slot, storing a phenotypic data frame.  

- A \strong{G} slot, storing a genotypic data frame. 

- An \strong{A} slot containing as allelic frequencies the information of G (only
available for codominant markers.)

- An \strong{E} slot, storing an environmental data frame.  

- A \strong{S} slot, storing a data frame with classes assigned to the 
individuals. 


- A \strong{C} slot, for a custom data frame. 

- An \strong{OUT} slot, containing a list for the storage of the results.


For dominant (presence-absence) markers, the slot A is empty.


\bold{\emph{III. A BRIEF OVERVIEW OF THE DATA HANDLING METHODS DEFINED FOR ECOGEN OBJECTS}}


The construction of a new "ecogen" object from a data frame is made with the homonymous function.

\emph{library("EcoGenetics")}

\emph{data(eco.test)}

\emph{eco <- ecogen(XY = coordinates, P = phenotype, G = genotype, E = environment, S = structure, order.G = TRUE)}


# The following methods can be used with ecogen objects:

# - - - - - - 

\bold{# 1. subsetting/ordering by row-method, using single square brackets ("[")}

\emph{eco.sub <- eco[1:50]}

\emph{eco.ord <- eco[c(50:1)]}

\emph{eco.ord[["XY"]] # object ordered and subset rows.
This is a powerful method for handling the stack of ecogen data frames}

\emph{eco <- eco[order(eco[["S"]][, 1])] #ordering S[, 1] and then the object}

\bold{# 2. \link[=eco.merge]{merging-method}, for two objects}

\emph{eco1 <- eco}

\emph{merged <- eco.merge(eco, eco1)}

\bold{# 3. \link[=eco.subset]{subsetting-method}, in reference to a group in the S slot (in this case, "1")}

\emph{eco.subS <- eco.subset(eco,"pop", 1)}


\bold{# 4. \link[=eco.rbind]{binding by row-method} (duplicated row names not allowed)}

\emph{eco2 <- eco; rownames(eco2[["P"]]) <-226:450}

\emph{eco.r <- eco.rbind(eco, eco2)}

\bold{# 5. \link[=eco.cbind]{binding by column-method}}

\emph{eco.c <- eco.cbind(eco, eco1)}


\bold{# 6. get-method using generic \link[=ecoslot.XY]{accessors} and "[[" (equivalent methods):}

\emph{ecoslot.XY(eco) ; eco[["XY"]]}

\emph{ecoslot.P(eco); eco[["P"]]}

\emph{ecoslot.G(eco); eco[["G"]]}

\emph{ecoslot.A(eco); eco[["A"]]  (only available for codominant data}

\emph{ecoslot.E(eco); eco[["E"]]}

\emph{ecoslot.S(eco); eco[["S"]]}

\emph{ecoslot.C(eco); eco[["C"]]}

\emph{ecoslot.OUT(eco); eco[["OUT"]]}


\bold{# 7. set-method using \link[=ecoslot.XY]{accessors} and "[[" (equivalent methods):}

\emph{eco.temp <- ecogen(XY = coordinates, P = phenotype)}

\emph{eco.temp}

\emph{ecoslot.G(eco.temp, order.G = TRUE) <- genotype}

## this is equivalent, in square brackets notation, to:

\emph{eco[["G", order.G=TRUE]] <- genotype}

\emph{ecoslot.E(eco.temp) <- environment}

## identical to eco[["E"]] <- environment

\emph{ecoslot.S(eco.temp) <- structure}

## identical to eco[["S"]] <- structure

\emph{eco.temp}

\bold{# 8. appending-method (storing information generated via \link[=ecoslot.XY]{accessors)}}

## fitting a multiple linear regression model:

\emph{linear.analysis <- eco.lmtree(eco[["P"]], eco[["E"]], "mlm")}

## storing the results:

\emph{ecoslot.OUT(eco) <- linear.analysis}

\emph{eco}

## Storing multiple result at once:
 
\emph{a <- c(1:10)}

\emph{b <- c(2:30)}
  
\emph{ecoslot.OUT(eco) <- list(eco, a, b)}

\emph{eco}

## the use of the accessor OUT has its equivalence in double
square brackets notation:

\emph{eco[["OUT"]] <- list(eco, a, b)}


## summary table

\emph{ecoslot.OUT(eco)}
  
## the data frame shows the results stored in alphabetical order and their classes. The specification of a second name, return the corresponding stored object:

\emph{ecoslot.OUT(eco, "a")} 

## note that the append method is a particular case of the
## set method with accessors / "[[" for the slot OUT.

\bold{# 9. \link[=eco.remove]{removing-method}}

## removing objects a and b from eco

\emph{eco <- eco.remove(eco, a, b)}




- - - - - - 

The workspace can be \link[=eco.clear]{cleared}, only storing the desired object:

\emph{ls()}

\emph{eco.clear(eco)}

\emph{ls()}

The cleaner and compact option, is to save the object as \link[=saveRDS]{RDS},
which is a way to save R objects as a file:

\emph{saveRDS(eco, "myObject.rds")}

The object can be loaded in the future with the function readRDS:

\emph{eco <- readRDS("myObject.rds")}

 ----------------------- o -----------------------

}


\author{

 Leandro Roser, Juan Vilardi, Beatriz Saidman and Laura Ferreyra
 
 Maintainer: Leandro Roser <learoser@gmail.com>
}

\references{
 
Roser LG, Ferreyra LI, Saidman BO, Vilardi JC (2017). EcoGenetics: An R
package for the management and exploratory analysis of spatial data in
landscape genetics. Molecular Ecology Resources, 17:e241-e250. doi:
10.1111/1755-0998.12697
 
}
