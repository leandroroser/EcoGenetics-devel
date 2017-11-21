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
 
 Maintainer: Leandro Roser <leandroroser@ege.fcen.uba.ar>
}

\references{
 
Anselin L. 1995. Local indicators of spatial association-LISA. Geographical analysis. 27: 93-115.

Borcard D., F. Gillet, and P. Legendre. 2011. Numerical ecology with R. Springer Science & Business Media.

Chander G., B. Markham, and D. Helder. 2009. Summary of current radiometric calibration coefficients for Landsat MSS, TM, ETM+, and EO-1 ALI sensors. Remote sensing of environment, 113: 893-903.

Chavez P. 1989. Radiometric calibration of Landsat Thematic Mapper multispectral images. Photogrammetric Engineering and Remote Sensing, 55: 1285-1294.

Chavez P. 1996. Image-based atmospheric corrections-revisited and improved. Photogrammetric engineering and remote sensing, 62: 1025-1035.

Double M., R. Peakall, N. Beck, and Y. Cockburn. 2005. Dispersal, philopatry, and infidelity: dissecting local genetic structure in superb fairy-wrens (Malurs cyaneus). Evolution 59: 625-635.

Dray S., and A., Dufour. 2007. The ade4 package: implementing the duality diagram for ecologists. Journal of statistical software, 22: 1-20.
 
Freedman D., and P. Diaconis. 1981. On the histogram as a density estimator: L 2 theory. Probability theory and related fields, 57: 453-476.
  
Geary R. 1954. The contiguity ratio and statistical mapping. The incorporated statistician, 115-146.

Getis A., and J. Ord. 1992. The analysis of spatial association by use of distance statistics. Geographical analysis, 24: 189-206.

Goslee S. 2011. Analyzing remote sensing data in R: the landsat package. Journal of Statistical Software, 43: 1-25.

Goudet J. 2005. Hierfstat, a package for R to compute and test hierarchical F-statistics. Molecular Ecology Notes, 5: 184-186.
 
Guillot G., F. Mortier and A. Estoup. 2005. GENELAND: a computer package for landscape genetics. Molecular Ecology Notes, 5: 712-715.
 
Jombart T. 2008. adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics, 24: 1403-1405.

Kalisz S., J. Nason, F.M. Handazawa, and S. Tonsor. 2001. Spatial population genetic structure in Trillium grandiflorum:  the roles of dispersal, mating, history, and selection.  Evolution 55: 1560-1568.

Legendre P., and L. Legendre. 2012. Numerical ecology. Third English edition .Elsevier Science, Amsterdam, Netherlands.

Lichstein J., T. Simons, S. Shriner, and K. Franzreb. 2002. Spatial autocorrelation and autoregressive models in ecology. Ecological monographs, 72: 445-463.

Loiselle B., V. Sork, J. Nason, and C. Graham. 1995. Spatial genetic structure of a tropical understory shrub, Psychotria officinalis (Rubiaceae). American Journal of Botany 1420-1425.

Moran P. 1950. Notes on continuous stochastic phenomena. Biometrika, 17-23.

Oden, N., 1984. Assessing the significance of a spatial correlogram. 
Geographical Analysis, 16: 1-16.

Oden N., and R. Sokal. 1986. Directional autocorrelation: an extension of spatial correlograms to two dimensions. Systematic Zoology, 35: 608-617.

Ord J., and A. Getis. 1995. Local spatial autocorrelation statistics: distributional issues and an application. Geographical analysis, 27: 286-306.

Reich R., R. Czaplewski and W. Bechtold. 1994. Spatial cross-correlation of undisturbed, natural shortleaf pine stands in northern Georgia. Environmental and Ecological Statistics, 1: 201-217.

Sokal R., and N. Oden 1978. Spatial autocorrelation in biology: 1. Methodology. Biological journal of the Linnean Society, 10: 199-228.

Sokal R., and N. Oden. 1978. Spatial autocorrelation in biology. 2. Some biological implications and four applications of evolutionary and ecological interest. Biological Journal of the Linnean Society, 10: 229-49.

Sokal R. 1979. Ecological parameters inferred from spatial correlograms. In: G. Patil and M. Rosenzweig, editors. Contemporary Quantitative Ecology and elated Ecometrics. International Co-operative Publishing House: Fairland, MD, pp. 167-96.

Sokal R. 1986. Spatial data analysis and historical processes. In: E. Diday, Y. Escoufier, L. Lebart, J. Pages, Y. Schektman, and R. Tomassone, editors. Data analysis and informatics, IV. North-Holland, Amsterdam, The Netherlands, pp. 29-43.

Sokal R., N. Oden and B. Thomson. 1998. Local spatial autocorrelation in a biological model. Geographical Analysis, 30: 331-354.

Sokal R., and B. Thomson. 2006. Population structure inferred by local spatial autocorrelation: an example from an Amerindian tribal population. American journal of physical anthropology, 129: 121-131.

Song C., C. Woodcock, K. Seto, M. Lenney and S. Macomber. 2001. Classification and change detection using Landsat TM data: when and how to correct atmospheric effects?. Remote sensing of Environment, 75: 230-244.

Sturges H. 1926. The choice of a class interval. Journal of the American Statistical Association, 21: 65-66.

Tucker C. 1979. Red and photographic infrared linear combinations for monitoring vegetation. Remote sensing of Environment, 8: 127-150.

Vekemans, X., and O. Hardy. 2004. New insights from fine-scale spatial genetic structure analyses in plant populations. Molecular Ecology, 
13: 921-935.

Wu. 1986. Jackknife, bootstrap and other resampling methods in regression analysis. the Annals of Statistics, 1261-1295.
 
}
