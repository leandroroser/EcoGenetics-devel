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
 Version: \tab 1.2.1-3\cr
 Date: \tab 2018-01-10\cr
 License: \tab GPL (>=2) \cr
 }
 
 
 
 
 ----------------------- o-----------------------
 
  \bold{\emph{I. STRUCTURE OF THE PACKAGE}}

The \strong{spatial analysis module} of the package computes global (\link[=eco.gsa]{Moran's I}, \link[=eco.mantel]{Mantel test}, etc.) and local (\link[=eco.lsa]{Getis-Ord's G*}, \link[=eco.lsa]{local Moran's I}, etc.) spatial tests, \link[=eco.variogram]{variograms} and \link[=eco.correlog]{correlograms} (see also \link[=eco.cormantel]{this link}). These analyses  use other tool provided by the package: \link[=eco.weight]{spatial weights matrices} or a sequence of \link[=eco.lagweight]{spatial weights matrices}. 

The package have also special plot methods for each of the analyses. Several conversor of data from/to other programs are available, (as to \link[=ecogen2genepop]{genepop} - an \link[=genepop2ecogen]{importer} tool is also defined for genepop-, \link[=ecogen2spagedi]{SPAGeDi}, etc.). Basic manipulation of genetic matrices is allowed by \link{eco.convert} and \link{eco.format}. 

Tools for computation of NDVI in Landsat imaginery, post-process of rasters and temporal analysis can be found in \link{eco.NDVI}, \link{eco.NDVI.post} and \link{eco.theilsen}. Several other useful functions are defined in the package.

The results obtained with the main functions defined in EcoGenetics are
object of class \link[=Methods]{S4}. For these objects, the package defines a "show" method for a general overview of the results, and methods to extract the information (generic \link[=ecoslot.XY]{accessors} and double square brackets (\link[=Extract]{"[["}) definitions). 

For storing and pre-processing the data, the package 
defines two special classes: \link[=ecogen]{ecogen} and \link[=ecopop]{ecopop}
 
 \bold{\emph{II.ECOGEN OBJECTS}}


Landscape genetics research requires the integration of data originated in different sources. The class \link[=ecogen]{ecogen} has been designed for handling multidimensional data of individuals. Its basic structure is the following:

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


The construction of a new "ecogen" object from a data frame is made with the homonymous function.

\emph{library("EcoGenetics")}

\emph{data(eco.test)}

\emph{eco <- ecogen(XY = coordinates, P = phenotype, G = genotype, E = environment, S = structure, order.G = TRUE)}


The package defines several methods for manipulation of ecogen objects:
\link[=eco.cbind]{eco.cbind}, to bind ecogen objects by column,
\link[=eco.split]{eco.split}, to split ecogen objects by group,
\link[=eco.rbind]{eco.rbind}, to bind ecogen objects by row and to re-bind previously splitted objects,
\link[=eco.subset]{eco.subset}, to select a group of individuals, given a population,
\link[=eco.merge]{eco.merge}, to merge ecogen objects with different composition of  individuals. 
The functions nrow, ncol, dim, names, as.list, is.ecogen and show are also defined.
Single brackets ("[") and double brackets ("[[") are also defined to select a subset of 
individuals or to get/set the data of a particular slot, respectively. Conversors \link[=genind2ecogen]{from} and \link[=ecogen2genind]{to} genind,  \link[=spagedi2ecogen]{from} and \link[=ecogen2spagedi]{to} spagedi, \link[=gstudio2ecogen]{from} and \link[=ecogen2gstudio]{to}  genepop,  \link[=ecogen2hierfstat]{to} hierfstat and  \link[=ecogen2geneland]{to} geneland are included. See the \link[=ecogen]{ecogen constructor} documentation and the publication of the package, cited at the end of this document, for more information.



\bold{\emph{III. ECOPOP OBJECTS}}

The class \link[=ecopop]{ecopop} is the population analogue of ecogen class: while ecogen rows represent individuals, ecopop rows represent populations.
An ecopop object contains aggregated data for each population (mean or other statistics of cuantitative variables, and counts for each level of cualitative variables [including allele counts]).
In normal situations, an ecopop object is obtained from an ecogen object as follows:

\emph{ecopop_object <- ecogen2ecopop(eco, hier = "pop")} # where pop is the name of the
                                                         # column of the slot S with
                                                         # the population of each
                                                         # individual

ecopop objects can also be obtained from the \link[=ecopop]{ecopop constructor}, as detailed in the documentation of the function.

The functions nrow, ncol, dim, names, as.list, is.ecogen and show are defined for ecopop objects.
Single brackets ("[") and double brackets ("[[") are also defined to select a subset of 
individuals or to get/set the data of a particular slot, respectively. See \link[=ecopop]{ecopop constructor} documentation for more information. Conversion \link[=genpop2ecopop]{from} and \link[=ecopop2genpop]{to} \link[adegenet]{genpop} is defined.

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
