% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecopop.3OF6.basic.methods.R
\docType{methods}
\name{eco.lock,ecopop-method}
\alias{eco.lock,ecopop-method}
\alias{eco.lock,ecopop}
\title{Lock rows in an ecogen object}
\usage{
\S4method{eco.lock}{ecopop}(object, set.names = NULL, valid.names = FALSE,
  order.df = FALSE)
}
\arguments{
\item{object}{ecopop object}

\item{set.names}{Character vector with names for the rows of the non-empty data frames. 
This argument is incompatible with valid.names}

\item{valid.names}{Logical. Create valid row names? This argument is incompatible with 
set.names. The program will name individuals with valid tags I.1, I.2, etc.}

\item{order.df}{Order individuals of data frames by row? (all data frames with a same order in row names).
 This option is only available when the 'lock.rows' parameter is TRUE. 
If the names of the data frames are not used (i.e., set.names and valid.names are not NULL),
setting this parameter to TRUE/FALSE has no effect in the function. 
Defalut TRUE. If FALSE, the row names of all the data frames must be ordered. The use of data frames 
with row names in different order will return an error.
In both cases, the program sets an internal names attribute of the object
using the row names of the first non-empty data frame found in the following order: 
XY, P, G, E, S, C. This attribute is used as reference to order rows when order.df = TRUE.}
}
\description{
This methods locks the rows in an ecogen object.  When rows are locked,
the object requires rows with identical indviduals in the non empty data frames, and
identity in the row names of the data frames.
}
\examples{
\dontrun{
data(eco.test)
my_ecopop2 <- eco.unlock(my_ecopop)
is.locked(my_ecopop2) 
my_ecopop3 <- eco.lock(my_ecopop)
is.locked(my_ecopop3) 
}

}
