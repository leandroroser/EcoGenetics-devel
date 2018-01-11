################################################
#### ECOPOP CONSTRUCTOR
################################################

#' Creating a new ecopop object
#' @param XY Data frame with n rows (populations) and m columns (coordinates).
#' @param P Data frame with n rows (populations), and m columns (phenotypic variables).
#' @param AF Data of class: "matrix", with n rows (populations)
#' and m columns (allele counts).
#' The ploidy and the type (codominant, dominant) of the data, 
#' must be passed with the arguments "ploidy" and "type" for consistency
#' with other methods of the package.
#' @param E Data frame with n rows (populations), and n columns (environmental variables).
#' @param S Vector (factor) with n items (population hierarchical levels).
#' @param C Data frame with n rows (populations), and m columns (custom variables).
#' @param ploidy Ploidy of the AF data frame. 
#' @param type Marker type: "codominant" or "dominant".
#' @param order.df Order populations of data frames by row? 
#' (all data frames with a same row order).
#' Defalut FALSE. The row names of all the data frames must be ordered. In this case,
#' the use of data frames with row names in different order will return an error.
#' In both cases, the program set the content of the S slots as the reference names of the object
#' using the row names of the first non-empty data frame found in the following order: 
#' XY, P, AF, E, C. This attribute is used as reference to order rows when order.df = TRUE. 
#' 
#' @details This is a generic function for creation of ecopop objects.
#' Missing data should be coded as "NA". 
#' 
#' 
#' \strong{ACCESS TO THE SLOTS. MODIFICATION OF ecopop OBJECTS}
#' 
#' The content of the slots can be extracted with the corresponding accessors
#' ecoslot.XY, ecoslot.P, ecoslot.AF, ecoslot.E and ecoslot.C. 
#' Accessors can be also used to assign data to the slots.
#' The correct use of ecopop objects requires the implementation of accessors, 
#' as they ensure the checking and pre-processing of the data. The use of accessors allows to
#' modify or fill the slots of ecopop objects, without the need of creating a new
#' object each time. See \emph{help("EcoGenetics accessors")} for a detailed description 
#' and examples about ecopop accessors. 
#' 
#' \strong{OTHER SLOT ACCESS METHODS FOR ECOPOP OBJECTS}
#' 
#' The use of brackets is defined for ecopop objects:
#' 
#'  - Single bracket: the single bracket ("[") is used to subset all the ecopop
#'  data frames (P, G, E, S, AF and C) by row, at once. The notation for an object
#'  is eco[from:to], where eco is any ecopop object, and from: to is the row
#'  range. For example: my_ecopop[1:10] , subsets the object  my_ecopop from row 1 to row 10, 
#'  for all the data frames at once.
#'  
#' - Double square brackets: the double square brackets are symbolic abbreviations 
#' of the accessors (i.e., it is a call to the corresponding accessor). 
#' The usage is:  my_ecopop[["X"]], where X is a slot:  my_ecopop[["P"]], 
#'  my_ecopop[["AF"]],  my_ecopop[["E"]],  my_ecopop[["S"]] and  my_ecopop[["C"]].
#' Double square brackets can be used in get/set mode. 
#' See Examples below and in help("EcoGenetics accessors").
#' 
#' 
#' \bold{ABOUT THE CONSTRUCTION OF NEW ECOPOP OBJECTS}
#' 
#' In most cases, a new ecopop object is created from an ecogen object, 
#' using the function ecogen2ecopop. A new ecopop object can also 
#' be directly constructed in two different ways. First, a new object can be created, 
#' incorporating all the information in one step with the constructor. 
#' Second, the data can be added to each slot, using the corresponding
#' accessor or, in an equivalent way, with double brackets notation ("[[").
#' 

#' @examples
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' ## Three ways to construct an ecopop object 
#' 
#' ## 1) ecogen to ecopop
#' my_ecopop <- ecogen2ecopop(eco, hier = "pop")
#' 
#' # extracting tables with accessors (double brackets notation)
#' XY_pop <- my_ecopop[["XY"]]
#' AF_pop <- my_ecopop[["P"]]
#' AF_pop <- my_ecopop[["AF"]]
#' E_pop <- my_ecopop[["E"]]
#' S_pop <- my_ecopop[["S"]]
#' 
#' ## 2) Creating a new ecopop object
#' my_ecopop2 <- ecopop(XY = XY_pop, P = XY_pop, AF = AF_pop, E = E_pop, S = S_pop,
#'                      ploidy = 2, type = "codominant")
#' 
#' ## 3) From an empty object
#' # new empty object
#' my_ecopop3 <- ecopop(ploidy = 2, type = "codominant")
#' 
#' set slots, using as example the data generated above
#' 
#' my_ecopop3[["XY"]] <- XY_pop # The first assignments initializes the S slot
#'                             # with the row names of the data frame used (XY)
#' my_ecopop3[["P"]] <- P_pop
#' my_ecopop3[["AF", ploidy = 2]] <- AF_pop
#' my_ecopop3[["E"]] <- E_pop
#' my_ecopop3[["S"]] <- S_pop
#' 
#' ## Subsetting by rows:
#' my_ecopop3[1:10]
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export ecopop


setGeneric("ecopop",      	 
           function(XY = data.frame(),
                    P = data.frame(),
                    AF = data.frame(), 
                    E = data.frame(),
                    S = factor(),
                    C = data.frame(),
                    ploidy,
                    type = c("codominant", "dominant"),
                    order.df = FALSE) {				
             
            
             type <- match.arg(type)
             if(is.null(ploidy) || missing(ploidy)) {
               stop("Please provide the ploidy of your data")
             }
             
             # creating a new ecopop object
             object <- new("ecopop", ploidy, type)
            
             # set object environment
             object@ATTR$whereIs <- parent.frame()
             object@ATTR$.call <- match.call()
             
             object@XY <- as.data.frame(XY)
             object@P <- as.data.frame(P)
             object@AF <- as.matrix(AF)
             mode(object@AF)<- "integer"
             object@E <- as.data.frame(E)
             object@S <- as.factor(S)
             object@C <- as.data.frame(C)
             
             # order rows
             if(order.df) {
               object@XY = object@XY[ , match(rownames(object@XY), object@S) ]
               P = object@P[ , match(rownames(object@P), object@S) ]
               AF = object@AF[ , match(rownames(object@AF), object@S) ]
               E = object@E[ , match(rownames(object@E), object@S) ]
               C = object@C[ , match(rownames(object@C), object@S) ]
             }
             
             validObject(object)
             
             object
             
           })
