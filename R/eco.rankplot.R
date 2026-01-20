#' Rankplot graphs
#' 
#' @description This function generates a plot for a numeric or
#' factor variable. A data frame/matrix with XY coordinates is required.
#' The X and Y axes in the plot correspond 
#' to the rank of the X and Y coordinates, respectively. 
#' 
#' @param XY Data frame or matrix with X-Y coordinates.
#' @param input Numeric/factor variable.
#' @param xlabel Optional label for x axis.
#' @param ylabel Optional label for y axis.
#' @param title Optional title label.
#' @param background Background color ("grey" or "white")-
#' @param legendlabel Optional legend label.
#' @param significant Should only the individuals with significant 
#' result be colored?. This argument can be used with \code{\link{eco.lsa}} results. 
#' Default TRUE
#' @param ns Color for non significant individuals, when significant = TRUE.
#' This argument can be used with \code{\link{eco.lsa}} results.
#' @param rescaled rescale values to [-1, 1] range?
#' @param interactivePlot Show an interactive plot via plotly? (default: TRUE)
#' @param ... Additional elements to the generic.
#' 
#' @examples
#' \dontrun{
#' data(eco3)
#' 
#' # The data set eco3 has 50 points in two sites, 
#' # but points are not visible in a usual X-Y plot, 
#' # due to the small distance among them in relation to the large
#' # distance between sites
#' 
#' var <- eco3[["P"]][,1]
#' plot(eco3[["XY"]], col = var)
#' x <- sample(1:100, 30)
#' y <- sample(1:100, 30)
#' 
#' # in a rankplot graph, the inter-individual distances are
#' # reduced to a single scale
#' rankeco3 <- eco.rankplot(var, eco3[["XY"]])
#' rankeco3
#' 
#' # the rankplot method supports the use of ggplot2 syntax with ggplot2 graphs
#' require(ggplot2)
#' rankeco3 <- eco.rankplot(var, eco3[["XY"]], interactivePlot = FALSE)
#' rankeco3 <- rankeco3 + theme_bw() + theme(legend.position="none")
#' rankeco3
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @rdname rankplot-methods
#' 
#' @export eco.rankplot


setGeneric("eco.rankplot", function(input, 
                                    XY, 
                                    xlabel = NULL,
                                    ylabel = NULL,
                                    title = NULL,
                                    legendlabel = NULL,
                                    background = c("grey", "white"),
                                    ...) {
  standardGeneric("eco.rankplot")
})

#-------------------------------------------------------------------#
#' @rdname rankplot-methods
#' @aliases eco.rankplot,eco.lsa-method
#' @exportMethod eco.rankplot

setMethod("eco.rankplot", 
          c("eco.lsa", 
            "missing", 
            "missing"),
          function(input, 
                   XY, 
                   xlabel,
                   ylabel,
                   title,
                   legendlabel,
                   background = c("grey", "white"),
                   significant = TRUE,
                   rescaled = FALSE,
                   ns = NULL,
                   interactivePlot = TRUE) {
            
            
            if(interactivePlot) {
              axis.size = 9
              title.size = 13
            } else {
              axis.size = 10
              title.size = 14
            }
            
            if((input@TEST)[1] != "permutation" && input@NSIM != 0) {
              stop("this method is available for eco.lsa with permutation test")
            }
            
            theme <- match.arg(background)
            if(!is.null(ns)) {
              col.ns <- ns
            }
            if(theme == "grey") {
              themecol <-  ggplot2::theme_grey()
              if(is.null(ns)) {
                col.ns <- "moccasin"
              }
            } else {
              themecol <- ggplot2::theme_bw()
              if(is.null(ns)) {
                col.ns <- "white"
              }
            }
            
            XY <- input@XY
            
            x <- rank(XY[, 1], na.last = "keep")
            y <- rank(XY[, 2], na.last = "keep")
            method <- input@METHOD
            
            if(rescaled) {
              z <- input@OUT$obs.res
            } else {
              z <- input@OUT$obs
            }
            
           if(input@NSIM != 0) {
            p <- input@OUT$p.val
           } else  {
             significant <- FALSE
             p <- rep(NA, length(x))
           }
            
            if(significant == TRUE) {
              alpha <- as.numeric(p < 0.05)
              z <- alpha * z
            }
            
            data <- data.frame(x, y, z)
            data[which(is.na(z)), ] <- NA
            
            
            if(significant == TRUE) {
              data2 <- data[-which(p < 0.05), ]
              p.points <- ggplot2::geom_point(data = data2, 
                                              colour =col.ns, 
                                              size = 3) 
            }
            
            
            if(is.null(xlabel)) {
              xlabel <- "X rank"
            } 
            
            if(is.null(ylabel)) {
              ylabel <- "Y rank"
            } 
            
            if(is.null(title)) {
              if(!is.null(input@NAMES)) {
                title <- input@NAMES
              } else {
              title <- " "
              }
            }
            
            if(is.null(legendlabel)) {
              legendlabel <- paste(" ", method)
            }
            
            rankplot <- ggplot2::ggplot(data, ggplot2::aes(x = x,y = y)) +
              ggplot2::geom_point(colour="grey50", size = 4.5)+
              ggplot2::geom_point(ggplot2::aes(colour = z), 
                                  size=3)+ 
              ggplot2::scale_color_gradient2(legendlabel, 
                                             high= scales::muted("red"),
                                             low = scales::muted("blue"))+
              themecol +
              ggplot2::xlab(xlabel) +
              ggplot2::ylab(ylabel) +
              ggplot2::labs(title = title) +
              ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                             axis.title = ggplot2::element_text(size = title.size), 
                             plot.title = ggplot2::element_text(size = title.size, hjust=0.5)) 
            
            
            
            xy.out <- data.frame(x, y)
            rownames(xy.out) <- rownames(XY)
            colnames(xy.out) <- c("X rank", "Y rank")
            
            attr(rankplot, "data") <- xy.out
            if(significant == TRUE) {
              rankplot <- rankplot + p.points
            }
            #message(paste("plot options: significant =", significant))
            #message(paste("plot options: rescaled =", rescaled))
            
            if(interactivePlot == TRUE) {
              rankplot <- suppressMessages(plotly::ggplotly(rankplot))
            }
            #message(paste("plot options: interactivePlot =", interactivePlot))
            rankplot
          })

#-------------------------------------------------------------------#
#' @rdname rankplot-methods
#' @aliases eco.rankplot,numeric-method
#' @exportMethod eco.rankplot


#plot with a numeric variable for colours vs XY

setMethod("eco.rankplot", 
          c("numeric",
            "dataframeORmatrix", 
            "missing"),
          function(input, 
                   XY, 
                   xlabel,
                   ylabel,
                   title,
                   legendlabel,
                   background = c("grey", "white"),
                   interactivePlot = TRUE) {
            
            if(interactivePlot) {
              axis.size = 9
              title.size = 13
            } else {
              axis.size = 10
              title.size = 14
            }
            
            
            theme <- match.arg(background)
            if(theme == "grey") {
              themecol <-  ggplot2::theme_grey()
            } else {
              themecol <- ggplot2::theme_bw()
            }
            
            x <- rank(XY[, 1])
            y <- rank(XY[, 2])
            
            z <- input
            
            data <- data.frame(x, y, z)
            
            
            if(is.null(xlabel)) {
              xlabel <- "X rank"
            } 
            
            if(is.null(ylabel)) {
              ylabel <- "Y rank"
            } 
            
            if(is.null(title)) {
              title <- " "
            }
            
            if(is.null(legendlabel)) {
              legendlabel <- "Z"
            }
            
            rankplot <- ggplot2::ggplot(data, ggplot2::aes(x = x,y = y)) +
              ggplot2::geom_point(colour="grey50", size = 4.5)+
              ggplot2::geom_point(ggplot2::aes(colour = z), 
                                  size=3)+ 
              ggplot2::scale_color_gradient2(legendlabel, 
                                             high= scales::muted("red"),
                                             low = scales::muted("blue"))+
              themecol + 
              ggplot2::xlab(xlabel) +
              ggplot2::ylab(ylabel) +
              ggplot2::labs(title = title) +
              ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                             axis.title = ggplot2::element_text(size = title.size), 
                             plot.title = ggplot2::element_text(size = title.size, hjust=0.5)) 
            
            
            
            xy.out <- data.frame(x, y)
            rownames(xy.out) <- rownames(XY)
            colnames(xy.out) <- c("X rank", "Y rank")
            
            if(interactivePlot) {
              rankplot <- suppressMessages(plotly::ggplotly(rankplot))
            }
            
            attr(rankplot, "data") <- xy.out
            #message(paste("plot options: interactivePlot =", interactivePlot))
            rankplot
            
            
          })

#-------------------------------------------------------------------#
#' @rdname rankplot-methods
#' @aliases eco.rankplot,factor-method
#' @exportMethod eco.rankplot

# plot with a factor for colors vs XY

setMethod("eco.rankplot", 
          c("factor",
            "dataframeORmatrix", 
            "missing"),
          function(input, 
                   XY, 
                   xlabel,
                   ylabel,
                   title,
                   legendlabel,
                   background = c("grey", "white"),
                   interactivePlot = TRUE) {
            
            
            if(interactivePlot) {
              axis.size = 9
              title.size = 13
            } else {
              axis.size = 10
              title.size = 14
            }
            
            theme <- match.arg(background)
            if(theme == "grey") {
              themecol <-  ggplot2::theme_grey()
            } else {
              themecol <- ggplot2::theme_bw()
            }
            
            x <- rank(XY[, 1])
            y <- rank(XY[, 2])
            
            z <- input
            
            data <- data.frame(x, y, z)
            
            
            if(is.null(xlabel)) {
              xlabel <- "X rank"
            } 
            
            if(is.null(ylabel)) {
              ylabel <- "Y rank"
            } 
            
            if(is.null(title)) {
              title <- " "
            }
            
            if(is.null(legendlabel)) {
              legendlabel <- "Z"
            }
            
            rankplot <- ggplot2::ggplot(data, ggplot2::aes(x = x,y = y)) +
              ggplot2::geom_point(colour="grey50", size = 4.5)+
              ggplot2::geom_point(ggplot2::aes(colour = z), 
                                  size=3)+ 
              ggplot2::scale_color_discrete(legendlabel)+
              themecol +
              ggplot2::xlab(xlabel) +
              ggplot2::ylab(ylabel) +
              ggplot2::labs(title = title) +
              ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                             axis.title = ggplot2::element_text(size = title.size), 
                             plot.title = ggplot2::element_text(size = title.size, hjust=0.5)) 
            
            
            
            
            xy.out <- data.frame(x, y)
            rownames(xy.out) <- rownames(XY)
            colnames(xy.out) <- c("X rank", "Y rank")
           
             if(interactivePlot) {
              rankplot <- suppressMessages(plotly::ggplotly(rankplot))
             }
            
            attr(rankplot, "data") <- xy.out
            #message(paste("plot options: interactivePlot =", interactivePlot))
            rankplot
            
          })
