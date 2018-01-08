
#-------------------------------------------------------------------#
#' Plot for a connection network
#' @param x Connection network
#' @param group Vector with classes assigned to the individuals, in the same original order
#' @param type Plot type: "edgebundle", for a circular network, "network" for a tension network
#' @param fontSize Argument passed to \code{\link[networkD3]{forceNetwork}}
#' contained in the weight object  (which is the order of the table used to construct the weights)
#' @param vertex.size Parameter to \code{\link[igraph]{plot.igraph}}
#' @param vertex.label Parameter passed to \code{\link[igraph]{plot.igraph}}
#' @param bounded Logical. Value to enable (TRUE) or disable (FALSE) 
#'                the bounding box limiting the force network graph extent see \code{\link[networkD3]{forceNetwork}}.
#' @param ebColor Vector with edge colors for the groups of the edgebundler plot (Experimental feature) 
#' @param ... Additional arguments passed to \code{\link[igraph]{plot.igraph}}
#' @description Plot method for an eco.weight object. For examples, see  \code{\link{eco.weight}}  
#' This function can make a static plot with the original coordinates and an additional graph with
#' the coordinates transformed as ranks. It can also construct dynamic plots 
#' (force networks and circle networks).
#' @examples 
#' # see the examples in the function eco.weight:
#' # ?eco.weight
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

eco.plotWeight <-  function(x, type = c("simple", "igraph", "edgebundle", "network"),
                            group = NULL, fontSize = 10, ebColor = NULL, 
                            vertex.size = 10, vertex.label = NA,
                            bounded = FALSE,  ...) {
  
  
  # If connections are zero, return a message
  if(length(x@CONNECTED) == 0) {
    message("Zero connections in the object...nothing to plot\n")
    return(plot(1, type = "n", axes = F, xlab = "", ylab = ""))
  }
  
  #par settings-on exit reset
 
  
  
  type <- match.arg(type)
  
  
  #### STATIC PLOT ##############################################3
  simplePlot <- function(con) {
    # for inverse and exponential, all individuals are
    # connected. Return a rester with the connections.
    if(x@METHOD %in% c("inverse", "exponential")) {
      nonzero <- which(x@W != 0)
      plot(as.matrix(dist(x@XY))[nonzero], x@W[nonzero],
           xlab = "inter-individual distance", ylab = "Weight",
           pch = 19)
      return(invisible())
    }
    
    # non zero connections as data frame. The first columns
    # have the rows of each connected pair
    df.con <- with(x, {
      out <- aue.image2df(x@W)
      out <- out[which(out[, 3] != 0), ] #connected pairs
      out
    })
    
    # colour configuration
    
    # dos posibilidades mas de color, pero no son buenas
    # col.custom <- brewer.pal(8, "Accent")
    #  nrep <- ceiling(nrow(which.con) / length(colors))
    # col.custom <- rep(col.custom, nrep)
    
    #nind <- length(unique(which.con[,1]))
    #col.custom<-rgb(runif(nind),runif(nind),runif(nind)) 
    set.seed(10)
    col.custom <- sample(rainbow(length(unique(df.con[,2])), v = 0.8, s = 0.7, alpha = 0.7))
    col.custom <-  col.custom[df.con[, 1]]
    
    # which.con : individuals connected as (number of rows)
    # coord coordinates
    #--------------------------LINE.PLOT FUNCTION-------------------------
    line.plot <- function(coord, which.con, colour) {
      
      # coordinates of each pair (c2, c1) of connected individuals
      c1 <- coord[which.con[, 2], ]
      c2 <- coord[which.con[, 1], ]
      
      lapply(1:nrow(which.con), function(i) {
        lines(c(c1[i, 1], c2[i, 1]), c(c1[i, 2], c2[i, 2]), 
              col = colour[i], lwd = 3)
      })
    }
    #------------------------------------------------------------------
    par(mfrow = c(1,2))
    
    plot(x = range(x@XY[, 1]), y = range(x@XY[, 2]),
         main = "original coordinates", xlab = "X", ylab = "Y",
         type='n')
    line.plot(x@XY, df.con, col.custom)
    points(x@XY[,1], x@XY[, 2], cex = 0.8, pch = 21, bg = "beige")
    
    
    rcoord <- apply(x@XY, 2, rank)
    plot(x = range(rcoord[, 1]), y = range(rcoord[,2]),
         main = "rank of coordinates", xlab = "X rank", ylab = "Y rank", type='n')
    line.plot(rcoord, df.con, col.custom)
    points(rcoord[, 1], rcoord[, 2], cex = 0.8, pch = 21, bg = "beige")
    
    invisible(NULL)
    
  } 
  
  
  # IGRAPH PLOT #################################################
  
  igraphPlot <- function(con, vertex.size, vertex.labels, group, ...) {
    myGraph <- igraph::graph_from_adjacency_matrix(con@W, mode = "undirected", add.rownames = TRUE, weighted = TRUE)
    
    if(!is.null(group)) {
      group <- as.factor(group)
      col <- heat.colors(length(levels(group)))
      igraph::V(myGraph)$color <- col[group]
    }
    # create 3 x 8 layout to plot
    graphics::layout(matrix(rep(c(1,1,1,1,1,1,2,2), 3), 3,8, byrow=TRUE))
    igraph::plot.igraph(myGraph, vertex.size= vertex.size, vertex.label= vertex.label, layout= igraph::layout_with_fr(myGraph), ...)
    
    if(!is.null(group)) {
      # make blank plot
      plot(1, type="n", axes=F, xlab="", ylab="")
      legend("topleft", legend= levels(group), fill = levels(as.factor(col)))
    }
    invisible(NULL)
  }
  
  ##### DYNAMIC PLOT ############################################
  
  dynamicPlot <- function(con,  type, group, fontSize, ebColor) {
    
    if(type == "network") {

      tabla <- aue.image2df(con@W)
      
      #tabla <- aue.image2df(as.matrix(dist(con@XY))) 

      cuales <-which(tabla[,3] == 0)
      
      # rescale between [0, 1]
      tabla[, 3] <- aue.rescale(tabla[, 3])
     
      # this was to be used with  linkColour = mycols when plotting the network 
      # wg[, 3] <- 10* aue.rescale(wg[, 3])
      #colores <- heat.colors(11)
      #colores <- colores[10:1]
      #intervalos <- cut(wg[, 3], 0:10, include.lowest = T)
      #mycols <- colores[as.numeric(intervalos)][-cuales]
      #ancho <- wg[, 3][-cuales]
      #tabla[cuales, 3] <- 10
      
      # remove individuals without links
      tabla <- tabla[-cuales, ]
      tabla[, 1:2] <- tabla[, 1:2] - min(tabla[, 1:2])
      
      colnames(tabla)<-c("source", "target", "value")
      # 0 indexed for JavaScript
      nodos <- data.frame(rownames(con@W))
      
      if(is.null(group)) {
        group <- rep(1, nrow(nodos))
      }
      
      nodos <- data.frame(nodos, group)
      colnames(nodos)<-c("name", "group")
      out <- networkD3::forceNetwork(Links = tabla, Nodes = nodos, Source = "source",
                                     Target = "target", Value = "value", 
                                     NodeID = "name", opacity = 0.9, Group = "group",
                                     zoom = TRUE, legend = TRUE, bounded = bounded, 
                                     fontSize = fontSize) 
     
    } else  {
      #circle
      # the program undersand dots as classes
      rownames(con@W) <- gsub("[.]", "-", rownames(con@W))
      
      nombres <- rownames(con@W)
      
      if(is.null(group)) {
        group <- rep(1, length(nombres))
      } else {
        #remove dots from group names
        group <- gsub("[.]", "-", group)
      }
      
      nodes <- data.frame(name = nombres, class = group)
      levels(nodes[, 1]) <- nombres
      myFactor <- paste0(group, ".", nombres)
      myFactor <- factor(myFactor, levels=myFactor)
      relations <- aue.image2df(con@W)
      relations <- relations[relations[, 3] != 0, ]
      relations <- relations[, 1:2]
      relations <- data.frame(from = myFactor[relations[, 1]], to = myFactor[relations[, 2]])
      #levels(relations[,1]) <- levels(myFactor)
      #levels(relations[,2]) <- levels(myFactor)
      #myCol <- clr[as.numeric(myCol)]
      g <- igraph::graph.data.frame(relations, directed = FALSE, vertices = myFactor)
      
      # set color
      # there is a problem to render colors in hex, a partial solution below
      
      if(is.null(ebColor)){
        
        out <- edgebundleR::edgebundle(g, tension = 0.4, fontsize = fontSize)
        
      } else {
        
        myCol <- myFactor[relations[, 1]]
        myCol <- gsub("[.].*", "", myCol)
        clr <- as.factor(myCol)
        #clr <- as.factor(group)
        levels(clr) <- ebColor
        #V(g)$color <- as.character(clr)
        igraph::E(g)$color <- as.character(clr)
        out <- edgebundleR::edgebundle(g,tension = 0.4,fontsize = fontSize)
        out$x$edges <- jsonlite::toJSON(igraph::get.data.frame(g,what="edges"))
        
        #temporal patch for the colour issue
        out <- htmlwidgets::onRender(
          out,
          'function(el,x){
          // loop through each of our edges supplied
          //  and change the color
          x.edges.map(function(edge){
          var source = edge.from.split(".")[1];
          var target = edge.to.split(".")[1];
          d3.select(el).select(".link.source-" + source + ".target-" + target)
          .style("stroke",edge.color);
          })
      }'
                )
    }
    }
    out
  }
  
  
  #message(paste("plot options: type (simple/igraph/network/edgebundle) =", type))
  #message(paste("plot options: fontSize =", fontSize))
  if(is.null(group)) {
    group <- "NULL"
  }
  #message(paste("plot options: group =", deparse(substitute(group))))
  
  if(type == "simple") {
    return(simplePlot(con = x))
  } else if(type == "igraph") {
    if(x@METHOD %in% c("inverse", "exponential")) {
      return(message("This method is not available for weights of method = 'inverse' or method = 'exponential'\n"))
    }
    return(igraphPlot(con = x, vertex.size = vertex.size, vertex.label = vertex.label, group = group, ...))
  } else {
    if(x@METHOD %in% c("inverse", "exponential")) {
      return(message("This method is not available for weights of method = 'inverse' or method = 'exponential'\n"))
    }
    return(dynamicPlot(con = x, type = type, group = group, fontSize = fontSize, ebColor = ebColor))
  }
}


