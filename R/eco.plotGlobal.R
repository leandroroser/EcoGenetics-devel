
#' GSA plot methods
#' @param input  eco.gsa object
#' @param interactivePlot Show an interactive plot via plotly? (default: TRUE)
#' @param background background color ("grey" or "white")
#' @param xlabel Label for X axis (default: NULL)
#' @param ylabel Label for Y axis (default: NULL)
#' @param title Title of the plot (default: NULL)
#' @param legend Show legends in ggplot graphs? (default: TRUE)
#' @param rescaled rescale join-count heatmap?
#' @param alpha significance level for the join-count heatmat 
#' @description This function allows to plot results contained in eco.gsa objects.
#' For examples, see \code{\link{eco.gsa}} 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

eco.plotGlobal <- function(input, interactivePlot = FALSE, 
                           background = c("grey", "white"),
                           xlabel =NULL, ylabel = NULL, title = NULL,
                           legend = TRUE, rescaled = FALSE, alpha = 0.05) {

    # solve global binding warnings
  # if(getRversion() >= "2.15.1") utils::globalVariables(c("obs", "obs2", "ymin"))
  obs <- obs2 <- ymin <- NULL 
  
  if(!all(dim(input@MULTI)) != 0 || length(input@MULTI) == 0) {
    return(message("nothing to plot..."))
  }
  
  
  theme <- match.arg(background)
  
  if(theme == "grey") {
    themecol <-  ggplot2::theme_grey()
  } else {
    themecol <- ggplot2::theme_bw()
  }
  
  if(is.null(xlabel)) {
    xlabel <- ""
  } 

  
  if(is.null(title)) {
    title <- ""
  }
  
  
  # legend
  if(legend){
    leyenda <- ggplot2::theme(legend.position = "right")
  } else {
    leyenda <- ggplot2::theme(legend.position = "none")
  }
  
  if(is.null(ylabel)) {
    ylabel <- input@METHOD
  }
  
  # thiis controls the size of elements in interactive and static plot
  if(interactivePlot) {
    p.size <- 1
    axis.size = 12
    title.size = 8
  } else {
    p.size <- 1
    axis.size = 14
    title.size = 8
  }
  
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  method <- input@METHOD
  
  cond1 <- method %in% c("Moran' I", "Geary's C", 
                         "Bivariate Moran's Ixy")
  
  # join count for 1 var
  cond2 <- method == "Join-count" && colnames(input@MULTI)[1] != "var"
  
  if(cond1 || cond2) {
    
    # this was a simple plot
    # x <- input@MULTI
    # graphics::layout(matrix(rep(c(1, 1, 2,2,2,2, 2), 7), 7,7, byrow=TRUE))
    # mycol <- x$pval < 0.05
    # mycol <- mycol + 1
    # mycol <- c("blue", "red")[mycol]
    # plot(1, type="n", axes=FALSE, xlab="", ylab="")
    # legend("topright", legend = c(paste0("P < ", alpha), "NS"), fill = c("red", "blue"), cex = 1.2)
    # out <- barplot(x$obs, col = mycol, names.arg = rownames(x), ylab = method,
    #              xlab = "Var", cex.axis = 1.5, cex.names = 1.5, cex.lab = 1.5)
    
    
    mydat <- input@MULTI
    mydat <- data.frame(rownames(mydat), mydat, stringsAsFactors = FALSE)
    colnames(mydat)[1] <- "var"
    mydat$var <- factor(mydat$var, levels =  as.character(mydat$var))

    
    mydat$ymin <- rep(0, nrow(mydat))
    
    #hack to ggplotly tooltip
    mydat$obs2 <- mydat$obs
    
    mycol<- mydat$p.val < alpha
    mycol <- mycol + 1
    mycol <- c("#F8766D", "#00B0F6")[mycol]
    #this trick is used to change the order of the labels in ggplot2
    mycol <- as.factor(mycol)
    if(length(levels(mycol)) == 2) {
    my_labs <- c(paste0("P < ", alpha), "NS")
    # solution to color inversion in ggplot2: scale manually colors
    #    scale_col <- ggplot2::scale_color_manual(values =  c("#F8766D", "#00B0F6"))
    } else {
      if(levels(mycol) == "#F8766D") {
        my_labs <- paste0("P < ", alpha)
    #scale_col <- ggplot2::scale_color_manual(values =  "#F8766D")
        
      } else {
        my_labs <- "NS" 
     #   scale_col <- ggplot2::scale_color_manual(values =  "#00B0F6")
      }
    }
      
    #levels(mycol) <- c("#F8766D", "#00B0F6")
    
    
    # bars have a bug in ggplotly, negative values are inverted
    #geom_bar(stat = "identity", aes(fill = mycol)) +

    out <- ggplot2::ggplot(mydat, ggplot2::aes(x= var, y = obs)) + 
      ggplot2::geom_pointrange(fatten = 1, size = p.size, ggplot2::aes(ymin = ymin, ymax = obs2, color = mycol))+
      ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
            axis.title = ggplot2::element_text(size = title.size), 
            legend.position = "right")+
      ggplot2::scale_color_discrete(name  ="P value", labels= my_labs) +
      ggplot2::ylab(ylabel) + 
      ggplot2::xlab("") +
      ggplot2::labs(title = title) +
      themecol+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))
      # +scale_col
  
    
    if(interactivePlot) {
      
      out <- suppressMessages(plotly::ggplotly(out + ggplot2::theme(plot.margin = ggplot2::unit(c(0.6, 0.6, 0.7, 0.6), "cm")), tooltip = c("var", "obs"))) 
      
      for(i in 1:length(out$x$data)) {
        if(out$x$data[[i]]$name =="#00B0F6") {
          out$x$data[[i]]$name <- paste0("P < ", alpha)
        } else if(out$x$data[[i]]$name =="#F8766D") {
          out$x$data[[i]]$name <- "NS"
        }
      }
    }
    
  } else {
    
    coordenadas <- input@MULTI[,1:3]
    #coordenadas <- coordenadas[order(coordenadas[, 1], coordenadas[,2]), ]
    coordenadas[, 1] <- as.factor(coordenadas[,1])
    coordenadas[, 2] <- as.factor(coordenadas[,2])
    rnom <- levels(coordenadas[, 1])
    cnom <- levels(coordenadas[, 2])
    coordenadas[, 1] <- as.numeric(coordenadas[, 1])
    coordenadas[, 2] <- as.numeric(coordenadas[, 2])
    
    pcoord <- coordenadas
    pcoord[, 3] <- input@MULTI[, 6]
    
    #create a grid to plot for observations and pvalues
    grilla <- expand.grid(1:max(coordenadas[,1]), 1:max(coordenadas[,2]))
    grilla[, 3] <- rep(0, nrow(grilla))
    # put the values of coordenadas in the grid
    grilla2 <- grilla
    
    for(i in 1:nrow(coordenadas)) {
    cual <- which(coordenadas[i, 1] == grilla[, 1] & coordenadas[i, 2] == grilla[, 2])
    #store observations
    grilla[cual, 3] <- coordenadas[i, 3]
    #store pvalues
    grilla2[cual, 3] <- pcoord[i, 3]
    }
    
    # conversion to raster
    matrixplot <- aue.df2image(grilla[, c(2,1,3)])
    matrixplot <- t(matrixplot)
    rownames(matrixplot) <- cnom
    colnames(matrixplot) <- rnom
    
    # conversion to raster
    pvals <- aue.df2image(grilla2[, c(2,1,3)])
    pvals <- t(pvals)
    pvals <- pvals < alpha
    mode(pvals) <- "integer"

    
    if(rescaled) {
      matrixplot <- aue.rescale(matrixplot)
    }
    
    
    if(!any(pvals)) {
      message("no significant results to show...\n")
      return(plot(1, type="n", axes=F, xlab="", ylab=""))
      
    }
    
    
    
    if(!interactivePlot) {
      matrixplot <- matrixplot*pvals
      out <- pheatmap::pheatmap(matrixplot, display_numbers = matrix(ifelse(pvals == 1, "*", ""), nrow(pvals)), fontsize_number = 18)
      
    } else {
      stop("Sorry, the use of d3heatmap has been deprecated")
      matrixplot[pvals == 0] <- -1
      out <- d3heatmap::d3heatmap(matrixplot)
    }
  }
  
  out
}

