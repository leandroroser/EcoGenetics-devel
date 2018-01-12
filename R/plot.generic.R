
################################################
# GENERIC PLOT FUNCTIONS
################################################


#' Multiple plot function for ggplot
#' @param ... ggplot objects
#' @param plotlist List of ggplot object
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#'@export

setGeneric("grf.multiplot", function(..., plotlist = NULL, 
                                     cols = 1, layout = NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots <- length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      capture.output(print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                                           layout.pos.col = matchidx$col)))
    }
  }
})



#' Plot a ggplot sequence in layers of n plots arranged in k rows
#' @param x list of ggplot objects
#' @param n number of plot in layout
#' @param nrow Number of rows in layout
#' @param byrow plot by row?
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

setGeneric("grf.seqmultiplot", 
           function(x, n, nrow, byrow = TRUE) {
  
  j <- 1
  while(j < length(x)) {
    
    u <- j + n - 1
    if(u > length(x)) break
    
    ly <- matrix(1:n, nrow = nrow)
    grf.multiplot(plotlist = x[j:u], 
                  layout = ly)
    
    ifelse(j == 1, j <- j + n - 1, j <- j + n)
  }
  if(j < length(x)) {
    rest <- j + 1:length(x)
    grf.multiplot(plotlist = x[rest], 
                  layout = matrix(1:n,nrow = nrow))
  }
})

