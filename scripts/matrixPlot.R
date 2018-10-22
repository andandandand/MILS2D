require(igraph)
require(ggplot2)
require(reshape2)

matrixPlot <- function(graphToPlot){
  
  gAdjMatrix <- as.matrix(as_adj(g))
  
  mm <- (gAdjMatrix == 1)
  
  mm %>% 
    melt() %>% 
    ggplot(aes(Var2, Var1)) + 
    geom_tile(aes(fill = value, 
                  color = value)) + 
    coord_equal() + 
    scale_fill_manual(values = c("black", "white")) + 
    scale_color_manual(values = c("white", "black")) + 
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) + 
    guides(fill = FALSE, color = FALSE) + 
    scale_x_discrete(expand = c(0,0)) + 
    scale_y_discrete(expand = c(0,0))
  
    print(mm)
}

#test

matrixPlot(make_star(5))