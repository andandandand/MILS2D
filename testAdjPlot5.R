require(igraph)
require(ggplot2)
require(reshape2)

require("igraph")


loadGraph <- function(dataPath)
{
  
  loadedDF <- read.csv(dataPath,
                       header=FALSE,
                       sep=',', #separate by comma
                       quote="'", # quote by '
                       stringsAsFactors = FALSE,
                       check.names = FALSE)
  
  #selects numeric values, drops the rest
  loadedDF <- loadedDF[sapply(loadedDF, is.numeric)]
  
  rownames(loadedDF) <- colnames(loadedDF)
  # change here https://stackoverflow.com/a/53037158/45963
  loadedMat <- unname(as.matrix(loadedDF)) 
  
  #we use the rownames to index deletions
   g <- graph_from_adjacency_matrix(loadedMat) %>%
     set_vertex_attr("label", value = 1:nrow(loadedDF))
  
  g <- graph_from_adjacency_matrix(loadedMat) 
  return(g)
}

unnameGraph <- function(graphToPlot){
  
  gAdjMatrix <- unname(as.matrix(graphToPlot))
  
  return(gAdjMatrix)
}

plotAdjMatrix <- function(graphToPlot){
  
  gAdjMatrix <- as.matrix(as_adj(graphToPlot))
  
  logMatrix <- (gAdjMatrix == 1)
  
  matData <- melt(logMatrix)
  
  g <- ggplot(data = matData,
              aes(Var2, Var1)) + 
    geom_tile(aes(fill = value, 
                  color = value)) + 
    coord_equal() + 
    scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white")) + 
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black")) + 
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    guides(fill = FALSE, color = FALSE) +
    scale_y_reverse()
  
  print(g)
}

## Test #1 
g1 <- make_star(5)

# this works
plotAdjMatrix(g1)

## Test #2
g2 <- loadGraph("./data/starGraphAdjMatrix.csv")

plotAdjMatrix(g2)
