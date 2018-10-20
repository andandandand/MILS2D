## runApp server.r before this to get bdm2D.r to work

source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

require(igraph)

formatEdgesForDeletion <- function(edgeLoss){
  formattedEdges <- paste0(edgeLoss$from,"|", edgeLoss$to)
  return (formattedEdges)
}

g <- make_star(5)  %>% set_vertex_attr("name", value = 1:5)
plot(g)

edgeLoss <- correctLossRanking(calculateLossByEdgeDeletion(g, 
                                                           blockSize = 4, 
                                                           offset = 1))

formattedEdges <- formatEdgesForDeletion(edgeLoss)

print(formattedEdges)

print(formattedEdges[1])

g2 <- delete_edges(g, formattedEdges[1:4])
plot(g2)