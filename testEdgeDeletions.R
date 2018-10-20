source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

require(igraph)

g <- make_star(5)  %>% set_vertex_attr("name", value = 1:5)
plot(g)

edgeLoss <- correctLossRanking(calculateLossByEdgeDeletion(g, blockSize = 4, offset = 1))
print(edgeLoss)

rankedEdges <- paste0(edgeLoss$from,"|", edgeLoss$to)
rankedEdges[1]

g2 <- delete_edges(g, rankedEdges[1:4])
plot(g2)