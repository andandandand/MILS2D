source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

require(igraph)

g <- make_star(5)  %>% set_vertex_attr("name", value = 1:5)
plot(g)

vertexLoss <- correctLossRanking(calculateLossByVertexDeletion(g, blockSize = 4, offset = 1))
print(vertexLoss)

g2 <- delete_vertices(g, rownames(vertexLoss)[1])
plot(g2)