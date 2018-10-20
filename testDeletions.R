source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

# order by populationage
# population[order(population$age),]

#g is an igraph graph, not an adjacency matrix
g <- loadGraph("../data/starGraphAdjMatrix.csv")
plot(g)
pv <- calculateLossByVertexDeletion(g, blockSize=4, offset = 1)
print(pv)
pe <- calculateLossByEdgeDeletion(g, blockSize = 4, offset = 1)
print(pe)

pv <- pv[order(pv$LossRank),]
print(pv)

pe <- pe[order(pe$perturbationsRank),]
print(pe)

print(pe[1:3, ]$from)
print(pe[1:3,]$to)

delEdgesG <- delete_edges(g, paste0(pe[1:3, ]$from, sep="|", pe[1:3,]$to))

plot(delEdgesG)

delVerticesG <- delete_vertices(g, pv[1:3,]$"name")

plot(delVerticesG)


getGraphWithDeletedEdges <- function (g, pe, numberOfEdges){

  pe <- pe[order(pe$perturbationsRank),]
  
  listOfEdges <- paste0(pe[1:numberOfEdges, ]$from, sep="|", pe[1:numberOfEdges,]$to)
  
  delEdgesG <- delete_edges(g, listOfEdges)
  
  return(delEdgesG)
}

getGraphWithDeletedVertices <- function(g, pv, numberOfVertices){
  
  pv <- pv[order(pv$perturbationsRank),]
}

testG <- getGraphWithDeletedEdges(g, pe, 1)

plot(testG)

