require("igraph")
source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/compressionLength.R")
source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")
source("scripts/listEdges.R")
#source("scripts/matrixPlot.R")


require("igraph")
require("ggplot2")
require("reshape2")

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

shinyServer(function(input, output, session) {
  
  ## MILS 2D tab
  # the names/indices of vertices for the graph are set at loadGraph
  g <- loadGraph("./data/starGraphAdjMatrix.csv")
  
  originalG <- g
  
  reducedG <- g
  
  lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, 
                                                                   blockSize=4, 
                                                                   offset = 1))
  
  lossEdges <- correctLossRanking(calculateLossByEdgeDeletion(g, 
                                                              blockSize = 4, 
                                                              offset = 1))
  
  deletionsCounter <- as.integer(1)

  reactiveData <- reactiveValues(g = g,
                                 reducedG = reducedG, 
                                 originalG = originalG,
                                 lossVertices = lossVertices,
                                 lossEdges = lossEdges,
                                 deletionsCounter = deletionsCounter)
  
  observeEvent(input$swapGraphsButton, {
    
    reactiveData$g <- reactiveData$reducedG
    
  })
  
  observeEvent(input$resetGraphsButton,{
    
    reactiveData$g <- reactiveData$originalG
    reactiveData$reducedG <- reactiveData$originalG
    
  })
  
  
  observeEvent(input$file1, {
    
    inFile <- input$file1
    
    if (is.null(inFile$datapath)){
      
      
    } else {
      
      reactiveData$lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, 4, 1))
      reactiveData$lossEdges    <- correctLossRanking(calculateLossByEdgeDeletion(g, 4, 1))
      reactiveData$g            <- loadGraph(inFile$datapath)
      reactiveData$reducedG     <- reactiveData$g
      reactiveData$originalG    <- reactiveData$g
      
      reactiveData$deletionsCounter <- as.integer(1)
      
    }
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$numberOfElements, {

    elems <- 0
    vertices <- c()
    edges    <- c()
    
    if(input$numberOfElements!=0){
    
      if(input$elementsToDelete == "vertices"){ 
          
         elems <- vcount(reactiveData$g)
         
         verticesToDelete <- reactiveData$lossVertices$name
         
         reactiveData$reducedG <- delete_vertices(reactiveData$g, 
                                                  verticesToDelete[1:input$numberOfElements])
      } 
      
      if(input$elementsToDelete == "edges") { 
      
          elems <- ecount(reactiveData$g)
          
          edgesToDelete <- formatEdgesForDeletion(reactiveData$lossEdges)
          
          reactiveData$reducedG <- delete_edges(reactiveData$g, 
                                                edgesToDelete[1:input$numberOfElements])
         
      }
    }
    
    updateSliderInput(session,
                      "numberOfElements",
                      max = elems-1)
  }, ignoreNULL = FALSE)
  
  
  
  output$graphPlot <- renderPlot({
    
    if(input$showAdjacencyMatrix==TRUE){
      
      #plotAdjMatrix(reactiveData$g) # why does this fail?
      #plotAdjMatrix(make_star(5)) # why does this work? 
      #cheat <- reactiveData$g # fails
      plotAdjMatrix(make_star(8))
      print("test")
    
    }
    
    else{
    coords <- layout_(reactiveData$g, as_star())
    
    plot(reactiveData$g,
         layout = coords,
         edge.arrow.size = 0.5,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    }
  }) 

  output$reducedGraphPlot <- renderPlot({
    
    coords <- layout_(reactiveData$reducedG, as_star())
    
    plot(reactiveData$reducedG,
         layout = coords,
         edge.arrow.size = 0.5,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    
  }) 
  
})
