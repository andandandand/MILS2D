require("igraph")

source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/compressionLength.R")
source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

source("scripts/listEdges.R")


shinyServer(function(input, output, session) {
  
  ## MILS 2D tab
  # the attribute name is set at loadGraph
  g <- loadGraph("./data/starGraphAdjMatrix.csv")
  
  lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, blockSize=4, offset = 1))
  
  lossEdges <- correctLossRanking(calculateLossByEdgeDeletion(g, blockSize = 4, offset = 1))
  
  deletionsCounter <- as.integer(1)

  reactiveData <- reactiveValues(g = g,
                                 lossVertices = lossVertices,
                                 lossEdges = lossEdges,
                                 deletionsCounter = deletionsCounter)
  
  
  observeEvent(input$file1, {
    
    inFile <- input$file1
    
    if (is.null(inFile$datapath)){
      
      
    } else {
      
      reactiveData$lossVertices <- correctLossRanking(calculateLossByVertexDeletion(g, 4, 1))
      reactiveData$lossEdges <- correctLossRanking(calculateLossByEdgeDeletion(g, 4, 1))
      reactiveData$g  <- loadGraph(inFile$datapath)
      
      reactiveData$deletionsCounter <- as.integer(1)
      
    }
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$numberOfElements, {

    elems <- 0
    vertices <- c()
    edges    <- c()
    
    if(input$elementsToDelete == "vertices"){ 
        
       elems <- vcount(reactiveData$g)
      
    } 
    
    if(input$elementsToDelete == "edges") { 
    
        elems <- ecount(reactiveData$g)
    
    }
    
    updateSliderInput(session,
                      "numberOfElements",
                      max = elems)
  })
  
  
  output$graphPlot <- renderPlot({
    
    coords <- layout_(reactiveData$g, as_star())
    
    plot(reactiveData$g,
         layout = coords,
         edge.arrow.size = 0.4,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    
  }) 

})
