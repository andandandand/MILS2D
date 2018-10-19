require("igraph")

source("scripts/BDM1D.R")
source("scripts/BDM2D.R")
source("scripts/compressionLength.R")
source("scripts/loadGraph.R")
source("scripts/edgeAndVertexKnockout.R")

source("scripts/listEdges.R")


shinyServer(function(input, output, session) {
  
  ## MILS 2D tab
  
  #TODO: FIX issue of graph not updating
  g <- loadGraph("./data/starGraphAdjMatrix.csv")
  
  pv <- calculateLossByVertexDeletion(g, blockSize=4, offset = 1)
  
  pe <- calculateLossByEdgeDeletion(g, blockSize = 4, offset = 1)
  
  deletionsCounter <- as.integer(1)

  #TODO: solve issue with deletionsCounter when element changes from edge to vertex or viceversa  
  reactiveData <- reactiveValues(g = g,
                                 pv = pv,
                                 pe = pe,
                                 deletionsCounter = deletionsCounter)
  
  
  observeEvent(input$file1, {
    
    inFile <- input$file1
    
    if (is.null(inFile$datapath)){
      
      
    } else {
      
      reactiveData$pv <- calculateLossByVertexDeletion(g, 4, 1)
      reactiveData$pe <- calculateLossByEdgeDeletion(g, 4, 1)
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
     
     #TODO: find out why this doesn't update when changing vertices for edges
     print(elems)  
  })
  
  ## deconvolve's solution  
  # observeEvent(input$n_components, {
  #   
  #   updateSliderInput(session,
  #                     "n_components",
  #                     max = vcount(react_graph$g))
  # })
  
  output$graphPlot <- renderPlot({
    
    coords <- layout_(reactiveData$g, as_star())
    
    plot(reactiveData$g,
         layout = coords,
         edge.arrow.size = 0.4,
         vertex.size = 25,
         vertex.label.family = "Arial Black")
    
  }) 

})
