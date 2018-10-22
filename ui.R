library(shiny)
library("shinythemes")

orange_slider <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
background: #f63;
border-color: #f63;
}"

shinyUI(
  fluidPage(
    theme = shinytheme("united"),
    tags$style(orange_slider),
    
    sidebarLayout(
      
      column(6, 
             tabsetPanel(
               tabPanel("For networks",
                        value = 1,
                        
                        h3("MILS for Networks"),
                        
                        div(
                          wellPanel(
                            
                            fileInput(inputId = "file1",
                                      label = "Choose a CSV file",
                                      accept = c('text/comma-separated-values',
                                                 'text/plain',
                                                 'text/csv',
                                                 '.csv')
                            ),
                            
                            checkboxInput(inputId = "showAdjacencyMatrix", 
                                          label = "Show adjacency matrix", 
                                          value = FALSE, width = NULL),
                            
                            hr(),
                            radioButtons(inputId="elementsToDelete", 
                                         label = "Elements to delete",
                                         choices = c("vertices", "edges"),
                                         selected = "vertices"),
                            
                            # updated on server
                            sliderInput(inputId="numberOfElements",
                                        label = "Number of elements to delete",
                                        min = 1, 
                                        max = 10, value = 1, step = 1
                                        ),
                            
                            hr(),
                            actionButton(inputId="swapGraphsButton",
                                         label  ="Update evaluated graph",
                                         width = "45%"),
                            hr(),
                            actionButton(inputId="resetGraphsButton",
                                         label = "Reset evaluated graph",
                                         width = "45%")
                          )
                        )
                      ), #end "For networks" tabpanel
               
               tabPanel("For strings",
                        value=2,
                        
                        h3("MILS for Strings")
                        
                        ),#end "For strings" tabpanel
               id = "conditionedPanels"
             )
      ),
      
      mainPanel(
        #withMathJax(),
        conditionalPanel(condition="input.conditionedPanels==1",
                         
                         br(),
                         fluidRow(
                          column(width = 5, 
                                 h3("Original Graph", align="center"),
                                 plotOutput("graphPlot")),
                          column(width = 5, 
                                 h3("Reduced Graph", align="center"),
                                 plotOutput("reducedGraphPlot"))
                         )
                        ), # end conditionalPanel
        
        #For strings
        conditionalPanel(condition="input.conditionedPanels==2",
                         
                         br(),
                         
                         fluidRow(
                           
                         )
        
      )  
    )
   )
  )
)#end shinyUI

  