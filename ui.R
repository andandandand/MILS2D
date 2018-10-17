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
                            
                            radioButtons(inputId="elementsToDelete", 
                                         label = "Elements to delete",
                                         choices = c("vertices", "edges"),
                                         selected = "vertices"),
                            
                            #TODO: update max dynamically on server
                            sliderInput(inputId="numberOfElements",
                                        label = "number of elements",
                                        min = 1, 
                                        max = 10, value = 1, step = 1
                                        ),
                            
                            #TODO: handle server logic
                            actionButton(inputId="swapGraphsButton",
                                         label  ="Update evaluated graph")
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
                                 #TODO: change to reducedGraphPlot
                                 plotOutput(""))
                         )
                        ), # end conditionalPanel
        conditionalPanel(condition="input.conditionedPanels==2",
                         
                         br(),
                         
                         fluidRow(
                           
            
                         
                         
                         )
        
      )  
    )
   )
  )
)#end shinyUI

  