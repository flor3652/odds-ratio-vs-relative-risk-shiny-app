library(shiny)
library(plotly)


shinyUI(pageWithSidebar(
  
  headerPanel("Relative Risk for a Given Odds Ratio"),
  
  #
  sidebarPanel(
    h3("Odds Ratio"),
    
    selectInput("orInpFormat", 
                "Slider or Numeric Input Format?",
                list("Slider"="slide",
                     "Numeric"="num")),
    
    #### Setting unique inputs for the different kinds of input
    #normal
    conditionalPanel(
      condition = "input.orInpFormat == 'slide'",
      sliderInput("slideOddsRatio", "Odds Ratio:", 1,10,2, step=.25)
    ),
    
    #exponential
    conditionalPanel(
      condition = "input.orInpFormat == 'num'",
      numericInput("numOddsRatio", "Odds Ratio:",value = 2,min = 1)
    ),
    
    h3("Graph"),
    
    checkboxInput("p2onx", "P2 (comparison group) on the X Axis? If unchecked, P1 will be on the X axis.", value=FALSE), #add to change the possible x-axis values.
    
    checkboxInput("setylim","Set the Y Limits?",value = FALSE),    
    
    #Giving users the option to select their population size (which shouldn't matter, so long as it is somewhat large)
    
    conditionalPanel(
      condition = "input.setylim == true",
      numericInput("uppery","Upper Y Limit",10),
      numericInput("lowery","Lower Y Limit",1)
    )
    
  ),
  #
  mainPanel(
  
    #Do the population output as well as the CLT output, maybe with a run button on the side panel to keep it from doing so many calculations all at the same time?
    
    tabsetPanel(
      tabPanel("Graph", plotOutput("rrPlot")),
      tabPanel("Table", dataTableOutput("rrTable")),
      tabPanel("3D Graph", plotlyOutput("threeDPlot"), width="100%")
    )
    
    
    
  )
))