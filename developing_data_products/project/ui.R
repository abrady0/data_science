library(shiny)
library(ggplot2)

data(state)
dataset <- as.data.frame(Seatbelts)

fluidPage(
  
  titlePanel("Driver Safety in UK And The Effect of Seatbelt Laws"),
  
  sidebarPanel(
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=, round=0),
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset)), selected='law')
    ))    
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)