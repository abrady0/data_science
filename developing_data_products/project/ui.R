library(shiny)
library(ggplot2)

data(state)
dataset <- as.data.frame(Seatbelts)

fluidPage(
  
  titlePanel("Speed and Stopping Distance of Cars Explorer"),
  
  sidebarPanel(
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=, round=0),
    
    selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
    selectInput('color', 'Color', c('None', names(dataset)))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)