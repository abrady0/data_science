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
    selectInput('color', 'Color', c('None', names(dataset)), selected='law'),
    wellPanel(
      p('Seatbelts is a multiple time series, with columns'),
      p('DriversKilled: car drivers killed.'),
      p('drivers: killed or injured'),
      p('front: front-seat passengers killed or seriously injured.'),
      p('rear','rear-seat passengers killed or seriously injured.'),
      p('kms: kilometers driven.'),
      p('PetrolPrice: petrol price.'),
      p('VanKilled:  number of van (‘light goods vehicle’) drivers.'),
      p('law: 0/1: was the law in effect that month?')      
    )
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)