########################
# q1
########################

library(manipulate)
myPlot <- function(s) {
  plot(cars$dist - mean(cars$dist), cars$speed - mean(cars$speed))
  abline(0, s)
}

# This function plots distance versus speed, each de-meaned and an associated line of slope s. 
# Which of the following code will make a manipulate plot that creates a slider for the slope?
manipulate(myPlot(s),s=slider(0,2,step=0.1))

###########################
# Q2

# Which of the following code uses the rCharts package to create a sortable and searchable data table 
# for the airquality data set? Assume the rCharts package and the airquality data set have already been
# loaded into R.
#install.packages('devtools')
#require(devtools)
#install_github('ramnathv/rCharts')
library(rCharts)
data(airquality)
dTable(airquality, sPaginationType="full_numbers")

###
# Q4

library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Data science FTW!"),
  sidebarPanel(
    h2('Big text'), # MISSING COMMA HERE !!!
    h3('Sidebar')
  ),
  mainPanel(
    h3('Main Panel text')
  )
))


# Q5

# ui.R
shinyUI(pageWithSidebar(
  headerPanel("Example plot"),
  sidebarPanel(
    sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)
  ),
  mainPanel(
    plotOutput('newHist')
  )
))

# server.R
library(UsingR)
data(galton)

shinyServer(
  function(input, output) {
    output$myHist <- renderPlot({
      hist(galton$child, xlab='child height', col='lightblue',main='Histogram')
      mu <- input$mu
      lines(c(mu, mu), c(0, 200),col="red",lwd=5)
      mse <- mean((galton$child - mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
    })
  }
)

# The server.R output name isn't the same as the plotOutput command used in ui.R.
# 'myHist' vs. 'newHist'

# 1,3,5,7,9,11
# 12,13