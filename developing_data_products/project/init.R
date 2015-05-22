# init file
install.packages('shiny')
library(shiny)
setwd('~/ocw/data_science/developing_data_products/project')
runApp()

# deploy
library(shinyapps)
shinyapps::deployApp()
