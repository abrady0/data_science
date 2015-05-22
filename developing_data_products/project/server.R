library(shiny)
library(ggplot2)


function(input, output) {
  data(Seatbelts)
  srcData <- as.data.frame(Seatbelts)
  
  dataset <- reactive({
    srcData[sample(nrow(srcData), input$sampleSize),]
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    #facets <- paste(input$facet_row, '~', '"'+input$facet_col)
    #if (facets != '. ~ .')
    #  p <- p + facet_grid(facets)
    
    print(p)
    
  }, height=700)
  
}
