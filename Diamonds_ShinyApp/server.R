#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
library(shiny)
library(ggplot2)
library(ggdark)


function(input, output) {
  
  # Reload the data based on Sample Size selected by user
  dataset <- reactive({
    diamonds[sample(nrow(diamonds), input$sampleSize),]
  })
  
  # Plot based on user inputs
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x = input$x, y = input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color = input$color)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    p <- p + ggtitle(input$caption)+ dark_theme_linedraw() 
    
    print(p)
    
    output$downloadImage <- downloadHandler(
      filename = function(){paste("download_plot", '.png', sep='')},
      content = function(file){ggsave(file,plot = p)}
    )
    
  }, height=800)
  
}