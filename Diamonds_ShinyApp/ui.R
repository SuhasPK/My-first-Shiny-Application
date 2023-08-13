#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)

dataset <- diamonds



pageWithSidebar(
  
  # Header panel with application name.
  headerPanel("Diamond Data Analysis"),
  sidebarPanel(
    # Explaining the data attributes.
    
    h4('The variables are as follows:'),
    h6('1. price : diamond price in USD'),
    h6('2.    carat   : diamond weight'),
    h6('3.    cut     : quality of the cut'),
    h6('4.    color   : diamond color (J-Worst to D-Best)'),
    h6('5.    clarity : how clear diamond is'),
    h6('6.    x       : length in mm'),
    h6('7.    y       : width in mm'),
    h6('8.    z       : depth in mm'),
    h6('9.    depth   : depth percentage'),
    h6('10.    table   : width of top of diamond'),
    br(),
    
    # Some form of user input for plotting
    h4(' Select different parameters:'),
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset), value=min(5000, nrow(dataset)), step=100, round=0),
    selectInput('x', 'X Axis Measure', names(dataset)),
    selectInput('y', 'Y Axis Measure', names(dataset), names(dataset)[[7]]),
    selectInput('color', 'Measure Color', c('None', names(dataset)), names(dataset)[[4]]),
    selectInput('facet_row', 'Facet Row', c(None='.', names(dataset)), names(dataset)[[2]]),
    selectInput('facet_col', 'Facet Column', c(None='.', names(dataset))),
    textInput('caption', 'Plot Caption', value='Plot on Diamond Dataset'),
    downloadButton('downloadImage', 'Download modified image')),
  
  mainPanel(
    # Add text to guide users of this application
    h4('Introduction & How to use'),
    p("A dataset containing the prices and other attributes of almost 54,000 Diamonds. Using this shiny application we can interactively change different plot attributes and App will plot those. This is very easy and interactive application which gives an idea of Diamond attributes and relation between them."),
    br(),
    
    # call plot function
    plotOutput('plot')
  )
  

  
  
  
)




















