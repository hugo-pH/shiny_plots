#server.R, plotdb_2
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")

#Script where the dataplot function is defined
source("barplot.R")
stk.ls<-list()
shinyServer(function(input, output) {
  
  #Get the selected values from the page, and print them in the plot.       
  output$map<-renderPlot({  
    
  attribute <- input$attribute  
    
  stock<-input$stocks
#Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument   
  stk.ls[stock]<-stock
if (is.null(stock) == TRUE){
  stop("There are no values selected, please enter your choices")
  }
  control<-input$control

#Execute the function defined in barplot.R
p<-printplot(stk=stk.ls, attr=attribute, ctrl= control)
   print(p)
})


})