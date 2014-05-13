#server.R
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
  stk<- input$line
  stock<-input$stocks
  stk.ls[stock]<-stock
#   paste(stk.ls[1], stk.ls[2], "class:",class(stk.ls))

                          
p<-printplot(stk=stk.ls, attr=attribute)
   print(p)
})


})