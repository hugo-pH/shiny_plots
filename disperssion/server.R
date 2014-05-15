#server.R, disperssion
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")

#Script where the dataplot function is defined
source("get_data.R")

shinyServer(function(input, output) {
  
  #Get the selected values from the page, and print them in the plot.  
  data<-reactive({
    attribute <- input$attribute  
    stock<-input$stocks
    year<-input$season
    if (is.null(stock) == TRUE){
      stop("There are no values selected, please enter your choices")
    }
    query_db(attr=attribute, stk= stock, year = year)
  })
         
  output$box<-renderPlot({  
    
    p<-  ggplot(data(), aes(as.factor(stock), value)) + #X axis groups is season
      geom_boxplot(notch = T, width = .3) + # legend group is stocks 
      geom_jitter() + #dots
      labs(x="line")
    print(p)
    })
  output$histo<-renderPlot({
    
    h<-ggplot(data(), aes(x = value)) +
    labs(x = input$attribute) +
    geom_histogram(aes(fill = ..count..), binwidth = 20) + 
    scale_fill_gradient("Count", low = "green", high = "red") 
    print(h)
  })  
  
  })