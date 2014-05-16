#server.R, scatter_plot
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")

#Script where the query_db function is defined
source("get_data.R")
#Define the list which will contain the selected lines
stk.ls<-list()
shinyServer(function(input, output) {
  
  #Get the selected values from the page, and print them in the plot.
  data<-reactive({
    at1 <- input$at1
    at2 <- input$at2
    stock<-input$stocks
    #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument
    stk.ls[stock]<-stock
    
    year<-input$season
    
    if (is.null(stock) == TRUE){
      stop("There are no values selected, please enter your choices")
    }
    all<-input$all 
    # if the user select the checkbox, the value of seasons will be "all" and the query will select all the years
    if (all == T){
      query_db(at1=at1, at2=at2, stk= stk.ls, year = "all")
    }else{
      query_db(at1=at1, at2=at2, stk= stk.ls, year = year)  
    }
  })
  
  output$plot<-renderPlot({
    
  
#   plot_data<-data.frame(attr1 = results$value, attr2 = results2$value)
  p <- ggplot(data(), aes(value.x, value.y))
  p<- p + geom_point(aes(colour = factor(line)), size = 4) #define plot geom (scatter) and map it to lines
  p<- p + aes(shape = factor(line)) + # maps the shape of the point to lines
      geom_point(colour="grey90", size = 1.5) + # a new layer of smaller points which overlay with the coloured points making then empty
      labs(x=input$at1, y=input$at2) + #labs for both axis
      scale_shape_discrete(name="line") + # The name of the legend, it's necessary to define it twice since we hace 'colours' and 'shape' defined
      scale_color_discrete(name="line")
  
  print(p)
  })
})