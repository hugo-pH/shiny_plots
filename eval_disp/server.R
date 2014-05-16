#server.R, eval_disp
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")

#Script where the dataplot function is defined
source("get_data.R")
stk.ls<-list()
shinyServer(function(input, output) {
   
    data<-reactive({
    attribute <- input$attribute  
    
    stock<-input$stocks
    #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument   
    stk.ls[stock]<-stock
    if (is.null(stock) == TRUE){
      stop("There are no values selected, please enter your choices")
    }
    query_db(attr=attribute, stk=stk.ls, ctrl=input$control)
    })
    
    output$map<-renderPlot({    
        p<-ggplot(data(), aes(factor(season), value)) +
        labs(x="year") +
        scale_fill_discrete(name="line") #legend title

        p <- p  +  geom_boxplot(aes(fill = factor(stock))) 
        print(p)
  })
    output$table1<- renderDataTable({
      data()
    })
  
})
