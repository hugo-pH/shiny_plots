#server.R, disperssion
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
library(agricolae)
library(car)
library(data.table)
library(plyr)
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s2.uniquename AS stock, esp.value AS season,
                      c1.name AS attribute, p.value AS value, epr.value AS date, cvp.value AS unit 
                      FROM stock s 
                      JOIN stock_relationship sr ON sr.subject_id = s.stock_id
                      JOIN stock s2 ON sr.object_id=s2.stock_id
                      JOIN nd_experiment_stock es ON s.stock_id = es.stock_id
                      JOIN nd_experiment_stockprop esp ON es.nd_experiment_stock_id = esp.nd_experiment_stock_id         
                      LEFT JOIN chado_stock ck ON s.stock_id = ck.stock_id         
                      JOIN nd_experiment_phenotype ep ON es.nd_experiment_id = ep.nd_experiment_id             
                      JOIN nd_experimentprop epr ON ep.nd_experiment_id = epr.nd_experiment_id            
                      JOIN phenotype p ON ep.phenotype_id = p.phenotype_id          
                      JOIN cvterm c1 ON p.attr_id = c1.cvterm_id            
                      JOIN cvtermprop cvp ON cvp.cvterm_id = c1.cvterm_id
                      WHERE cvp.type_id = 43425")
bulkdata$value<-as.numeric(bulkdata$value)
postgresqlCloseConnection(con)
#Script where the dataplot function is defined
source("get_data.R")

shinyServer(function(input, output) {
  
  op1<-reactive({
    subset(bulkdata, attribute==input$attribute)
  })
  
  output$select.stk<-renderUI({
    if (is.null(input$attribute) == TRUE){
      return()
    }else{
      options<-op1()
      stk.opt<-as.vector(options[["stock"]])
      
      selectInput("stock",
                  label="Choose a stock",
                  choices = stk.opt,
                  selected = NULL)                  
    }
  })
  
  op2<-reactive({
    if (nrow(op1) < 1){
      return()
    }else{
   subset(op1(), stock==input$stock)
    }
  })
  
  output$select.season<-renderUI({
    if (is.null(input$attribute) == TRUE){
      return()
    }else{
      options<- op1()
      seas<-as.vector(options[["season"]])
      
      selectInput("season",
                  label = "Choose a season",
                  choices = seas,
                  selected = NULL)
    }
    
  })
    
  #Get the selected values from the page, and print them in the plot.  
  data<-reactive({
    if (input$go==0){
         return(NULL)
      }else{
    attr <- input$attribute  
    stock<-input$stock
    year<-input$season
 
    subset(bulkdata, attribute==attr & stock==stock & season==year)
#     query_db(attr=attribute, stk= stock, year = year)
    }  
})
         
  output$qqplot<-renderPlot({ 
    if (input$go  ==0){
      
      return(NULL)
    }else{
    data<-data()
    qqnorm(data$value)  
    }
    })
  output$histo<-renderPlot({
    if (input$go  ==0){
      
      return(NULL)
    }else{
    data<-data()
    h<-ggplot(data(), aes(x = value)) +
    geom_histogram(aes(x=value, y=..density.., fill = ..density..)) +
    geom_density() +  
    scale_fill_gradient("Count", low = "green", high = "red") +
    labs(x=paste0(input$attribute, " ", "(", unique(data$unit), ")"))  
    
    print(h)
    }
  })  
  output$norm<-renderPrint({
    if (input$go==0){
        return(NULL)
    }else{
    data<-data()
    options(contrasts=c("contr.sum","contr.poly")) 
    shap.res<-shapiro.test(data$value)
    shap.res  
    }
  })
  })