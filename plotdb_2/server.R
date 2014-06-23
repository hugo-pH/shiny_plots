#server.R, plotdb_2
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
source("barplot.R")
source("plot_no_stat.R")
stk.ls<-list()
shinyServer(function(input, output) {
  
  options<-reactive({
    subset(bulkdata, attribute==input$attribute)
    })
  
  
  output$stk<-renderUI({
    if (is.null(input$attribute) == TRUE | input$mult == TRUE){
      return()
    }else{
      options<-options()
      stk.opt<-as.vector(options[["stock"]])
     
      selectInput("stock",
                        label="Choose a stock",
                        choices = stk.opt,
                        selected = NULL)                  
    }
  })
  
  
  #Ui select button
  output$list<-renderUI({
    if (input$mult == FALSE){
      return()
    }else{
      options<-options()
      stk.opt<-as.vector(options[["stock"]])
      selectInput("stkList",
                        label="Choose a stock",
                        choices = stk.opt,
                        selected = NULL,
                        multiple=TRUE)
    }
  })
  
  
  
  #Get the selected values from the page, and print them in the plot.       
data<-reactive({
  if (input$go==0){
      return(NULL)
  }else{
      if (input$mult == FALSE){
      stock<-input$stock
        }else{
      stock<-input$stkList
        }

    stk.ls[stock]<-stock
    stk.ls["control"]<-"chaendler"  
    if (stock == "chaendler"){
      stop("You have selected the control stock, please select other stock to compare with the control.")
    }
    data.ls<-lapply(stk.ls, function(x){
    subdata<-subset(bulkdata, attribute==input$attribute & stock==x)
    if (nrow(subdata) == 0) {
      err<-paste("The line", x, "don't have values for this phenotypic attribute, please choose other stock or attribute")
      stop(err)}
      subdata

    })
    rbindlist(data.ls)
  }
  })
  
  
  
  #Get the selected values from the page, and print them in the plot.       
output$map<-renderPlot({  
  if (input$go==0){
      return(NULL)
  }else{  
  
  attribute <- input$attribute  
  if (input$mult == FALSE){
    stock<-input$stock
  }else{
    stock<-input$stkList
  }
  #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument     
  stk.ls[stock]<-stock

  data<-data()
#Execute the function defined in barplot.R or plot_no_stat.R depending on the checkbox
    if (input$mult == FALSE){
      p<-printplot(data, stk=stk.ls)
    }else{
      p<-plot(data, stk=stk.ls)
    }
 print(p)
}
})


})