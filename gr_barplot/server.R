#server.R, gr_barplot

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
source("functions.R")
stk.ls<-list()
shinyServer(function(input, output) {
  #Reactive object to subset the bulkdata depending on the attribute input. It will be used to determine which stocks are available for a given attribute.
  options<-reactive({
    subset(bulkdata, attribute==input$attribute)
  })
  
  #Create the selectInput for one stock.
  output$stk<-renderUI({
    if (is.null(input$attribute) == TRUE | input$mult == TRUE){ #Returns nothing if there is no attribute selected or the user has checked the multiple-stock checkbox.
      return()
    }else{
      options<-options()
      stk.opt<-as.vector(options[["stock"]])#Get the stock names from the subsetted df, transforms it in a vector. 
      
      selectInput("stock",
                  label="Select an individual stock",
                  choices = stk.opt,
                  selected = NULL)                  
    }
  })
  
  
  #Create the selectInput for a list of stocks. 
  output$list<-renderUI({
    if (input$mult == FALSE){ #if the check button is not selected, don't show this box
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
  
  
  
  #Creates a df containing the user selected values.       
  data<-reactive({
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      return(NULL)
    }else{
      if (input$mult == FALSE){ #if the checkbox is not selected, get the stock from input$stock (unique stock selection)
        stock<-input$stock
        validate( #Show a message if the user selects chaendler stock (control stock)
          need(stock != "chaendler","You have selected the control stock, please select other stock to compare with the control." )
        )
      }else{ # Else, get the stocks from the input$stkList (multiple stock selection)
        stock<-input$stkList
      }
      
      #Creates a list of stock from the users selection and add the control stock
      stk.ls[stock]<-stock
      stk.ls["control"]<-"chaendler"  
    
      #Loops over the stk.ls list and subset for the given stock and the selected attribute. Creates a list of df, each one corresponding to one stock and one attribute
      data.ls<-lapply(stk.ls, function(x){
        subdata<-subset(bulkdata, attribute==input$attribute & stock==x)
        validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
          need(nrow(subdata)>0, "Waiting for your selection")
        )
        subdata
        
      })
      #Bind all the df in the data.ls list, creates a unique df
      rbindlist(data.ls)
    }
  })
  alpha<-reactive({
    input$alpha
  })
  
  output$plot<-renderPlot({  
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      return(NULL)
    }else{  
      data<-data()
      attribute <- input$attribute  
      if (input$mult == FALSE){#if the checkbox is not selected, get the stock from input$stock (unique stock selection)
        stock<-input$stock
      }else{  # Else, get the stocks from the input$stkList (multiple stock selection)
        stock<-input$stkList
      }
      #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument     
      stk.ls[stock]<-stock
      
    
      #Execute the function defined in barplot.R or plot_no_stat.R depending on the checkbox
      if (input$mult == FALSE){
        p<-printplot(data, stk=stk.ls)
      }else{
        p<-s.plot(data, stk=stk.ls)
      }
      print(p)
    }
  })
  
  
})