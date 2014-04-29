#server.R
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s.uniquename AS sample, s2.uniquename AS stock, esp.value AS season, p.name AS uniquename_sample, c1.name AS attribute, c2.name AS observable_object, p.value AS value, epr.value AS date 
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
                      JOIN cvterm c2 ON p.observable_id = c2.cvterm_id")
#Script were the dataplot function is defined
source("barplot.R")
shinyServer(function(input, output) {

#Get the selected values from the page, and print them in the plot.       
  output$map<-renderPlot({  
    
    attribute <- input$attribute  
        
    season <- input$season
      
        p<-printplot(data=bulkdata, attr=attribute, year = season)
        print(p)
        })

})