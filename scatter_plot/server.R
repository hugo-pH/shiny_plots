#server.R, scatter_plot
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
library(agricolae)
library(car)
library(data.table)
library(plyr)
library(nortest)
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s2.uniquename AS stock, s.uniquename AS sample, esp.value AS season,
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


#Create a list of df, each one storing values for an unique attribute
bulk.ls<-split(bulkdata, bulkdata$attribute)

#Define the list which will contain the selected lines
stk.ls<-list()

shinyServer(function(input, output) {
  
  #Reactive object to subset the bulkdata depending on the attribute inputs. It will be used to determine which seasons are available for a given attribute.
  op1<-reactive({
    at1=input$at1
    at2=input$at2
    at1df<-bulk.ls[[at1]] #Get the df from the df list which contains the values of the first input attribute
    at2df<-bulk.ls[[at2]] ##Get the df from the df list  which contains the values of the second input attribute
    data<-merge(at1df, at2df, by=c("stock", "sample", "date", "season"))#Merge both df into a unique df which contains the data from both attributes. New columns are named attribute.x  value.x	unit.x	attribute.y	value.y	unit.y
    validate(#Avoid red error message to be shown when the two selected attributes cannot be compare (they're not in the same year). Meanwhile, print the message.
      need(nrow(data) > 1, "It's not possible to compare the two selected attributes, please select another pair of values."))
    data
    })
  

  #Create the selectInput for seasons. 
  output$select.season<-renderUI({
    if (input$all == TRUE){
      return()
    }
    if (input$at2 == ""){ #If attr 2 is empty OR the checkbox is selected, the selectInput for seasons is not shown
      seas<-""
    }else{
      options<- op1()    
      seas<-as.vector(options[["season"]])
    }
      selectInput("season",
                  label = "Choose a season",
                  choices = seas,
                  selected = NULL)
    
  })
  #Reactive object to subset the op1 object depending on the season input. 
  #It's used to determine which stock are available for a given attribute and season.
  op2<-reactive({
    if (input$at2 == ""){ #If attr 2 is empty do nothing
      return()
    }else{
      all<-input$all
      year=input$season
      d<-op1()
      if(all == TRUE){#If the checkbox is selected, don't subset the op1 and select all years
        op1()
      }else{
      subset(d, season==year)
    }
    }
  })
  output$select.stk<-renderUI({
    options<-op2()  
    
    if (input$season == ""){ #If season is empty, the selectInput for stock is not shown
      stk.opt<-NULL
    }else{
      
      stk.opt<-as.vector(options[["stock"]])
    }
    if (input$all == TRUE){
      
      stk.opt<-as.vector(options[["stock"]])
    }
      selectInput("stock",
                  label="Choose a stock",
                  choices = stk.opt,
                  selected = NULL,
                  multiple=TRUE)                  
    
  })
 
  
  #Get the final dataset using the stock to subset the op2 object
  data<-reactive({
    if (input$go==0){
      return(NULL)
    }else{
      d<-op2()  
      stock<-input$stock
      #Create a list of stock from the selected values in the selectInpeut widget. 
      stk.ls[stock]<-stock
      #Loop over the list of stock and subset the op2 df using the stock name.
      #It returns a list of df, each one containing values for a unique stock
      ls<-lapply(stk.ls, function(x){
        subset(d, stock==x)})
      #Bind all df in the ls list
      rbindlist(ls)
    
    }
    })

  
  output$plot<-renderPlot({
    if (input$go==0){  #if the 'Run' button is not clicked, don't start the plot
      return(NULL)
    }else{
    data<-data()

    validate(#Avoid red error message to be shown when the user changes the attribute. Meanwhile, print the message "waiting for your selection"
      need(nrow(data)>1, "Waiting for your selection")
      )
    #Create a list of stock from the stock names stored in data(), that is, the stocks selected by the user
    stk.ls<-as.list(levels(as.factor(data$stock)))
    #Get the max value in y and min value in x. This is used to define the position of labels within the plot
    max.y<-max(data$value.y)
    min.x<-min(data$value.x)
    
    #######################
    ###cor.plot function###
    #######################
    #This function tests for normality of the data, calculates the correlation coefficients for two of attributes
    
    cor.plot<-function(i, data, y, x){ 
      #Subset the data to get a unique stock
      d<-subset(data, stock == i)
      if(length(d$value.x) < 5 | length(d$value.y) < 5){ #If the stock has less than 5 values the statistic cannot be computed, so stop the execution and print an error
        stop(paste0("The stock ", i, " has less than 5 values, please remove this stock from your selection"))
      }
      if(length(d$value.x) > 2000){#If there are more than 2000 values in value.x (attr1), performs a  Anderson-Darling test  from nortest package
        nor.x<-ad.test(d$value.x)
      }else{#When there are less than 2000 values, performs a shapiro test
        nor.x<-shapiro.test(d$value.x)
      }
      if(length(d$value.y) > 2000){#If there are more than 2000 values in value.y (attr2), performs a  Anderson-Darling test  from nortest package
        nor.y<-ad.test(d$value.y)
      }else{#When there are less than 2000 values, performs a shapiro test
        nor.y<-shapiro.test(d$value.y)
      }
      
      if (nor.x[["p.value"]] > 0.05 | nor.y[["p.value"]] > 0.05){#If one of p.values obtained in the previous test is greater than 0.05, performs correlation using the Pearson method
        #Correlation test with Pearson method
        ct<-cor.test(d$value.x, d$value.y, method="pearson")
        #Analysis name, it is used to print the type of the analysis in the label
        method<-"Pearson"
        
      } else {#If one of p.values obtained in the previous test is greater than 0.05, performs correlation using the Spearman method
        ct<-cor.test(d$value.x, d$value.y, method="spearman", exact=FALSE)
        #Analysis name, , it is used to print the type of the analysis in the label
        method<-"Spearman"
      }
      
      #Create a df (8 columns) containg the results of the analysis, and create the labels for the plot
      #Columns: stock (for stock names), xr an yr (position for R coeff label), R (coeff label), 
      #method (the used method in correlation test), P(p.value label), xp and yp (coordinates for P label)
      data.frame(stock = i, 
                 # R label is placed on the top. y and x are the max and min values in each axis respectively. 
                 xr = (1.1 * x), yr = (1.15 * y), 
                 #R label is built from the ct object which stores the cor.test results. 
                 #            substitute and as.expression are used to create a proper label for ggplot
                 R=as.character(as.expression(substitute(~~m~coeff~"="~r, 
                                                         list(r=format(ct[["estimate"]], digits=3), m=method)))),
                 method=rep(method),
                 #P label is placed above R label (the y position is multiplied into 1.05 instead of 1.15)
                 xp= (1.1 * x), yp = (1.05 * y),
                 #P value, use ct[["p.value"]] from cor.test
                 P=as.character(as.expression(substitute(~~P.value~"="~pv, 
                                                         list(pv=format(ct[["p.value"]], digits=3)))))            
      )
      
    }
    #Loop over the list of stock and apply the cor.plot function. A list of the previous df is returned. 
    cor.res<-lapply(stk.ls, cor.plot, data=data, x=min.x, y=max.y)
    #Bind all the df in the df.list    
        cor.df<-rbindlist(cor.res)
    
    #define plot geom (scatter) and map it to stock
    p <- ggplot(data, aes(value.x, value.y)) + geom_point(aes(colour = factor(stock)), size = 4) 
    p <- p + facet_wrap(~stock) +
      geom_point(colour="grey90", size = 1.5) +
      scale_color_discrete(name="stock") +# The name of the legend,
      geom_smooth(method="lm")+
      # Axis labs, attribute name (unit)
      labs(x=paste0(input$at1, " ", "(", unique(data$unit.x), ")" ), y=paste0(input$at2, " ", "(", unique(data$unit.y), ")" )) + #labs for both axis
      # R label, correlation coefficient
      geom_text(data=cor.df, aes(x=xr, y=yr, label=R, group=NULL),size=4, hjust=0, parse=T)  +
      # P label, p.value
      geom_text(data=cor.df, aes(x=xp, y=yp, label=P, group=NULL),size=4, hjust=0, parse=T) 
    print(p)
    
    }
  })
})