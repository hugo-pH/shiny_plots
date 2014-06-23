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

bulk.ls<-split(bulkdata, bulkdata$attribute)

#Script where the query_db function is defined
source("get_data.R")
#Define the list which will contain the selected lines
stk.ls<-list()
shinyServer(function(input, output) {
  
  op1<-reactive({
    at1=input$at1
    at2=input$at2
    at1df<-bulk.ls[[at1]]
    at2df<-bulk.ls[[at2]]
    merge(at1df, at2df, by=c("stock", "sample", "date", "season"))
  })

  output$select.season<-renderUI({
    if (is.null(input$at2) == TRUE | input$all == TRUE){
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
  op2<-reactive({
    if (is.null(input$at2) == TRUE){
      return()
    }else{
      all<-input$all
      year=input$season
      d<-op1()
      if(all == TRUE){
        op1()
      }else{
      subset(d, season==year)
    }
    }
  })
  output$select.stk<-renderUI({
    if (is.null(input$season) == TRUE){
      return()
    }else{
      options<-op2()
      stk.opt<-as.vector(options[["stock"]])
      
      selectInput("stock",
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
      d<-op2()  
      stock<-input$stock
      #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument
      stk.ls[stock]<-stock
      ls<-lapply(stk.ls, function(x){
        subset(d, stock==x)})
      rbindlist(ls)
    
    }
    })
  
  output$plot<-renderPlot({
    if (input$go==0){
      return(NULL)
    }else{
    data<-data()
    stk.ls<-as.list(levels(as.factor(data$stock)))
#     stock<-input$stock
    #Create a list from the selected values of the selectInput widget. The list will be supplied to the printplot function which needs a stock list as an argument
#     stk.ls[stock]<-stock
    max.y<-max(data$value.y)
    min.x<-min(data$value.x)

    cor.plot<-function(i, data, y, x){ 
      d<-subset(data, stock == i)
      if(length(d$value.x) < 5 | length(d$value.y) < 5){
        stop("The selected stock has less than 5 values, please select other stock")
      }
      if(length(d$value.x) > 2000){
        nor.x<-ad.test(d$value.x)
        }else{
      nor.x<-shapiro.test(d$value.x)
        }
      if(length(d$value.y) > 2000){
        nor.y<-ad.test(d$value.y)
      }else{
        nor.y<-shapiro.test(d$value.y)
      }
      
      if (nor.x[["p.value"]] > 0.05 | nor.y[["p.value"]] > 0.05){
        ct<-cor.test(d$value.x, d$value.y, method="pearson")
#         r=format(ct[["estimate"]], digits=3)
#         pv=format(ct[["p.value"]], digits=3)
        cor.res<-data.frame(stock = i, xr = (1.1 * x), yr = (1.15 * y),
        R=as.character(as.expression(substitute(~~Spearman~coeff~"="~r, 
                                                list(r=format(ct[["estimate"]], digits=3))))),
        method=rep("spearman"),
        xp= (1.1 * x), yp = (1.05 * y),
        P=as.character(as.expression(substitute(~~P.value~"="~pv, 
                                                list(pv=format(ct[["p.value"]], digits=3)))))            
                            )
        cor.res
        
      } else {
        ct<-cor.test(d$value.x, d$value.y, method="spearman", exact=FALSE)
#         r=as.character(as.expression(paste0("Spearman coeff=", format(ct[["estimate"]], digits=3), "\n")))
#         pv=paste0("P value=", format(ct[["p.value"]], digits=3))
        cor.res<-data.frame(stock = i, xr = (1.1 * x), yr = (1.15 * y),
                            R=as.character(as.expression(substitute(~~Spearman~coeff~"="~r, 
                                                                    list(r=format(ct[["estimate"]], digits=3))))),
                            method=rep("spearman"),
                            xp= (1.1 * x), yp = (1.05 * y),
                            P=as.character(as.expression(substitute(~~P.value~"="~pv, 
                                                                     list(pv=format(ct[["p.value"]], digits=3)))))
                              )
        cor.res
      }
    }
    cor.res<-lapply(stk.ls, cor.plot, data=data, x=min.x, y=max.y)
    #http://stackoverflow.com/questions/17293522/adding-r2-to-each-facet-in-ggplot2
    
    
    cor.df<-rbindlist(cor.res)
    p <- ggplot(data, aes(value.x, value.y)) + geom_point(aes(colour = factor(stock)), size = 4) #define plot geom (scatter) and map it to lines
    p <- p + facet_wrap(~stock) +
      geom_point(colour="grey90", size = 1.5) +
#       aes(shape = factor(stock)) + # maps the shape of the point to lines
      scale_shape_discrete(name="stock") + # The name of the legend, it's necessary to define it twice since we have 'colours' and 'shape' defined
      scale_color_discrete(name="stock") +
      geom_smooth(method="lm")+
      labs(x=paste0(input$at1, " ", "(", unique(data$unit.x), ")" ), y=paste0(input$at2, " ", "(", unique(data$unit.y), ")" )) + #labs for both axis
      geom_text(data=cor.df, aes(x=xr, y=yr, label=R, group=NULL),size=4, hjust=0, parse=T)  +
      geom_text(data=cor.df, aes(x=xp, y=yp, label=P, group=NULL),size=4, hjust=0, parse=T) 
    print(p)
    
    }
  })
})