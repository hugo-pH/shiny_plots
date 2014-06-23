#server.R, plotdb
#Load libraries for shiny, plots and PgSQL connection
library(shiny)
library(ggplot2)
library("RPostgreSQL")
library(agricolae)
library(car)
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the phenotypics values from all attributes and store it in a dataframe
bulkdata<- dbGetQuery(con, "SELECT s.uniquename AS sample, s2.uniquename AS stock, esp.value AS season,
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
#Script were the dataplot function is defined
source("barplot.R")
source("download_result.R")
shinyServer(function(input, output) {
  #Reactive object to subset the bulkdata depending on the attribute input. It will be used to determine which seasons are availables for a given attribute.
  options<-reactive({
    subset(bulkdata, attribute==input$attribute)
  })
  #Create the selectInput for seasons.    
  output$select.season<-renderUI({
    if (is.null(input$attribute) == TRUE){
      return() # If no attribute is selected, return a empty object. 
    }else{
      options<-options()#If an attribute is selected, get the seasons availables in the subsetted 'options' object
      seas<-as.vector(options[["season"]])
      #Create the selectInput 
      selectInput("season",
                  label = "Choose a season",
                  choices = seas,
                  selected = NULL)
    }

  })

  #Get the dataset which will be ploted latter. 
  data<-reactive({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      
      return(NULL)
    }else{  #Else subset the bulkdata object depending on the user inputs. 
   sb<-subset(bulkdata, attribute==input$attribute & season==input$season)
    }
   })
  
  #Plot tab
  output$plot<-renderPlot({
    if (input$go==0){ # if the 'Run' button is not clicked, returns nothing.
      
      return(NULL)
    }else{ # Else, print the plot. The printplot function is defined in barplot.R
        p<-printplot(data=data())
        print(p)      
    }
        })
  #Normality tab, print the result of an normality test       
  output$norm<-renderPrint({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      
      return(NULL)
    }else{ #Else, shapiro.test on the attribute values.
    data<-data()
    shap.res<-shapiro.test(data$value)
    shap.res  
    }
    })
  #Homocedasticity tab, , print the result of an levene Test for homogenity of variances.
  output$homo<-renderPrint({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      
      return(NULL)
    }else{ # Else, leveneTest on the attribute values over the stocks. 
    data<-data()
    lev.res<-leveneTest(value ~ stock, data=data)
    lev.res
    }
    })
  #Differences tab, print the result of an statistical test
  output$aov<-renderPrint({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      
      return(NULL)
    }else{ #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
    #If the p.value is > 0.05 an ANOVA will be performed, else a kruskal.test will be performed. 
    data<-data()
    lev.res<-leveneTest(value ~ as.factor(stock), data=data)
    lev.pval<-lev.res[["Pr(>F)"]][[1]]
        if (lev.pval  > 0.05){ 
        aov.res<-aov(value ~ stock, data)
        summary(aov.res)
        }else{
        kw.res<-kruskal.test(value~as.factor(stock), data = data)
        kw.res
        }
    }
       })
  #Groups tab. Print the result of an statistical test to search for differences among groups (Tukey test or Kruskall)
  output$groups<-renderPrint({
    if (input$go  ==0){ # if the 'Run' button is not clicked, return nothing.
      
      return(NULL)
    }else{ 
    #Perform a test, ANOVA or Kruskal depending on the obtained levene.Test p.value 
    data<-data()
    lev.res<-leveneTest(value ~ as.factor(stock), data=data)
    lev.pval<-lev.res[["Pr(>F)"]][[1]]
    if (lev.pval > 0.05){#If the p.value is > 0.05 an ANOVA will be performed,
      aov.res<-aov(value ~ stock, data)
      aov.sum<-summary(aov.res)
      aov.pval<-aov.sum[[1]]$'Pr(>F)'[[1]]#here is where the p.value is obtained in the summary(aov) object
          if(aov.pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a Tukey test will be performed
            tuk.res<-HSD.test(aov.res, "stock", group = T)  
            tuk.res
          }else{ # Else, print this:
            print("Anova was no significant")
          }
    }else{ #else a kruskal.test will be performed. 
      kw.res<-kruskal.test(value~as.factor(stock), data = data)
            if(kw.res[["p.value"]] < 0.05){ # If the p.value obtained is < 0.05 a kruskal test from the agricolae package will be performed.
              group.res<-kruskal(y=data$value, trt=data$stock, group=T,  p.adj="bonferroni")
              group.res
            }else{# Else, print this:
            print("The kruskal test was not significant")
             }
      }
    }   
    })

output$downloadData <- downloadHandler(

  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste0(input$attribute, "-", input$season, ".csv")
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    # Write to a file specified by the 'file' argument
    write.table(download(data()), file, sep="\t", row.names = FALSE)
  
  }
)
})