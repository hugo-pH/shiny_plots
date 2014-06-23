##get_data.R, scatter plot 
##This function retrieve data from the db using 4 arguments, 2 traits (at1, at2), 
#a list of stock and a character vector of year.
#Regarding to the seasons, the function will retrieve all of them if the checkbox(input$all) 
#is checked. When this happens, the query is defined without the $4 argument.
#Once the data is retrieved, the function subset the 'results' df to obtain
#two new df (at1.df, at2.df) which will be merged again to create a new df containing one column for each phenotypic value. 
#This is done because the scatter plot needs of one column for each axis. 
#Merging is performed with the stock.uniquename (results$stock) value which is the only that allow the merge since the other rows are repeated (as date or stock).



  query_db<-function(at1, at2, stk, year){
    
    drv<-dbDriver("PostgreSQL")
    con <- postgresqlNewConnection(drv=drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
    #Set the search path to chado, public in the database
    path<-postgresqlExecStatement(con, "SET search_path TO chado, public;")
    dbClearResult(path)
    
    if (year == "all"){
      query=("SELECT s2.uniquename AS line, esp.value AS season, c1.name AS attribute, 
           p.value AS value, epr.value AS date, cvp.value AS unit
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
             WHERE s2.uniquename = $1 AND c1.name IN ($2, $3) 
             AND cvp.type_id = 43425") 
     
    }else{
      query=("SELECT s2.uniquename AS line, s.uniquename AS stock, esp.value AS season, c1.name AS attribute, 
         p.value AS value, epr.value AS date, cvp1.value AS unit
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
           JOIN cvtermprop cvp1 ON cvp1.cvterm_id = c1.cvterm_id
               WHERE s2.uniquename = $1 AND c1.name IN ($2, $3) AND esp.value = $4
           AND cvp1.type_id = 43425")
    }
    
    df<-data.frame()
    
    #Loop over the list of stocks and retrieve the data from the DB.
    for (i in names(stk)){
    #Send the query with the given arguments 
    if (year == "all"){  
      res<-postgresqlExecStatement(con=con, statement=query, params= c(stk[i], at1, at2))
    }else{
      res<-postgresqlExecStatement(con=con, statement=query, params= c(stk[i], at1, at2, year))
    }
      #Fetch the results to a df
    results<-fetch(res, n=-1)
    #Set the values column of the phenotypic attr to numeric since in the DB is defined as character
    results$value<-as.numeric(results$value)
    dbClearResult(res)
    #Check if there is data for the selected values. Stop the execution in the case there is no data retrieved.
    if (nrow(results) == 0) {
      err<-paste("No data, please choose other stock")
      stop(err)
    }
    df<-rbind(df, results)
  }
    postgresqlCloseConnection(con)
  
    #Creating a new df from 'results' which contain one column for each pheno value. 
    at1.df<-subset(df, df$attribute == at1)
    at2.df<-subset(df, df$attribute == at2)
    atf<-merge(at1.df, at2.df, by=(c("line", "date", "season")))
   
   return(atf)
  }
