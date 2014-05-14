#Define the function for plotdb_2
#This function creates a grouped barplot using the provided stock names plus the control line which is coded in the inside the function.
#Two arguments are provided, the attribute name (character vector) and a list of stock names.
#The function retrive the data from the DB and obtains the mean and sd values for each stock. All the seasons are retrieved.


  printplot<- function(attr, stk, ctrl){
      
    if (ctrl == T){
    stk["control"]<-"chaendler" ##Add the control line name to the list of stocks
    }
    drv<-dbDriver("PostgreSQL")
    con <- postgresqlNewConnection(drv=drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
    #Set the search path to chado, public in the database
    path<-postgresqlExecStatement(con, "SET search_path TO chado, public;")
    dbClearResult(path)
    query=("SELECT s2.uniquename AS stock, esp.value AS season, c1.name AS attribute, 
            p.value AS value, epr.value AS date 
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
                WHERE s2.uniquename = $1 AND c1.name = $2")
   #Define the df which will contain the data for ggplot 
    df<-data.frame()
   
#Loop over the list of stocks and retrieve the data from the DB.   
for (i in names(stk)){
  #Send the query with the given arguments
  res<-postgresqlExecStatement(con=con, statement=query, params= c(stk[i], attr))
  #Fetch the results to a df
  results<-fetch(res, n=-1)
  #Set the values column of the phenotypic attr to numeric since in the DB is defined as character
  results$value<-as.numeric(results$value)
  dbClearResult(res)
  #Check if there is data for the selected values. Stop the execution in the case there is no data retrieved.
  if (nrow(results) == 0) {
    err<-paste("No data for the line", i, "please choose other stock")
    stop(err)
  }
  #Get the mean values of a given line aggregating by seasons and stored as a df
  mean.stk<-aggregate(value~season, data=results, mean, na.rm= T, simplify = T)
  #Add the line name to the df
  mean.stk$line<-rep(i)
  #Get the sd and append it to the mean.stk df
  sd.val<-aggregate(formula= value~season, data=results, FUN = "sd")
  mean.stk$sd<-sd.val$value
  ##Append all the result to a df with the aim of provide it to ggplot
  df<-rbind(df, mean.stk)
  }

  postgresqlCloseConnection(con)
##Grouped bar plot with sd and y-axis label
ggplot(df, aes(season, value, fill=line)) +
  geom_bar(position="dodge",stat="identity") +
  geom_errorbar(aes(ymin = value - sd, ymax = value + sd),
                side=.3,
                width=.2, # Width of the error bars
                position=position_dodge(.9)) +
  labs(y = attr)


  }


