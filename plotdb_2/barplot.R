#Define the function


  printplot<- function(attr, stk){

    stk["control"]<-"chaendler" ##Add the control line name to the list of stocks
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
df<-data.frame()
for (i in names(stk)){
  res<-postgresqlExecStatement(con=con, statement=query, params= c(stk[i], attr))
  results<-fetch(res, n=-1)
  results$value<-as.numeric(results$value)
  dbClearResult(res)

  mean.stk<-aggregate(value~season, data=results, mean, na.rm= T, simplify = T)
  mean.stk$line<-rep(i)

#   Check if there is data for the selected values. If the result of subset is a empty df (nrow=0) stop the execution and print a message.
  if (nrow(mean.stk) == 0) {
    
    stop("No data for your selection, please choose other values.")
  }
 
  df<-rbind(df, mean.stk)
  }

  postgresqlCloseConnection(con)

ggplot(df, aes(season, value, fill=line)) +
  geom_bar(position="dodge",stat="identity")


  }


