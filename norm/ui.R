#ui.R, disperssion
#Load library for PgSQL connection
library("RPostgreSQL")
#Connect to the database
drv<-dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="drupal", host="10.0.0.16", user="drupal", port ="5432")
#Set the search path to chado, public in the database
dbSendQuery(con, "SET search_path TO chado, public;")
#Query the DB to get all the attributes that are in the phenotype table
#The query returns a dataframe with 1 column
attr_df<-dbGetQuery(con, 'SELECT DISTINCT c.name AS attribute FROM cvterm c 
                    JOIN phenotype p ON  c.cvterm_id = p.attr_id ORDER BY c.name ASC')
#Transform the #1 column to a vector, this is necessary since the 'selectInput'
# widget needs a vector for the 'choices' parameter
attr_names<-attr_df[,1]
#Get all the seasons
seasons_df<-dbGetQuery(con, 'SELECT DISTINCT nds.value AS season FROM nd_experiment_stockprop nds ORDER BY  nds.value ASC')
#Same transformation than in the previous query
seasons<-seasons_df[,1]
#Get the stock names from the DB
lines_df<-dbGetQuery(con, 'SELECT DISTINCT s2.uniquename AS stock
                     FROM stock s 
                     JOIN stock_relationship sr ON sr.subject_id = s.stock_id
                     JOIN stock s2 ON sr.object_id=s2.stock_id ORDER BY s2.uniquename ASC')
postgresqlCloseConnection(con)
lines<-lines_df[,1]
#start UI
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      #Three selectInputs: attribute, stock and season
      #The attribute input is fixed and gets the values from the DB
      #The stock input options depends on the attribute. Defined in server.R
      #The season input options depends on the attribute and the stock selected previously. Defined in server.R
      #The 'Run' button prevents shiny for starting before any option is selected. 
      selectInput("attribute",
                  label="Choose a phenotypic attribute",
                  choices = attr_names,
                    selected = NULL),
      uiOutput("select.season"),
      
      uiOutput("select.stk"),
      
      actionButton("go","Run")

    ),
    
    mainPanel( 
      #The main panel is composed of two tabs, one for the historgram and the other for the QQ plot
      tabsetPanel(type="tabs",
                  tabPanel("Histogram", plotOutput("histo"), verbatimTextOutput("norm")),
                  tabPanel("Q-Q plot", plotOutput("qqplot"))
                  ),
      #Avoid error messages to be printed in red color    
      tags$style(type="text/css",
                 "#map.shiny-output-error { color: inherit;}",
                 "#map.shiny-output-error:before { content: ; }"
      )
    )
  )
))