#ui.R, homogeneity
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

postgresqlCloseConnection(con)

#start UI
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      #Two selectInputs: attribute and season
      #The attribute input is fixed and gets the values from the DB
      #The season input options depends on the attribute selected previously. Defined in server.R
      #The 'Run' button prevents shiny for starting before any option is selected.
      selectInput("attribute",
                  label="Choose a phenotypic attribute",
                  choices = attr_names,
                  selected = NULL),
      uiOutput("select.season"),
      actionButton("go","Run")
    ),
    mainPanel( 
      #Avoid error messages to be printed in red color
      plotOutput("map"),
      verbatimTextOutput("bart"),    
      verbatimTextOutput("lev"), 
      verbatimTextOutput("flig"), 
      tags$style(type="text/css",
                 "#map.shiny-output-error { color: inherit;}",
                 "#map.shiny-output-error:before { content: ; }"
      )
    )
  )
))