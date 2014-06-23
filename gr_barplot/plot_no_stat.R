#Define the function for gr_barplot
#This function creates a grouped barplot using a list of stock names plus the control line which is coded in the inside the function.
#Two arguments are provided, a df containing the data and a list of stock names.
s.plot<- function(data, stk){
  
  stk["control"]<-"chaendler"  
  
  # Get the mean values grouping by stock, season and attributes.
  dataplot<-aggregate(data$value, by= list(data$stock, data$season, 
                                           data$attribute, data$unit),
                      mean, na.rm= T, simplify = T)
  #Get the standard deviation values grouping by stock, season and attributes.
  sd<-aggregate(data$value, by= list(data$stock, data$season, 
                                     data$attribute, data$unit),
                sd, na.rm= T, simplify = T)
  #Add the sd values to the 'dataplot' df where the mean values are keeped. 
  dataplot$sd<-sd$x
  #Change the names of the 'dataplot' df columns since aggregate function doesn't add the proper names
  colnames(dataplot)<-c("stock", "season", "attribute", "unit",  "mean", "sd")
  
  
  p<-ggplot(dataplot, aes(season, mean, fill=stock))
  
  ##Grouped bar plot with sd and y-axis label
  p<- p + geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                  side=.3,
                  width=.2, # Width of the error bars
                  position=position_dodge(.9)) +
    labs(y =  paste0(unique(dataplot$attribute), " ", "(", unique(dataplot$unit), ")"),
         title="No statistical analysis possible")
  
  p
}