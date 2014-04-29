## create a dataframe
library(ggplot2)
#Define the function
printplot<- function(data, attr, year){
#Subset all the data from the database using the arguments provided by the user in the web page.
predata<-subset(data, attribute==attr & season==year)
#Check if there is data for the selected values. If the result of subset is a empty df (nrow=0) stop the execution and print a message.
if (nrow(predata) == 0) {
  stop("No data for your selection, please choose other values.")
  }
#Transform data to numeric since the phenotype.value field of chado is set to character
predata$value<-as.numeric(predata$value)
#Get the mean values grouping by stock, season and attributes.
dataplot<-aggregate(predata$value, by= list(predata$stock, predata$season, predata$attribute),
                mean, na.rm= T, simplify = T)
#Get the standard deviation values grouping by stock, season and attributes.
sd<-aggregate(predata$value, by= list(predata$stock, predata$season, predata$attribute),
              sd, na.rm= T, simplify = T)
#Add the sd values to the 'dataplot' df where the mean values are keeped. 
dataplot$sd<-sd$x
#Change the names of the 'dataplot' df columns since aggregate function doesn't add the proper names
colnames(dataplot)<-c("stock", "season", "attribute", "mean", "sd")
#Bar plot
ggplot(data=dataplot, aes(x=stock, y=mean)) + geom_bar(stat="identity") + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin= mean - sd, ymax=mean + sd),
              side=.3,
              width=.2,                    # Width of the error bars
              position=position_dodge(.9))
}
