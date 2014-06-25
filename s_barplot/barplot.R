##barplot.R, plotdb

#Define the function
printplot<- function(data){

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



#Define basic plots
#Plot for values with significant differences. Define colours
plot_sig<-p <- ggplot(data=dataplot, aes(x=stock, y=mean)) +
               scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), guide=FALSE)

#Plot for values without significant differences,use only one colour
plot_no<- p <- ggplot(data=dataplot, aes(x=stock, y=mean)) +
               geom_bar(stat="identity", fill="#009E73") 

#Stats: test for homocedasticity with levene test, if data variance is homogeneous (p.value > 0.05), perform and ANOVA, else perform a Kruskall test. 
options(contrasts=c("contr.sum","contr.poly")) 
lev.res<-leveneTest(value ~ as.factor(stock), data=data)
lev.pval<-lev.res[["Pr(>F)"]][[1]] #Here is stored the pvalue in levene test.
if (lev.pval > 0.05){ #If the p.value is > 0.05 an ANOVA will be performed,
  aov.res<-aov(value ~ stock, data)
  aov.sum<-summary(aov.res)
  aov.pval<-aov.sum[[1]]$'Pr(>F)'[[1]] #here is where the p.value is obtained in the summary(aov) object
  if(aov.pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a Tukey test will be performed using the HSD.test from agricolae package
    group.res<-HSD.test(aov.res, "stock", group = T)  
    #Remove blank spaces at the end of the stock name string.  
    group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
    #Merge the dataplot df with the group.res$groups df obtained in the HSD.test. In this way, the letters for each group are appended to the original dataplot df as a new column named 'M'. The merge is possible beacause of the 'groups$trt' column, which contains the stock names.
    dataplot<-merge(dataplot, group.res$groups, by.x = "stock", by.y="trt")
    #When ANOVA p.value < 0.05, the bar colours are mapped to the M column (groups from HSD.test), therefore different colours means significant differences.
    p <- plot_sig + geom_bar(data=dataplot, aes(fill=M), stat="identity") +
                    geom_text(data=dataplot, aes(x=stock, y=(1.1 * mean + sd), label = M)) 
      }else{ #If the ANOVA pvalue > 0.05, use the plot_no, the same colour for all bars
   p <- plot_no
  }
}else{##When levene test pvalue < 0.05, kruskal.test will be performed. 
  kw.res<-kruskal.test(value~as.factor(stock), data = data)
  if(kw.res[["p.value"]] < 0.05){ #If the p.value obtained is < 0.05 a new kruskal test from the agricolae package will be performed.
    group.res<-kruskal(y=data$value, trt=data$stock, group=T, p.adj="bonferroni")
    #Remove blank spaces at the end of the stock name string.  
    group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
    #Merge the dataplot df with the group.res$groups df obtained in the kruskal test. In this way, the letters for each group are appended to the original dataplot df as a new column named 'M'. The merge is possible beacause of the 'groups$trt' column, which contains the stock names.
    dataplot<-merge(dataplot, group.res$groups, by.x = "stock", by.y="trt")
    #When kruskal.test p.value < 0.05, the bar colours are mapped to the M column (groups from 'kruskal' agricolae function), therefore different colours means significant differences.
    p <- plot_sig + geom_bar(data=dataplot, aes(fill=M), stat="identity") +
      geom_text(data=dataplot, aes(x=stock, y=(1.1 * mean + sd), label = M)) 
  }else{ #If the kruskal.test pvalue > 0.05, use the plot_no, the same colour for all bars
    p <- plot_no
  }
}

p <- p  +   geom_errorbar(aes(ymin= mean - sd, ymax=mean + sd),
                side=.3,
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
            labs(x ="stock", y = paste0(unique(data$attribute), " ", "(", unique(dataplot$unit), ")")) + # axis names, for y axis, paste attribute name + (units).
            theme(axis.text.x=element_text(face="bold", angle=60, hjust=1,  size=12)) # Turn 60º the stock names within the x axis. 


print(p)

}