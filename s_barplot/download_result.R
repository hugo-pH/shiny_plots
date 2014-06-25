download<-function(data){
predata<-data
# Get the mean values grouping by stock, season and attributes.
dataplot<-aggregate(predata$value, by= list(predata$stock, predata$season, 
                                            predata$attribute, predata$unit),
                    mean, na.rm= T, simplify = T)
#Get the standard deviation values grouping by stock, season and attributes.
sd<-aggregate(predata$value, by= list(predata$stock, predata$season, 
                                      predata$attribute, predata$unit),
              sd, na.rm= T, simplify = T)
#Add the sd values to the 'dataplot' df where the mean values are keeped. 
dataplot$sd<-sd$x
#Change the names of the 'dataplot' df columns since aggregate function doesn't add the proper names
colnames(dataplot)<-c("stock", "season", "attribute", "unit",  "mean", "sd")

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
      }else{ #If the ANOVA pvalue > 0.05, return the original object
    dataplot
  }
}else{##When levene test pvalue < 0.05, kruskal.test will be performed. 
  kw.res<-kruskal.test(value~as.factor(stock), data = data)
  if(kw.res[["p.value"]] < 0.05){ #If the p.value obtained is < 0.05 a new kruskal test from the agricolae package will be performed.
    group.res<-kruskal(y=data$value, trt=data$stock, group=T, p.adj="bonferroni")
    #Remove blank spaces at the end of the stock name string.  
    group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
    #Merge the dataplot df with the group.res$groups df obtained in the kruskal test. In this way, the letters for each group are appended to the original dataplot df as a new column named 'M'. The merge is possible beacause of the 'groups$trt' column, which contains the stock names.
    dataplot<-merge(dataplot, group.res$groups, by.x = "stock", by.y="trt")
  }else{ #If the kruskal.test pvalue > 0.05, return the original object
    dataplot
  }
}


return(dataplot)
}