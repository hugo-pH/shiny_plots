#Define the function for plotdb_2
#This function creates a grouped barplot using the provided stock names plus the control line which is coded in the inside the function.
#Two arguments are provided, the attribute name (character vector) and a list of stock names.
#The function retrive the data from the DB and obtains the mean and sd values for each stock. All the seasons are retrieved.


printplot<- function(data, stk){
  
  stk["control"]<-"chaendler"  
  
  #Auxiliary functions
  ## The aim of this function is to perform an statiscal analysis to test if there are significant differences between the values obtained in different seasons. 
  ##This function acts over a dataframe where there is only one stock. First it test homocedasticity with bartlett test. Second it applies an ANOVA or K.Wallis test depending on the P value obtained in bartlett test.
  ## Third, If the P value is > 0.05, it creates a list with two elements, the name of the performed analysis (anova or kruskal wallis) and the P value obtained, In this case, the list will be the final output of the function. 
  ## if the P value obtained is < 0.05 it applies a test to search for differences between groups (Tukey or Kruskal both from agricolae package), then it creates a list with 3 elements, again the name of the performed analysis (anova or kruskal wallis) and the P value obtained, the third element is the "$groups$trt" dataframe returned by the  HSD.test and the Kruskall functions from agricolae package.
  
  
  model<-function(data){
    options(contrasts=c("contr.sum","contr.poly")) 
    bart.res<-bartlett.test(value ~ as.factor(season), data=data)
    if (bart.res$p.value > 0.05){
      aov.res<-aov(value ~ season, data=data)
      sm<-summary(aov.res)
      pval=sm[[1]]$'Pr(>F)'[[1]]
      if(pval < 0.05){
        group.res<-HSD.test(aov.res, "season", group = T)  
        group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
        group.res$groups$stock<-rep(levels(as.factor(data$stock)))
        st.year<-list(analysis="anova", pval=pval, groups=group.res[["groups"]])
      }else{
        st.year<-list(analysis="anova", pval=pval)
      }
      
    }else{
      kt<-kruskal.test(value~as.factor(season), data = data)
      pval=kt[["p.value"]]
      if(pval < 0.05){
        group.res<-kruskal(y=data$value, trt=data$season, group=T, p.adj="bonferroni")
        group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
        group.res$groups$stock<-rep(unique(data$stock))
        st.year<-list(analysis="kruskal", pval=pval, 
                      groups=group.res[["groups"]])
        
      }else{
        st.year<-list(analysis="kruskal", pval=pval)
      }
    }
  }
  
  
  #stats, means, colours
  t.func<-function(data, h){
    if (h == T){
      t.res<-t.test(value~stock, data=data, paired=FALSE, var.equal=TRUE)
      
    }else{
      sep<-split(data, data[["stock"]])
      t.res<-wilcox.test(sep[[stk[[1]]]][["value"]], sep[[stk[[2]]]][["value"]], correct=TRUE)
    }
    if (t.res[["p.value"]] < 0.05){
      a=paste0(stk[[1]], "(different from control)")
      b=stk[[2]]
    }else{
      a=paste0(stk[[1]], "(not different from control)")
      b=stk[[2]]
    }
    data.frame(season=rep(unique(data$season), 2), pval=rep(t.res[["p.value"]], 2), 
               color=c(a,b), stock=c(stk[[1]], stk[[2]]))
  }
  
  
  
  
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
  
  
  func.season<-function(data){
    levels(as.factor(data$season))
  }
  
  season<-dlply(data, .(stock), func.season)  
  
  if (identical(season[[1]],season[[2]]) == FALSE){
    ##Grouped bar plot with sd and y-axis label
    p<-ggplot(dataplot, aes(season, mean, group=stock, fill=stock))  +
      scale_fill_manual(values=c("dodgerblue", "gold")) +
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                    side=.3,
                    width=.2, # Width of the error bars
                    position=position_dodge(.9)) +
      labs(y =  paste0(unique(dataplot$attribute), " ", "(", unique(dataplot$unit), ")"), 
           title="No statistical analysis possible")
    p
  }else{
    
    #Do model function for each stock
    st.year<-dlply(data, .(stock), model)
    
    options(contrasts=c("contr.sum","contr.poly")) 
    flig.res<-fligner.test(value ~ as.factor(stock), data=data)
    if (flig.res$p.value > 0.05){
      
      st.control<-dlply(data, .(as.factor(season)), t.func, h=TRUE)
    }else{
      st.control<-dlply(data, .(as.factor(season)), t.func, h=FALSE)
    }
    st.all<-rbindlist(st.control)
    
    dataplot<-merge(dataplot, st.all, by=c("season", "stock"))  
    
    
    ##Plot      
    p<-ggplot(dataplot, aes(season, mean, group=stock, fill=color))  +
      scale_fill_manual(values=c("darkorchid1", "darkolivegreen2", "orange1"))
    
    
    if (st.year[[(stk[[-2]])]][["pval"]] < 0.05){
      dataplot<-merge(dataplot, st.year[[stk[[-2]]]][["groups"]], by.x = c("season", "stock"), by.y=c("trt", "stock"), all=TRUE)
      p<-  p + geom_text(data=dataplot, aes(y= (1.1 * mean + sd), label = M), position = position_dodge(width=0.9))##grouped postion http://stackoverflow.com/questions/18518558/label-bar-plot-with-geom-text-in-ggplot
    }
    p <- p + geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                    side=.3,
                    width=.2, # Width of the error bars
                    position=position_dodge(.9)) +
      labs(y =  paste0(unique(dataplot$attribute), " ", "(", unique(dataplot$unit), ")")) 
    
  }
  p
}


