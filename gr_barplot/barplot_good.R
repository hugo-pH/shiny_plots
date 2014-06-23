#Define the function for plotdb_2
#This function creates a grouped barplot using the provided stock names plus the control line which is coded in the inside the function.
#Two arguments are provided, the attribute name (character vector) and a list of stock names.
#The function retrive the data from the DB and obtains the mean and sd values for each stock. All the seasons are retrieved.


printplot<- function(data, stk){
  
  stk["control"]<-"chaendler"  
  
  
  ## The aim of this function is to perform an statiscal analysis to test if there are significant 
  ##differences between the attributes values obtained in different SEASONS for the same stock. 
  ##This function acts over a dataframe where there is only one stock. 
  ##First, it tests homocedasticity with bartlett test. 
  ##Second, it applies an ANOVA or K.Wallis test depending on the P value obtained in bartlett test.
  ##Third, If the P value is > 0.05, it creates a list with two elements, the name of the performed analysis (anova or kruskal wallis) and the P value obtained.
  ## In this case, the list will be the final output of the function. 
  ## if the P value obtained is < 0.05 it applies a post-hoc test to search for differences between groups (Tukey or Kruskal both from agricolae package), 
  ## then it creates a list with 3 elements, again the name of the performed analysis (anova or kruskal wallis) and the P value obtained.
  ##The third element is the "$groups$trt" dataframe returned by the  HSD.test and the Kruskall functions from agricolae package.
  seasons.aov<-function(data){
    #Stats: test for homocedasticity with levene test, if data variance is homogeneous (p.value > 0.05), perform and ANOVA, else perform a Kruskall test. 
    #If the ANOVA or KW are significant (pvalue < 0.05), performs a post-hoc test to search for differences among groups
    options(contrasts=c("contr.sum","contr.poly")) 
    lev.res<-leveneTest(value ~ as.factor(season), data=data)
    lev.pval<-lev.res[["Pr(>F)"]][[1]] #Here is stored the pvalue in levene test.
    if (lev.pval > 0.05){#If the p.value is > 0.05 an ANOVA is performed
      aov.res<-aov(value ~ season, data=data)
      sm<-summary(aov.res)
      pval=sm[[1]]$'Pr(>F)'[[1]]  #get the pvalue from the summary(aov) object
      if(pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a Tukey test is performed using the HSD.test from agricolae package
        group.res<-HSD.test(aov.res, "season", group = T)  
        #Remove blank spaces at the end of the season string
        group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
        ##Adds a new column to the 'groups' df with the name of the stock over which the test was performed. 
        group.res$groups$stock<-rep(levels(as.factor(data$stock)))
        #Creates the final output, a list with the name of the analysis, the p.value obtained and the 'groups' df from the HSD.test
        st.year<-list(analysis="anova", pval=pval, groups=group.res[["groups"]])
      }else{#If the P value is > 0.05, it creates a list with two elements, the name of the performed analysis (anova) and the P value obtained.
        st.year<-list(analysis="anova", pval=pval)
      }
      
    }else{#When levene test pvalue < 0.05, kruskal.test is performed. 
      kt<-kruskal.test(value~as.factor(season), data = data)
      pval=kt[["p.value"]]
      if(pval < 0.05){ # If the ANOVA returns a p.value < 0.05 a new Kruskal-Wallis test is performed using the 'kruskal' function from agricolae package
        group.res<-kruskal(y=data$value, trt=data$season, group=T, p.adj="bonferroni")
        #Remove blank spaces at the end of the season string
        group.res$groups$trt<-gsub(" ", "", group.res$groups$trt, fixed=T)
        ##Adds a new column to the 'groups' df with the name of the stock over which the test was performed. 
        group.res$groups$stock<-rep(unique(data$stock))
        #Creates the final output, a list with the name of the analysis, the p.value obtained and the 'groups' df from 'kruskal' function
        st.year<-list(analysis="kruskal", pval=pval, 
                      groups=group.res[["groups"]])
        
      }else{#If the P value is > 0.05, it creates a list with two elements, the name of the performed analysis (kruskal) and the P value obtained.
        st.year<-list(analysis="kruskal", pval=pval)
      }
    }
  }
  
  #The stock.test function compares the values from one stock in one year with the same year of the control stock.
  #The arguments are a df and logical object, which indicates if the values are homocedastic or are not.
  #The dataframe must contain only two stock, the problem stock and the control stock. And values must come from a unique season. 
  #The output is a df with 4 columns, season, pval, color and stock. For each season there are two rows, one is the control stock and the other one the tested stock. 
  #Example:
  #  season         pval       color                            stock
  #   1   2004 7.627951e-09    pg29(different from control)     pg29
  #   2   2004 7.627951e-09    chaendler                        chaendler
  # The color column informs about the differences among the problem stock and the control stock.
  stock.test<-function(data, h){
    if (h == T){ #if values are homocedastic, performs a t.test
      res<-t.test(value~stock, data=data, paired=FALSE, var.equal=TRUE)
      
    }else{ #else, perfoms a wilcox.test
      stk.split<-split(data, data[["stock"]]) # Create a list spliting the original data. The list contains two df, one for each stock (control and problem).
      res<-wilcox.test(stk.split[[stk[[1]]]][["value"]], stk.split[[stk[[2]]]][["value"]], correct=TRUE)#Wilcox test over the values of both stock.
    }
    if (res[["p.value"]] < 0.05){#if the test is significant, creates an object containing the name of the stock and "(different from control)"
      a=paste0(stk[[1]], "(different from control)")
      b=stk[[2]]
    }else{#else, creates an object containing the name of the stock and "(not different from control)"
      a=paste0(stk[[1]], "(not different from control)")
      b=stk[[2]]
    }
    #Creates the final output, a dataframe with 4 columns and two rows. The first row contains the data from the problem stock, which are stk[[1]] and the 'a' object. The season and the pvalue are repeated in both rows.   
    data.frame(season=rep(unique(data$season), 2), pval=rep(res[["p.value"]], 2), 
               color=c(a,b), stock=c(stk[[1]], stk[[2]]))
  }
  
  
  
  ### Prepare the data for ggplot ###
  
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
  
  
  ## Using plyr, get the levels of seasons for each stock and store it in a list. 
  func.season<-function(data){
    levels(as.factor(data$season))
  }
  season<-dlply(data, .(stock), func.season)  
  
  
  if (identical(season[[1]],season[[2]]) == FALSE){
    ##If the problem stock has data along a different number of seasons than control stock
    # no statistical analysis is performed
    
    ##Grouped bar plot with sd and y-axis label
    # Colours are mapped to stock names
    # Title indicates the absence of statistical analysis. 
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
    #If the problem stock has data along the same seasons than control stock, the analysis is performed
    

    
    options(contrasts=c("contr.sum","contr.poly")) 
    flig.res<-fligner.test(value ~ as.factor(stock), data=data)
#     lev.res<-leveneTest(value ~ as.factor(stock), data=data)
#     lev.pval<-lev.res[["Pr(>F)"]][[1]] #Here is stored the pvalue in levene test.
    if (flig.res$p.value > 0.05){
      h=TRUE
#       st.control<-dlply(data, .(as.factor(season)), stock.test, h=TRUE)
    }else{
      h=FALSE
    }
    st.control<-dlply(data, .(as.factor(season)), stock.test, h=h)
    st.all<-rbindlist(st.control)
    
    dataplot<-merge(dataplot, st.all, by=c("season", "stock"))  
    
#Apply seasons.aov function for each stock. 
#Returns a list of lists. Each list contains the results of the analysis performed over all the seasons on a individual stock
st.year<-dlply(data, .(stock), seasons.aov)
    
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


