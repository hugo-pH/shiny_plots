#Define the functions for gr_barplot

############################
### season.test function ###
############################

## The aim of this function is to perform an statiscal analysis to test if there are significant 
#differences among the attribute values obtained in different SEASONS for the same stock. 
##The input is a dataframe which contains at least, three columns, named 'stock', 'season' and 'value'. 
#'Stock' column carries the name of the group. 'season' column contains the season or conditions and 'value' is the variable value. 
##This function acts over a dataframe where there is only one stock. 
##First, it tests homocedasticity with bartlett test. 
##Second, it applies an ANOVA or K.Wallis test depending on the P value obtained in bartlett test.
##Third, If the P value is > 0.05, it creates a list with two elements, the name of the performed analysis (anova or kruskal wallis) and the P value obtained.
## In this case, the list will be the final output of the function. 
## if the P value obtained is < 0.05 it applies a post-hoc test to search for differences between groups (Tukey or Kruskal both from agricolae package), 
## then it creates a list with 3 elements, again the name of the performed analysis (anova or kruskal wallis) and the P value obtained.
##The third element is the "$groups$trt" dataframe returned by the  HSD.test and the Kruskall functions from agricolae package.

  #Stats: test for homocedasticity with levene test, if data variance is homogeneous (p.value > 0.05), perform and ANOVA, else perform a Kruskall test. 
  #If the ANOVA or KW are significant (pvalue < 0.05), performs a post-hoc test to search for differences among groups (HSD.test or kruskall function, both from agricolae package)
  season.test <- function (data) {
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

###########################
### stock.test function ###
###########################

  #The stock.test function compares the values from one problem stock in one year with the same year of the control stock.
  #The arguments are a df and logical object, which indicates if the values are homocedastic or are not.
  #The dataframe must contain only two stock, the problem stock and the control stock. And values must come from a unique season. 
  #The output is a df with 4 columns, season, pval, color and stock. For each season there are two rows, one is the control stock and the other one the tested stock. 
  #Example:
  #  season         pval       color                            stock
  #   1   2004 7.627951e-09    pg29(different from control)     pg29
  #   2   2004 7.627951e-09    chaendler                        chaendler
  # The color column informs about the differences among the problem stock and the control stock.
    control.test <- function (h, data, stk) {
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

##########################
### printplot function ###
##########################

#This function creates a grouped barplot using the provided stock names plus the control line which is coded in the inside the function.
#Two arguments are provided,a dataframe containing the data and a list of stock names containing an unique stock.
printplot<- function(data, stk){
  
  ##Add control stock to stk list
  stk["control"]<-"chaendler"  
  
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
    lev.res<-leveneTest(value ~ as.factor(stock), data=data)
    lev.pval<-lev.res[["Pr(>F)"]][[1]] #Here is stored the pvalue in levene test.
    if (lev.pval > 0.05){#Define the value of 'h', which is used in the control.test function
      h=TRUE
      }else{
      h=FALSE
    }
    ##Apply the control.test function over the values of a season. 
    #Test if there are differences among problem and control stock in each season.
    #Return a list of df
    st.control<-dlply(data, .(as.factor(season)), control.test, h=h, stk=stk)
    ##Bind all the df(data.table package)
    st.all<-rbindlist(st.control)
    ##Merge 'dataplot' df with st.all. This adds the 'color' column which cotains the results of the analysis and will be used to set the colour in the barplot. 
    dataplot<-merge(dataplot, st.all, by=c("season", "stock"))  
    
    #Apply seasons.aov function for each stock. 
    #Returns a list of lists. Each list contains the results of the analysis performed over all the seasons on a individual stock
    st.year<-dlply(data, .(stock), season.test)
    
    ##Plot      
    ##x is mapped to season, y to mean values, groups of bars to stock and colour to 'color' column
    p<-ggplot(dataplot, aes(season, mean, group=stock, fill=color))  +
      scale_fill_manual(values=c("darkorchid1", "darkolivegreen2", "orange1")) #Define the colours used in the barplot
    
    ##Check if the season.test function has returned a pval < 0.05 for the problem stock. stk[[-2]] means the stock which is not the control. Control is always in position 2 because is appended to the stk list inside the function 
    if (st.year[[(stk[[-2]])]][["pval"]] < 0.05){
      ##if pval is < 0.05, merge 'dataplot' df with the df stored in the st.year$"stockname" list, which is the 'groups' df returned by 'HSD.test' and 'kruskall' functions (from agricolae)
      dataplot<-merge(dataplot, st.year[[stk[[-2]]]][["groups"]], by.x = c("season", "stock"), by.y=c("trt", "stock"), all=TRUE)
      ##Add labels to columns of problem stock. Labels are in dataplot$M and comes from 'groups' df merged previously. Labels are positioned above of error bar with (1.1 * mean +sd)
      p<-  p + geom_text(data=dataplot, aes(y= (1.1 * mean + sd), label = M), position = position_dodge(width=0.9))##grouped postion http://stackoverflow.com/questions/18518558/label-bar-plot-with-geom-text-in-ggplot
    }
    

    ##Define type of plot, and error bar and axis names    
    p <- p + geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                    side=.3,
                    width=.2, # Width of the error bars
                    position=position_dodge(.9)) +
      labs(y =  paste0(unique(dataplot$attribute), " ", "(", unique(dataplot$unit), ")")) # axis names, on y axis, paste attribute name + (unit).
    
  }
  p
}


