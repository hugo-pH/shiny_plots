#Plot attribute
Plotdb is a shiny app which performs a statistical analysis over a dataset and represents it graphically. 
The user can select and attribute and a year which will be determined by the choosen attribute.  
The app checks the data normality and homocedasticity. Depending on the homocedasticity output an ANOVA or Kruskall Wallis test is performed. Finally, if these test were significant, a Tukey test or Kruskal Wallis test is performed with the aim of testing for differences among groups.  
The data is represented in a barplot, where bar colours are determined by the s

An example of the plot output:
![Alt text](../images/plotdb.png?raw=true)
