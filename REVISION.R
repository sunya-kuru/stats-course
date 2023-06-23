#DATE - 23-06-2023
library(tidyverse)
data(iris)
df_long<- pivot_longer (iris, cols= c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width"), names_to = "measurement", values_to = "value")
df_final <- df_long %>% separate(measurement, c('partofflower', 'measure'))
#NOTES:
#difference between standard error, standard deviation, confidence interval (what type of insights do these provide?)
#Box Plots and Standard Deviation shows the variation in the sample data collected,i.e., how far the sample data is from the sample mean.
#Whereas, the Standard Error shows the variation of the data from the population mean, or the uncertainty in the measurement of the population mean.
#S.E can be measured? by boot strapping the sample data (Gausssian distribution is an exceptional case where the S.D. and S.E. scaling is possible without bootstrapping because the data is normally distributed)
#ECOLOGICAL data are not normally distributed most of the time, therefore, requires boot strapping (with replacements)?
#bootstrapping with replacements gives us multiple means, and then we can find the C.I. of the means as the representative population mean.
#Bootstrapping/Loops
#empty vector
mn=numeric(1000)
#filling this with bootstrapped measure of the "sample mean with replacements in the sample data"
for (i in 1:1000)
{temp= sample(mn, replace= T)
mn[i]=mean(temp)}
#example:
Data= iris$Sepal.Length
mn=numeric(1000)
for (i in 1:1000)
  
{
  temp= sample(Data, replace= T)
  mn[i]=mean(temp)}
mean= median(mn)
lci= quantile (mn,0.025)
rci= quantile (mn,0.975)
mean (Data)
#compare the bootstrapped mean= median(mn) value with the population mean (Data)
mn=numeric(100)

 
#for later
 df2 <- df_final%>%
  add_column(add_column = "lci")
 
 
 df_wide<- pivot_wider (df_long, cols= c("setosa", "virginica","versicolor"), names_from  = "Species")
 
 

