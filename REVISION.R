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
 

library(tidyverse)
library(boot)

data("iris")

iris_long <- pivot_longer(iris, cols = starts_with(c("Sepal", "Petal")), names_to = "variable", values_to = "value")

#OR

iris_long <- iris %>% pivot_longer(cols = -Species, names_to = "Temp", values_to = "Value") %>% 
  #separate(Temp, into = c("Part", "Measurement"))
  separate_wider_delim(Temp, ".", names = c("Parts", "Measurement"))




mean_func <- function(data, index) {
  mean(data[index])
}

mean_data <- iris_long %>%
  group_by(Species, Parts, Measurement) %>%
  summarise(mean_value = mean(Value),
            ci = boot(Value, mean_func, R = 1000)$t) %>%
  ungroup()

#alternate
mean_data <- iris_long %>%
  group_by(Species, Parts, Measurement) %>%
  reframe(mean_value = mean(Value),
            ci = boot(Value, mean_func, R = 1000)$t)


final_df <- mean_data %>%
  group_by(Species, Parts, Measurement) %>%
  summarise(mean_value = mean(mean_value),
            ci_lower = quantile(ci, 0.025),
            ci_upper = quantile(ci, 0.975))

p<- ggplot(final_df, aes(x = Species, y = mean_value, fill = Parts)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(. ~ Measurement, scales = "free_x", space = "free_x") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Species", y = "Mean ± 95% CI", title = "Mean Sepal and Petal Sizes with 95% CIs in Three Plant Species") +
  theme_minimal()

p + theme(panel.grid = element_blank())

#instead of using binwidth, for which, we need to know the distribution of the data, therefore, specifying the bin number is helpful.

a<- ggplot(iris_long, aes(x = Value)) +
  geom_histogram(binwidth = 0.1, color = "white", fill = "steelblue") +
  facet_grid(Species+Parts~Measurement) +
  labs(x = "Value", y = "count") +
  theme_bw()
a + theme(panel.grid = element_blank())


library(extrafont)
pd=position_dodge(0.2)




vlines <- iris_long(xintercept = c(-1, 1))
plot + geom_vline(data = vlines, aes(xintercept = xintercept), linetype = "dashed", color = "red")



#data should be in long format for ggplot visualisation.


#26-06-23 

#return from a function
res= c(ci_lower, mean_value, ci_upper) #????
return(res)


#example1
quantile (1:10)
#here, there are 5 values: 0%   25%   50%   75%  100% 
                        # 1.00  3.25  5.50  7.75 10.00 
#calling return (returning the data) from the function using [square bracket]
quantile(1:10)[1]

#example2 
a= summary(iris_long)
a[1]
a[[1]]
a[c(1:3)]
colnames(a) 

str(iris) #to find the data type of each column of the data frame
str(iris_long) 

#manipulating the order of the data by changing the data as a factor, and assigning levels
b=factor(a, levels=c( "setosa","versicolor","virginica"))

#reframe= same function as summary (but doesn't require un grouping)
?boot

#interpretation of the  histogram plot= to check the (only) the distribution/spread of data, and if there is a requirement for bootstrapping.
#inferring tests, treatments by plotting the mean, c.i, s.d. 















boot<-function(vec)
  mn=numeric(1000)
for (i in 1:1000)
  
{
  temp= sample(vec, replace= T)
  mn[i]=mean(temp)}
mean= median(mean_value)
lci= quantile (mn,0.025)
rci= quantile (mn,0.975)
mean (Data)
 

# Define the function for calculating the statistic of interest (mean in this case)
statistic <- function(mean_value, vec) {
  return(mean(mean_value[vec]))
}

# Perform bootstrap resampling
boot_results <- boot(data, statistic, R = 1000)  # R = number of bootstrap samples

# Calculate the bootstrap mean
bootstrap_mean <- boot_results$t0

# Calculate the bootstrap confidence intervals
bootstrap_ci <- boot.ci(boot_results, type = "basic")$basic
 

#bootstrapping and loop



p + theme(panel.grid = element_blank())





