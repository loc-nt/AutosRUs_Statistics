library(jsonlite)
library(tidyverse)

# get data:
mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
coil_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

# rename columns to remove spaces
names(mpg_table)<-str_replace_all(names(mpg_table), c(" " = "_"))
head(coil_table)

# MPG Regression
mpg_model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mpg_table) #generate multiple linear regression model
mpg_model #show the model
summary(mpg_model) #generate summary statistics

# Suspension Coil Summary
# create a summary statistics table for the suspension coil's pounds-per-inch
summary(coil_table$PSI) # mean and median
sd(coil_table$PSI) # standard deviation
var(coil_table$PSI) # variance

# Suspension Coil T-Test
  # test for normality
plt <- ggplot(coil_table, aes(x = PSI))
plt + geom_density() #visualize distribution using density plot --> not skew
shapiro.test(coil_table$PSI) # not approximate normal distribution

# One-sample t-test: compare the means of sample dataset vs. population dataset
  # Even though the input is not normally distributed, let's just assume that it is and try doing the t-test
  # NOT skew, so no need to transform the data, assuming we are doing 2-side t-test
population_mean_PSI = 1500
t.test(coil_table$PSI, mu = population_mean_PSI)
