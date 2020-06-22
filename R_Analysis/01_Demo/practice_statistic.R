library(jsonlite)
#import csv file
demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
# import json file
demo_table2 <- fromJSON(txt='demo.json')

# Create a vector
x <- c(3, 3, 2, 2, 5, 5, 8, 8, 9)
# Select value #3 in the vector:
x[3]

# select the 3 third row of the column 'year' of demo_table
# demo_table is currently a data frame type
demo_table[3,"Year"]
demo_table[3,3]

# select the whole column:
demo_table$"Vehicle_Class"
# Then select the a single value of that column:
demo_table$"Vehicle_Class"[2]

# filter table by using []:
filter_table <- demo_table2[demo_table2$price > 10000,]

# filters by using subset(), easier to read:
# filter by price and drivetrain
filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) 

# get a random sample from a large vector:
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)

# get a random sample from a large dataframe:
  # 1. get a range from 1 to number of rows in the df --> this is index list
  # 2. get a sample from that index list, here we are getting 3 samples
  # 3. then extract those 3 rows matching the indexes
demo_table[sample(1:nrow(demo_table), 3),]

#add columns to original data frame using mutate():
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) 

#create summary table using group_by:
  # group by 'condition', and get the mean of odometer
  # summarize is similar to mutate()
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage = mean(odometer))

# create summary table with multiple columns
  # n() is to count rows
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage = mean(odometer), Maximum_Price = max(price), Num_Vehicles = n()) 


# Reshape data (pivot and unpivot...):
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)
  # unpivot table:
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:safety_rating)
  # or:
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

  # pivot table:
wide_table <- long_table %>% spread(key="Metric",value="Score")

  # check if the two table are exaclty the same?
  # if all.equals not TRUE, try sorting the columns table <- table[,order(colnames(table))]
all.equal(demo_table3, wide_table)


# GGPLOT

# built-in example dataset mpg
head(mpg)

# bar plots:
plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

# more plots:
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n()) #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot (same as geom_bar())

# change titles:
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") #plot bar plot with labels
# rotate x-title degree:
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

# with subset:
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
plt + geom_line() #line chart
  # scale x-axis to use only the available data points (ticks), and y-axis to limit within a range:
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels and scales

# scatter plot:
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels

# customize:
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

# more customizes:
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv, size = cty)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics

# box-plot:
plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

# more box-plots:
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees

# heatmap:
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels 

# more heatmaps:
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

# plot multiple layers:
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + #add boxplot
theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
geom_point() #overlay scatter plot on top  

# more layers:
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ)) # SD for standard deviation
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars, +/- SD

# facet a long-formatted table (aka, split the chart into multiple smaller charts for easier comparison)
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)

plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels





# STATISTICS:
# Test for normality:
  # Qualitative Test for Normality: visualize plots looking for the 'bell curve'
head(mtcars) # another built-in dataset
ggplot(mtcars,aes(x=wt)) + geom_density() #visualize distribution using density plot, by creating buckets of similar values and calculating the density for each bucket.

  # Quantitative Test for Normality: use the Shapiro-Wilk test
shapiro.test(mtcars$wt)   # if p-value >= 0.05, the dataset is considered normal distributed

# Another normality test e.g:
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) #import used car dataset
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

# Extract sample data:
sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

# One-sample t-test: compare the means of sample dataset vs. population dataset
  # Note that the population dataset is skewed on the right, we are using log10 transformation to make our mileage data more normal.
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) #compare sample versus population means
    # -> just care for the p-value for now. p = 0.818 > significant level (assuming significant level is 5%)
    # -> not have sufficient evidence to reject the null hypothesis, and we would state that the two means are statistically similar.

# Two-sample t-test: compare the means of 2 sample datasets
sample_table <- population_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50) #generate another 50 randomly sampled data points
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples
  # p=0.97 > 0.05 so fail to reject the null hypothesis, , and we would state that the two samples are statistically similar.

# Paired t-test: compare observations in one dataset with observations in another
mpg_data <- read.csv('mpg_modified.csv') #import dataset
mpg_1999 <- mpg_data %>% filter(year==1999) #select only data points where the year is 1999
mpg_2008 <- mpg_data %>% filter(year==2008) #select only data points where the year is 2008
t.test(mpg_1999$hwy, mpg_2008$hwy, paired = T) #compare the mean difference between two samples
  # The p-value is above the assumed significance level.
  # Therefore, we would state that there is not enough evidence to reject the null hypothesis
  # and there is no overall difference in fuel efficiency between vehicles manufactured in 1999 versus 2008.

# ANOVA test: (phuong trinh 1 an)
mtcars_filt <- mtcars[,c("hp","cyl")] #filter columns from mtcars dataset
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor
  # pure aov()
aov(hp ~ cyl,data = mtcars_filt) #compare means across multiple levels
  # retrieve p-value
summary(aov(hp ~ cyl,data=mtcars_filt))
  # "Pr(>F)" is p-value = 1.32 * 10^-8 << significant level
  # reject the null hypothesis, there is a significant difference in horsepower between at least one engine type and the others.

# correlation:
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot
cor(mtcars$hp, mtcars$qsec) #calculate correlation coefficient
  # r=-0.7, so there is a strong negative correlation between the horsepower and quarter-mile time 
# another e.g:
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) #read in dataset
head(used_cars)
plt <- ggplot(used_cars,aes(x= Miles_Driven,y=Selling_Price)) #import dataset into ggplot2
plt + geom_point() #create a scatter plot
cor(used_cars$Miles_Driven, used_cars$Selling_Price) #calculate correlation coefficient
  # r = 0.02, which means that there is a negligible correlation between miles driven and selling price in this dataset.

# Correlaion matrix:
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)

# Linear regression (mtcars):
lm(qsec ~ hp, mtcars) #create linear model, return a and b
summary(lm(qsec~hp,mtcars)) #summarize linear model
  # r-squared = 0.5 (r=0.7). so 50% of all quarter mile time predictions will be correct
  # p-value <0.05, so reject our null hypothesis, which means that the slope of our linear model is not zero.
  # now visualize:
model <- lm(qsec ~ hp,mtcars) #create linear model
yvals <-  model$coefficients['hp']*mtcars$hp +
          model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(mtcars,aes(x = hp,y = qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y = yvals), color = "red") #plot scatter and linear model

# Multiple linear regression:
lm(qsec ~ mpg + disp + drat + wt + hp, data=mtcars) #generate multiple linear regression model
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics
  # Pr(>|t|) is p-value. If p-value is < significant level, then reject the null, which means there is a slope 
    # and have a significant impact on quarter-mile race time
  # When an intercept is statistically significant (< 0.05, so also reject the null, means there is a significant impact),
    # it means there are other variables and factors that contribute to the variation in quarter-mile time that have not been included in our model.
  # R-squared significantly increased from the single linear

# Chi-squared:
tbl <- table(mpg$class, mpg$year) #generate contingency table (aka a frequency table)
chisq.test(tbl) #compare categorical distributions
  # p > 0.05: not enough evidence to reject the null hypothesis, and there is no difference in the distribution of vehicle class across 1999 and 2008 from the mpg dataset.
