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
