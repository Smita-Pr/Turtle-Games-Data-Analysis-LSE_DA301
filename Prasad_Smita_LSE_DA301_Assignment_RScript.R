## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## Turtle Games is a game manufacturer and retailer. 
## They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
## performance by utilizing customer trends. 

## In particular, Turtle Games wants to understand:
## - what is the impact on sales per product 
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales

################################################################################

# EDA using R

## The sales department of Turtle games prefers R to Python. 

###############################################################################
#set working directory to LSE_DA301_assignment_files

# 1. Load and explore the data

# Install and import Tidyverse.
#it consists of :
# ggplot2, tibble, tidyr, readr, purrr, dplyr, stringr, and forcats. 
# use install.packages('tidyverse') if not installed
library(dplyr)
library(tidyverse)

# Import the data set from the following file -: 
# the turtle_sales.csv file 

####################
### Columns         
# Ranking        World ranking of the game.
# Product       Unique code allocated to the product based on the item 
                #description.
#Platform      The video game console on which the game was launched.
#Year                 The year the game was first released.
#Genre                 The genre of the video game.
#Publisher           The company that published the game.
#NA_Sales        The number of games sold in North America using pounds and 
                  #displayed in millions.
#EU_Sales    The number of games sold in Europe using pounds and displayed
              #in millions.
#Global_Sales  Total sales in the world
########################
sales <- read.csv("turtle_sales.csv")

# View the data frame.
View(sales)

#check for missing data
sum(is.na(sales))

# There are two missing values
sales_na_rows <- sales[rowSums(is.na(sales)) > 0,]

#View the na rows
sales_na_rows

# The year values for these two rows are missing
# we can ignore the na values in the year column for now

# Explore the data set
# View the descriptive statistics.
summary(sales)
str(sales)

# Create a Data Profiling report using the Data explorer report

library(DataExplorer)
create_report(sales)


# Create a new subset of the sales df keeping the sales and relevant columns only
df_sales_trunc <- select(sales,!c("Ranking", "Year", "Genre", "Publisher"))
colnames(df_sales_trunc)

# save the file 
write.csv(df_sales_trunc,file="sales_trunc.csv",row.names = FALSE)
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
# Set the data source, add mapping elements.
# Use the columns NA_sales and EU_Sales 
ggplot (data = df_sales_trunc, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = NA_Sales, y = EU_Sales)) +
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  labs(title = "Scatter Plot Sales (in Million Pounds) - Europe Vs North America",
       x="Sales in North America",
       y="Sales in Europe")+
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm',se=FALSE)+ 
  # use minimal theme
   theme_minimal()

# Set the data source, add mapping elements.
# Use the columns NA_sales and Global_Sales 
ggplot (data = df_sales_trunc, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = NA_Sales, y = Global_Sales)) +
  labs(title = "Scatter Plot Sales (in Million Pounds) - North America Vs Global ",
       x="Sales in North America",
       y="Total Sales Globally")+
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')+
  # use minimal theme
  theme_minimal()

# Set the data source, add mapping elements.
# Use the columns EU_sales and Global_Sales 
ggplot (data = df_sales_trunc, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x = EU_Sales, y = Global_Sales)) +
  labs(title = "Scatter Plot Sales (in Million Pounds) - Europe Vs Global",
       x="Sales in Europe",
       y="Total Sales Globally")+
  # Add a geom as point for scatterplot.
  # Set the colour to red.
  geom_point(color = 'red',
             # Set the alpha transparency to 0.5.
             alpha = 0.5,  
             # Set the point size to 1.5.
             size = 1.5) +
  # Add the line-of-best-fit to the plot.
  geom_smooth(method = 'lm')+
  # use minimal theme
  theme_minimal()


# Combined view of scatter plots for all three Sales columns
pairs(df_sales_trunc[,3:5],lower.panel = NULL)

####### use the original sales data set to see examine sales by Platform
# Bar Plots

# Sales in Europe by Platform
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Platform,y= EU_Sales)) +
        # Add a bar for barplot
        # Set the colour to blue
        #set the title
        labs(title = "Sales in Europe by Platform",y="Sales in Europe (Million Pounds)")+
        geom_bar(stat="identity",width=0.5,fill="blue") +
        coord_flip()+
        theme_minimal()

#Sales in North America by Platform
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Platform,y= NA_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Sales in North America by Platform",y="Sales in North America (Million Pounds)")+
  geom_bar(stat="identity",width=0.5,fill="blue") +
  coord_flip()+
  theme_minimal()

#Sales Globally by Platform
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Platform,y= Global_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Total Sales Globally by Platform",y="Sales Globally (Million Pounds)")+
  geom_bar(stat="identity",width=0.5,fill="blue") +
  coord_flip()+
  theme_minimal()

### By Genre

#Sales Globally by Genre
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Genre,y= Global_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Total Sales Globally by Genre",y="Sales Globally (Million Pounds)")+
  geom_bar(stat="identity",fill="blue",width=0.5) +
  coord_flip()+
  theme_minimal()

#Sales in NA by Genre
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Genre,y= NA_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Total Sales in North America by Genre",y="Sales Globally (Million Pounds)")+
  geom_bar(stat="identity",fill="blue",width=0.5) +
  coord_flip()+
  theme_minimal()

#Sales in EU by Genre
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Genre,y= EU_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Total Sales in Europe by Genre",y="Sales Globally (Million Pounds)")+
  geom_bar(stat="identity",fill="blue",width=0.5) +
  coord_flip()+
  theme_minimal()

# Sales Globally by Publisher
ggplot (data = sales, 
        # Add mapping elements.
        # Insert a + to add the geom.
        mapping=aes(x= Publisher,y= Global_Sales)) +
  # Add a bar for barplot
  # Set the colour to blue
  #set the title
  labs(title = "Total Sales Worldwide by Publisher",y="Sales Globally (Million Pounds)")+
  geom_bar(stat="identity",fill="blue",width=0.5) +
  coord_flip()+
  theme_minimal()


## 2b) Histograms
# Create histograms.
#sales North America histogram
ggplot(sales,aes(x=NA_Sales))+geom_histogram(fill="blue")+
    theme_minimal()
#sales Europe histogram
ggplot(sales,aes(x=EU_Sales))+geom_histogram(fill="blue")+
  theme_minimal()
#sales Global histogram
ggplot(sales,aes(x=Global_Sales))+geom_histogram(fill="blue")+
  theme_minimal()

## 2c) Boxplots
# Create boxplots for Global Sales
ggplot(sales, aes(x=Genre,y = Global_Sales)) +
  # Specify the geom_boxplot function.
  labs(title="Distribution of Global Sales For Different Genres",
       y="Total Sales in Million Pounds")+
  geom_boxplot(fill="blue",outlier.colour = "red")+theme_minimal()


# Create boxplots for NA sales

ggplot(sales, aes(x=Genre,y = NA_Sales)) +
  # Specify the geom_boxplot function.
  labs(title="Distribution of Sales in North America For Different Genres",
       y="Total Sales in Million Pounds")+
  geom_boxplot(fill="blue",outlier.colour = "red")+theme_minimal()


# Create boxplots for EU sales
ggplot(sales, aes(x=Genre,y = EU_Sales)) +
  # Specify the geom_boxplot function.
  labs(title="Distribution of Sales in Europe For Different Genres",
       y="Total Sales in Million Pounds")+
  geom_boxplot(fill="blue",outlier.colour = "red")+theme_minimal()

###############################################################################

# 3. Observations and insights

##  observations and insights ......

# The distribution of the Sales data -:

### North America Sales Stats ####
#   Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0000  0.4775  1.8200  2.5160  3.1250 34.0200 


###  EU Sales Stats #####
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max. 
# 0.000   0.390   1.170   1.644   2.160  23.800 

#### Global Sales Stat  #####
#Min. 1st Qu.  Median    Mean    3rd Qu.  Max. 
#0.01    1.12    4.32    5.33    6.43   67.85 


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. 
################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(sales)
colnames(sales)
# Check output: Determine the min, max, and mean values.
sales_stats <- summarise(sales,
                           max_na=max(sales$NA_Sales),
                           max_global=max(sales$Global_Sales),
                           max_eu=max(sales$EU_Sales),
                           min_na=min(sales$NA_Sales),
                           min_global=min(sales$Global_Sales),
                           min_eu=min(sales$EU_Sales),
                           Mean_NA=mean(sales$NA_Sales),
                           Mean_global=mean(sales$Global_Sales),
                           Mean_EU=mean(sales$EU_Sales))

# View the df with summary statistics.
sales_stats

# alternate method to see the summary statistics of sales data
library(skimr)
skim(sales)


# View Unique values in the categorical columns
unique(sales$Platform) #22 Platforms
unique(sales$Genre) #12 Genres
unique(sales$Publisher) #24 Publishers


range(sales$Ranking) # rankings range : 1 to 16096

#convert Product and Ranking to Categorical Variables

sales$Ranking <- as.factor(sales$Ranking)
sales$Product <- as.factor(sales$Product)

#view structure of sales
str(sales)
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.

# Exploring NA Sales 
###

#aggregate sales by Product
aggr_na_sales <- aggregate(NA_Sales~Product,sales,sum)

# View the data frame.
aggr_na_sales

# Explore the data frame.

# Top twenty products by sales in North America
top_20_prod_na <- head(arrange(aggr_na_sales,desc(NA_Sales)),20)

ggplot(top_20_prod_na,aes(Product,NA_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top 20 Products by Sales in North America",
      y="Sales in Million Pounds",
      x="Unique Product Code")+
  theme_minimal()

# aggregate sales by Genre
top_genres_na <- aggregate(NA_Sales~Genre,sales,sum)

ggplot(top_genres_na,aes(Genre,NA_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top Genres by Sales in North America",
       y="Sales in Million Pounds",
       x="Genre")+
  theme_minimal()


#####

# Exploring EU Sales 
####

#aggregate sales by Product
aggr_eu_sales <- aggregate(EU_Sales~Product,sales,sum)

# View the data frame.
aggr_eu_sales

# Explore the data frame.

# Top twenty products by sales in North America
top_20_prod_eu <- head(arrange(aggr_eu_sales,desc(EU_Sales)),20)

ggplot(top_20_prod_eu,aes(Product,EU_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top 20 Products by Sales in Europe",
       y="Sales in Million Pounds",
       x="Unique Product Code")+
  theme_minimal()

# aggregate sales by Genre
top_genres_EU <- aggregate(EU_Sales~Genre,sales,sum)

ggplot(top_genres_EU,aes(Genre,EU_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top Genres by Sales in Europe",
       y="Sales in Million Pounds",
       x="Genre")+
  theme_minimal()


####
# Exploring Global Sales 
#aggregate sales by Product
aggr_gb_sales <- aggregate(Global_Sales~Product,sales,sum)

# View the data frame.
aggr_gb_sales

# Explore the data frame.

# Top twenty products by sales in North America
top_20_prod_gb <- head(arrange(aggr_gb_sales,desc(Global_Sales)),20)

ggplot(top_20_prod_gb,aes(Product,Global_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top 20 Products by Sales Worldwide",
       y="Sales in Million Pounds",
       x="Unique Product Code")+
  theme_minimal()

# aggregate sales by Genre
top_genres_gb <- aggregate(Global_Sales~Genre,sales,sum)

ggplot(top_genres_gb,aes(Genre,Global_Sales))+
  geom_bar(stat="identity",width=0.5,fill="blue",color="black")+
  labs(title = "Top Genres by Sales Worldwide",
       y="Sales in Million Pounds",
       x="Genre")+
  theme_minimal()


###############################################################################
# Install and import Moments.
# use install.packages("moments") to install moments
library (moments)

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qqnorm(sales$NA_Sales)
qqline(sales$NA_Sales, col='red',lwd=1)
hist(sales$NA_Sales)

qqnorm(sales$Global_Sales)
qqline(sales$Global_Sales, col='red',lwd=1)
hist(sales$Global_Sales)

qqnorm(sales$EU_Sales)
qqline(sales$EU_Sales, col='red',lwd=1)
hist(sales$EU_Sales)

## 3b) Perform Shapiro-Wilk test

# Normality of NA Sales
shapiro.test(sales$NA_Sales)
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution. 
# Also evident using the qqnorm plots and the histogram

# Normality of EU Sales
shapiro.test(sales$EU_Sales)
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution. 
# Also evident using the qqnorm plots and the histogram

# Normality of Global Sales
shapiro.test(sales$Global_Sales)
# Our p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution. 
# Also evident using the qqnorm plots and the histogram



## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

#Skewness assesses the extent to which a variable’s distribution is symmetrical. 
#If the distribution of responses for a variable stretches toward the right 
#or left tail of the distribution, then the distribution is characterized as skewed. 
#A negative skewness indicates a greater number of larger values,
#whereas a positive skewness indicates a greater number of smaller values. 
#A skewness value between −1 and +1 is considered excellent,
#but a value between −2 and +2 is generally considered acceptable. 
#Values beyond −2 and +2 are considered indicative of substantial non-normality." 

skewness(sales$NA_Sales)
# 4.30921 indicates positive skewness
#In a positive skew, the outliers will be present on the right side of the curve
sd(sales$NA_Sales)
mean(sales$NA_Sales)
median(sales$NA_Sales)

skewness(sales$Global_Sales)
# 4.045582 indicates positive skewness
sd(sales$Global_Sales)
mean(sales$Global_Sales)
median(sales$Global_Sales)

skewness(sales$EU_Sales)
# 4.818688 indicates positive skewness
sd(sales$EU_Sales)
mean(sales$EU_Sales)
median(sales$EU_Sales)

## 3d) Determine correlation
# Determine correlation.
cor(sales$NA_Sales,sales$EU_Sales)
cor(sales$NA_Sales,sales$Global_Sales)
cor(sales$EU_Sales,sales$Global_Sales)
# as expected the NA and EU Sales columns are highly correlated to the Global Sales
# since global sales is comprised of EU and NA Sales.

# Install the psych package.
#install.packages('psych')

# Import the psych package.
library(psych)

# Use the corPlot() function to visualise the correlations
# Specify the data frame (sales) and select numeric columns 
# set character size (cex=2).

corPlot(select(sales,NA_Sales,Global_Sales ,EU_Sales),cex=2)

###############################################################################

# 4. Plot the data
#combine the top20 genre NA,EU and Global dataframes
top_twenty_genre <- cbind(top_genres_EU,top_genres_na,top_genres_gb)

#drop dupllicate columns
top_twenty_genre <- top_twenty_genre %>% select(c(1,2,4,6))

#melt to long form
top_twenty_genre <- melt(top_twenty_genre,id.var="Genre")

#rename columns to variable = Region and value = Sales
top_twenty_genre <- top_twenty_genre %>% rename(Region=variable,Sales=value)

#view the df
top_twenty_genre

ggplot(top_twenty_genre,aes(x=Genre,y=Sales,fill=Region))+
  geom_bar(stat="identity",position="dodge")+
  labs(title = "Sales by Genre",
       y="Sales in Million Pounds",
       x="Genre")+
  theme_minimal()


###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The NA_Sales,EU_Sales and Global_Sales data is not normally distributed 
# and as expected they are highly correlated to each other 
# Since the EU and NA Sales data contribute to the overall Global Sales.

# On aggregating we see that the product 107 sells the highest 
# product 107
filter(sales,sales$Product==107)
# world ranking for it is 1.
#Ranking Product Platform Year  Genre Publisher NA_Sales EU_Sales Global_Sales
#1       1     107      Wii 2006 Sports  Nintendo    34.02     23.8        67.85

# total sales percentage worldwide
round(max(sales$Global_Sales)/sum(sales$Global_Sales),3)*100
# It accounts for 3.6 % of total Global Sales


###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. 
# Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.


###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
df_sales_trunc <- read.csv("sales_trunc.csv",header=T)
View(df_sales_trunc)
# Determine a summary of the data frame.
summary(df_sales_trunc)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(df_sales_trunc$NA_Sales,sales$EU_Sales)
cor(df_sales_trunc$NA_Sales,sales$Global_Sales)
cor(df_sales_trunc$EU_Sales,sales$Global_Sales)
# as expected the NA and EU Sales columns are highly correlated to the Global Sales
# since global sales is comprised of EU and NA Sales.

# using library(psych) loaded previously 

# Use the corPlot() function to visualise the correlations
# Specify the data frame (df_sales_trunc) and select numeric columns 
# set character size (cex=2).

corPlot(select(df_sales_trunc,NA_Sales,Global_Sales ,EU_Sales),cex=2)



# Create a linear regression model on the original data.ie.sales

#simple regression model using Global_Sales as y
# and NA_Sales as x or independent variable
lm_na_gb <- lm(Global_Sales~NA_Sales,data = sales)
summary(lm_na_gb)
#Adjusted R-squared:  0.8738 
#simple regression model using Global_Sales as y
# and EU_Sales as x or independent variable
lm_eu_gb <- lm(Global_Sales~EU_Sales,data = sales)
summary(lm_eu_gb)
#Adjusted R-squared:  0.7695 
## 2b) Create a plot (simple linear regression)
# Basic visualisation.
plot(sales$NA_Sales,sales$Global_Sales)
abline(lm_na_gb,col="red")

plot(sales$EU_Sales,sales$Global_Sales)
abline(lm_eu_gb,col="red")
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
str(sales)
#convert YEar to Date
sales_numeric_data <- select_if(sales,is.numeric)
#remove YEar Column
sales_numeric_data <- select(sales_numeric_data,!c("Year"))
colnames(sales_numeric_data)
# Multiple linear regression model.
lm_model <- lm(Global_Sales~NA_Sales+EU_Sales,data=sales_numeric_data)
summary(lm_model)
#result
#Residual standard error: 1.112 on 349 degrees of freedom
#Multiple R-squared:  0.9687,	Adjusted R-squared:  0.9685 
#F-statistic:  5398 on 2 and 349 DF,  p-value: < 2.2e-16

qqnorm(residuals(lm_model))
qqline(residuals(lm_model), col='blue')
shapiro.test(residuals(lm_model))
## The p-value from the test is <0.05, and we can conclude that the sample data is not 
# normally distribution.
# The residuals are not normally distributed
# The model In order to make valid inferences from regression  
# the residuals of the regression should follow a normal distribution. 
# model may not explain all trends

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Observed values provided
#NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
#NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
#NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
#NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
#NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.

#store observed values in a data frame
NA_Sales_obs <- c(34.02,3.93,2.73,2.26,22.08)
EU_Sales_obs <- c(23.80,1.56,0.65,0.97,0.52)

Sales_obsv <- data.frame(NA_Sales=NA_Sales_obs,EU_Sales=EU_Sales_obs)

str(Sales_obsv)
#predict using observed values
predictGlobal_Sales <- predict(lm_model,
                               newdata=Sales_obsv,
                               interval="confidence")

# View the predicted values
predictGlobal_Sales

# use the metrics package to evaluate the model
# filter actual Global_Sales values for comparison with predicted values
ObsvGlobal_Sales <- filter(df_sales_trunc,NA_Sales %in% NA_Sales_obs,EU_Sales %in% EU_Sales_obs)
ObsvGlobal_Sales <- select(ObsvGlobal_Sales,c("NA_Sales","EU_Sales","Global_Sales"))

#reorder to match NA_Sales and EU_Sales
ObsvGlobal_Sales <- ObsvGlobal_Sales[order(match(
  paste(ObsvGlobal_Sales[,1],ObsvGlobal_Sales[,2]),
  paste(Sales_obsv[,1],Sales_obsv[,2]))
),]

#view ObsvGlobal_Sales
ObsvGlobal_Sales

evaluate_model <- data.frame()
evaluate_model %>% summarise(R2 = cor(ObsvGlobal_Sales$Global_Sales, predictGlobal_Sales[,1])^2,
                          MSE = mean((ObsvGlobal_Sales$Global_Sales - predictGlobal_Sales[,1])^2),
                          RMSE = sqrt(MSE),
                          MAE = mean(abs(ObsvGlobal_Sales$Global_Sales - predictGlobal_Sales[,1])))

plot(ObsvGlobal_Sales$Global_Sales, predictGlobal_Sales[,1],col="blue")

#visualize the model

#install car package
#install.packages("car)
#load the library
library(car)
avPlots(lm_model)

# alternate way to visualize the model
plot(lm_model)



###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The multiple linear regression model yields better results overall than 
# the simple linear regression models
# significance of the explanatory variables in the coefficients table 
# shows that the explanatory variables are significant
# The intercept is 0.22175 , the coefficients of the variables are-:
# coeff of NA_Sales 1.15543 and coeff of EU_Sales is 1.34197.
# predictions are made using the equation where y is Global_Sales value -:
# y = 0.22175 +1.15543*NA_Sales + 1.34197*EU_Sales
# The adjusted Adjusted R-squared:  0.9685 indicates a good model
# However the residuals are not normally distributed 
 # indicating model does not explain all trends in the data

###############################################################################





