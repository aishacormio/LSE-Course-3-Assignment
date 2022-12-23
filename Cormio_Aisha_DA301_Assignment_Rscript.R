# Get the working directory.
getwd()

# Change working directory
setwd(dir='/Users/aisha.cormio/Downloads/LSE_DA301_Week_1_files (1)')

# Import the data set.
turtle_sales <- read.csv('turtle_sales.csv', header = TRUE)

# view the dataframe
head(turtle_sales)

# Import neessary packages.
library('tidyverse')
library(ggplot2)

# Remove the 'Ranking', 'Year', 'Genre', and 'Publisher' columns.
turtle_sales2 <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)

# view the dataframe
head(turtle_sales2)

# View summary.
summary(turtle_sales2)


# Explore the data using visualizations.
qplot(Platform, data=turtle_sales2, geom='bar')
# X360 seems to be the most popular console, as it has the greatest number of
# games, followed by PS3, PC and Wii.

# Create a scatter plot of global sales.
qplot(Platform, Global_Sales, data=turtle_sales2, geom=c('point', 'jitter'))
# The majority of games seem to fall between 0 and 20 million pounds in sales,
# while two Wii games generated just under 70 million pounds

# Create a scatter plot of NA sales.
qplot(Platform, NA_Sales, data=turtle_sales2, geom=c('point', 'jitter'))

qplot(Platform, EU_Sales, data=turtle_sales2, geom=c('point', 'jitter'))
# The two highest selling Wii games lead sales in both north america and europe,
# however see greater sales in north america generating just under 35M each
# and 25M in europe.

# North america has more games that generated over 10M in sales, while europe
# seems to only have 4 games that went over 10M in sales.

# It would be interesting to see the overall sales mix taken by each region
# and the sales mix by region for each platform, to see if certain platforms are
# preferred in certain regions.

qplot(Platform, Global_Sales, data=turtle_sales2, geom='boxplot')

qplot(Platform, NA_Sales, data=turtle_sales2, geom='boxplot')

qplot(Platform, EU_Sales, data=turtle_sales2, geom='boxplot')

###############################################################################


# Week 5 assignment activity

View(turtle_sales2)

summary(turtle_sales2)
dim(turtle_sales2)

# NA min is 0, max is 34, mean is 2.5
# Eu min is 0, max is 23, mean is 1.6
# Global min is 0, max is 67, mean is 5.3.


# Group data by product id
df_Product <- turtle_sales2 %>% group_by(Product) %>%
  summarise(Global_Sales_ttl=sum(Global_Sales),
            NA_Sales_ttl=sum(NA_Sales),
            EU_Sales_ttl=sum(EU_Sales)
            )

# View new dataframe
head(df_Product)
dim(df_Product)
summary(df_Product)
str(df_Product)


# Create scatterplots, histograms and boxplots to gain insights into the sales
# data.

# Plot histogram products & global sales
ggplot(df_Product, aes(x=Global_Sales_ttl)) + 
  geom_histogram(bins = 50, fill='blue')+
  labs(title="Distribution of products by global sales generated")

# Plot histogram products & north america sales
ggplot(df_Product, aes(x=NA_Sales_ttl)) + 
  geom_histogram(bins = 50, fill='lightgreen')+
  labs(title="Distribution of products by North America sales generated")

# Plot histogram products & europe sales
ggplot(df_Product, aes(x=EU_Sales_ttl)) + 
  geom_histogram(bins = 50, fill='pink')+
  labs(title="Distribution of products by Europe sales generated")

# Across regions, but especially in Europe, there is a significant number of
# products which generate relatively low sales, while fewer products are able
# to generate over £10M in sales each. It would be interesting to further explore
# what sales mix% these low performing product represent, look into the cost of 
# manufacturing and/or sourcing these low performing products and determine if 
# it is worth keeping all these products in the assortment, or if it might be more
# profitable to invest in the top performing products.


# Comparing EU and NA sales.
ggplot(df_Product, aes(x=NA_Sales_ttl, y=EU_Sales_ttl)) + 
  geom_point(color='orange',
             alpha=0.75,
             size=2) +
  geom_smooth(method=lm, color = 'brown') +
  labs(title = "Europe vs North America Sales", x = "North America Sales £ (M)",
       y = "Europe Sales £ (M)")


# Comparing Global and EU sales.
ggplot(df_Product, aes(x=Global_Sales_ttl, y=EU_Sales_ttl)) + 
  geom_point(color='pink',
             alpha=0.75,
             size=2) +
  geom_smooth(method=lm, color = 'red') +
  labs(title = "Gloabl vs Europe Sales", x = "Global Sales £ (M)",
       y = "Europe Sales £ (M)")


# Comparing Global and NA sales.
ggplot(df_Product, aes(x=Global_Sales_ttl, y=NA_Sales_ttl)) + 
  geom_point(color='lightgreen',
             alpha=0.75,
             size=2) +
  geom_smooth(method=lm, color = 'darkgreen') +
  labs(title = "Gloabl vs North America Sales", x = "Global Sales £ (M)",
       y = "Europe Sales £ (M)")




# Group data by platform
df_Platform <- turtle_sales2 %>% group_by(Platform) %>%
  summarise(Global_Sales_ttl=sum(Global_Sales),
            NA_Sales_ttl=sum(NA_Sales),
            EU_Sales_ttl=sum(EU_Sales))

# View new dataframe
head(df_Platform)
dim(df_Platform)
summary(df_Platform)



# Visualise global sales by platform.
ggplot(df_Platform, aes(x=Platform, y=Global_Sales_ttl)) +
  geom_bar(stat='sum',
           fill='blue') +
  labs(title="Global Sales by Platoform", y= "Global Sales £ (M)")+
  theme(legend.position = "none")


# Visualise NA sales by platform.
ggplot(df_Platform, aes(x=Platform, y=NA_Sales_ttl)) +
  geom_bar(stat='sum',
           fill='lightgreen') +
  labs(title="North America Sales by Platoform", y= "North America Sales £ (M)")+
  theme(legend.position = "none")

# Visualise EU sales by platform.
ggplot(df_Platform, aes(x=Platform, y=EU_Sales_ttl)) +
  geom_bar(stat='sum',
           fill='pink') +
  labs(title="Europe Sales by Platoform", y= "Europe Sales £ (M)")+
  theme(legend.position = "none")

# Best performing platform in Europe is Wii, followed by PS3, X360 and DS.
# Best performing platform in North America is X360, closely followed by Wii and
# then PS3, and DS.
# Globally the top platform is Wii, followed by X360, PS3 and DS.
# Although the same 4 platforms appear at the top of sales across regions, the 
# trends are slightly different trends, for example X360 is not nearly as popular
# in Europe as it is in North America.



# Determine the normality of Global Sales data
# Q-Q plot:
qqnorm(turtle_sales$Global_Sales)
# Add a reference line:
qqline(turtle_sales$Global_Sales, col='red')


# Shapiro-Wilk test:
shapiro.test((turtle_sales$Global_Sales))
# The p-value is <0.05, so we can conclude that the sample data is not 
# normally distributed.


# Check for skewness.
# First import the moments package and library.
library(moments)
skewness(turtle_sales$Global_Sales)
# The output suggests a positive skewness.


#Check for kurtosis.
kurtosis(turtle_sales$Global_Sales)
# Our kurtosis value is greater than 3, suggesting the data has longer tails than
# normal distribution.




# Determine the normality of NA Sales data
# Q-Q plot:
qqnorm(turtle_sales$NA_Sales)
# Add a reference line:
qqline(turtle_sales$NA_Sales, col='red')


# Shapiro-Wilk test:
shapiro.test((turtle_sales$NA_Sales))
# The p-value is <0.05, so we can conclude that the sample data is not 
# normally distributed.


# Check for skewness.
skewness(turtle_sales$NA_Sales)
# The output suggests a positive skewness.


#Check for kurtosis.
kurtosis(turtle_sales$NA_Sales)
# Our kurtosis value is greater than 3, suggesting the data has longer tails than
# normal distribution.



# Determine the normality of EU Sales data
# Q-Q plot:
qqnorm(turtle_sales$EU_Sales)
# Add a reference line:
qqline(turtle_sales$EU_Sales, col='red')


# Shapiro-Wilk test:
shapiro.test((turtle_sales$EU_Sales))
# The p-value is <0.05, so we can conclude that the sample data is not 
# normally distributed.


# Check for skewness.
skewness(turtle_sales$EU_Sales)
# The output suggests a positive skewness.


#Check for kurtosis.
kurtosis(turtle_sales$EU_Sales)
# Our kurtosis value is greater than 3, suggesting the data has longer tails than
# normal distribution.


# The distribution of global, north america and europe sales seems to be fairly similar.


# Determine if there are any correlations.

# Check correlation between Global and EU sales using Pearson's correlation.
cor(turtle_sales$Global_Sales, turtle_sales$EU_Sales)
# The correlation coefficient of 0.88 suggests a strong positive correlation.

# Check correlation between Global and NA sales using Pearson's correlation.
cor(turtle_sales$Global_Sales, turtle_sales$NA_Sales)
# The correlation coefficient of 0.93 suggests an even stronger positive correlation.

# Check correlation between NA and EU sales using Pearson's correlation.
cor(turtle_sales$NA_Sales, turtle_sales$EU_Sales)
# The correlation coefficient of 0.70 suggests a strong positive correlation.

# The scatter plots and the Pearson correlation test clearly show a strong 
# positive correlation between all 3 sales data. The strongest correlation being
# between global and north america sales suggesting north america follows global
# trends pretty closely, while europe trends are a little more unique.

################################################################################

# Install the psych package.
install.packages('psych')

# Import the psych package.
library(psych)


# Create a linear regression model for Global sales & EU Sales.
modelGE <- lm(turtle_sales3$Global_Sales ~ turtle_sales3$EU_Sales)

# View modelGE
modelGE

# View the summary stats.
summary(modelGE)

# Plot the model.
plot(turtle_sales3$Global_Sales ~ turtle_sales3$EU_Sales)
coefficients(modelGE)

# Add line-of-best-fit.
abline(coefficients(modelGE))



# Create a linear regression model for Global sales & NA Sales.
modelGN <- lm(turtle_sales3$Global_Sales ~ turtle_sales3$NA_Sales)

# View modelGN
modelGN

# View the summary stats.
summary(modelGN)

# Plot the model.
plot(turtle_sales3$Global_Sales ~ turtle_sales3$NA_Sales)
coefficients(modelGN)

# Add line-of-best-fit.
abline(coefficients(modelGN))



# Create a linear regression model for Europe sales & NA Sales.
modelEN <- lm(turtle_sales3$EU_Sales ~ turtle_sales3$NA_Sales)

# View modelEN
modelEN

# View the summary stats.
summary(modelEN)

# Plot the model.
plot(turtle_sales3$EU_Sales ~ turtle_sales3$NA_Sales)
coefficients(modelEN)

# Add line-of-best-fit.
abline(coefficients(modelEN))



# Create a multiple linear regression model
modelGlobal = lm(Global_Sales~NA_Sales+EU_Sales, data=turtle_sales3)

# Print the summary statistics.
summary(modelGlobal)


# Load the new data file (turtle_sales_test.csv).
turtletest <- read.csv(file.choose(), header=TRUE)

# View the data.
str(turtletest)

# Create a new object and specify the predict function.
predictTest = predict(modelGlobal, newdata=turtletest,
                      interval='confidence')

# Print the object.
predictTest 

# The model predicted global sales that are fairly close to the actual observed
# figures, however 4 out of 5 predicted figures were slightly bigger than the
# the observed sales.


# Both North America sales and Europe sales alone are useful predictors of global
# sales,however we see the best model with the highest adjusted r-squared value
# is when we use a multiple linear regression model using both region sales which
# is able to explain 87% of global sales.


