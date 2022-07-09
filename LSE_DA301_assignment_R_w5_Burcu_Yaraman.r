# 1. Prepare THE workstation

# tidyverse package
library(tidyverse)
# Useful for importing data
library(readr) 
# Useful for data wrangling
library(dplyr) 
# Useful for data wrangling
library(tidyr) 
# Useful for creating tidy tables
library(knitr) 
# Useful for working with vectors and functions
library(purrr)
# Useful to create insightful summaries of data set
library(skimr)
# Useful to create insightful reports on data set
library(DataExplorer)

# Import CSV file into R (games_sales.csv)
games_sales <- read.csv (file.choose (), header = T)

# Sense check the data set

View(games_sales)

as_tibble(games_sales) 
# Observations: 
# Year data type is chr --> needs to be converted into integer

##############################################################################

# 2. Understand and wrangle the data set

# Convert Year data type to integer
games_sales$Year <- as.integer(games_sales$Year)

# Missing values / Sum of missing values 
games_sales[is.na(games_sales)]
sum(is.na (games_sales))
# Observations: 271 missing values

summary(games_sales)
# Observations: all missing values are in Year column

# Sum of duplicated rows
sum(duplicated(games_sales))
# Observations: no duplicates

# Remove the rows with NA values
games_sales_clean <- filter(games_sales, !is.na(Year))

summary(games_sales_clean)
# Observations: number of rows decreased to 16327 from 16598 
# (difference: 271 = no of NAs)

##############################################################################

# 3. Format the data set

# All the values under Genre are converted to lowercase for consistency. 
games_sales_clean$Genre <- str_to_lower(games_sales_clean$Genre)

# Merge the values for the variables Platform and Genre in a new column
Platform_Genre = str_c(games_sales_clean$Platform, games_sales_clean$Genre, 
                       sep = "_")
games_sales_formatted <- mutate(games_sales_clean, Platform_Genre)

glimpse(games_sales_formatted)   

##############################################################################

# 4. Aggregate the data sets
# Year figures in the data are ‘year of launch’. 

# In order to find the total Sales per Lauch Year aggregate the data and 
# form a table that includes Global Sales, EU_Sales, NA_Sales figures
# for the corresponding years 

Global_Sales_by_year<-aggregate( Global_Sales~Year, games_sales_formatted, sum)
View(Global_Sales_by_year)

EU_Sales_by_year<-aggregate( EU_Sales~Year, games_sales_formatted, sum)
View(EU_Sales_by_year)

NA_Sales_by_year<-aggregate( NA_Sales~Year, games_sales_formatted, sum)
View(NA_Sales_by_year)

Sales_by_year<-mutate(Global_Sales_by_year, EU_Sales_by_year, NA_Sales_by_year)

##############################################################################

# 4. Visualise the data to understand the trends between the variables

# Skewness basic plots

hist (Global_Sales_by_year$Global_Sales) 
boxplot (Global_Sales_by_year$Global_Sales) 

hist (EU_Sales_by_year$EU_Sales) 
boxplot (EU_Sales_by_year$EU_Sales) 

hist (NA_Sales_by_year$NA_Sales) 
boxplot (NA_Sales_by_year$NA_Sales) 

# the Skewness and Kurtosis functions:
install.packages("moments") 
library(moments)

skewness(Global_Sales_by_year$Global_Sales)  
kurtosis(Global_Sales_by_year$Global_Sales)

skewness(EU_Sales_by_year$EU_Sales)  
kurtosis(EU_Sales_by_year$EU_Sales)

skewness(NA_Sales_by_year$NA_Sales)  
kurtosis(NA_Sales_by_year$NA_Sales)

# Skewness ggplots

ggplot(Global_Sales_by_year, aes(x=Global_Sales, col=I("red"))) + 
  geom_histogram(bins=30)+
  scale_x_continuous (breaks = seq(0, 800, 50), 
                      "Global Sales(m) / launch year")+ 
  scale_y_continuous ("Frequency")+
  labs(title = "Frequency Distribution of Global Games Sales(m) by Launch Year") 

ggplot(EU_Sales_by_year, aes(x=EU_Sales, col=I("yellow"))) + 
  geom_histogram(bins=30)+
  scale_x_continuous (breaks = seq(0, 800, 50), "EU Sales(m) / launch year")+ 
  scale_y_continuous ("Frequency")+
  labs(title = "Frequency Distribution of EU Games Sales(m) by Launch Year")  

ggplot(NA_Sales_by_year, aes(x=NA_Sales, col=I("blue"))) + 
  geom_histogram(bins=30)+
  scale_x_continuous (breaks = seq(0, 800, 50), "NA Sales(m) / launch year")+ 
  scale_y_continuous ("Frequency")+
  labs(title = "Frequency Distribution of NA Games Sales(m) by Launch Year")  

# In the case of our data set, the sales calls data is positively skewed, 
# more values lie below the mean and 
# the distribution has a 'tail' which extends towards the higher values.
# Moreover, positive kurtosis means that the distribution is more peaked 
# (observations close to the mean are more frequent) and has fatter tails 
# (extreme values are more frequent) than normal distribution.

# Correlation between EU_Sales and NA_Sales

ggplot(data = Sales_by_year,  # Set data source.
       mapping = aes(x = EU_Sales, y = NA_Sales)) +  # Add mapping element.
  geom_point(color = "red", # Set the colour to red.
             alpha = 0.5,  # Set the alpha transparency to 0.5.
             size = 1.75) +
  geom_smooth(method = "lm")+
  scale_x_continuous ("EU Sales(m) / launch year")+ 
  scale_y_continuous ("NA Sales(m) / launch year")+
  labs(title = "EU Sales(m) vs NA Sales(m)")

cor (Sales_by_year$EU_Sales, Sales_by_year$NA_Sales)

DataExplorer::create_report(Sales_by_year) 
#Observation: Two variables look strongly correlated. 
# Watch out for multicollinearity! keeping in mind that
# multicollinearity affects the coefficients and p-values, 
# but it does not influence the predictions, precision of the predictions, 
# and the goodness-of-fit statistics.

write.csv(Sales_by_year, file = "C:/Users/murat/LSE_CA/sales_by_year.csv")






 










