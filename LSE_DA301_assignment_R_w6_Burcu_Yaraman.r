# Import package
library(tidyverse)

# Import CSV file into R (sales_by_year.csv) # the output file of the previous exercise(w5).
# Also added in the zip folder of my deliverables.
Sales_by_year <- read.csv (file.choose (), header = T)

# Sense check the data set
View(Sales_by_year)
as_tibble(Sales_by_year)
summary(Sales_by_year)
cor(Sales_by_year)
# Note that according to Evans' classification, 
# less than 0.20 is very weak, 
# 0.20 to 0.39 is weak, 
# 0.40 to 0.59 is moderate, 
# 0.60 to 0.79 is strong and 
# 0.80 or greater is a very strong correlation. 
# Global Sales and EU Sales - NA Sales are strongly correlated,
# it is good to go with them.

model_Global_Sales <- lm(Global_Sales ~ EU_Sales + NA_Sales, data=Sales_by_year)
summary(model_Global_Sales) # [3] Print the summary statistics.
# The multiple R-squared for this model is 0.997, 
# while the adjusted R-squared for this model is 0.9969.
# Check the significance of the explanatory variables in the coefficients table:  
# We can see that both EU Sales and NA Sales are very significant.

# We would want to look at the residuals from this model:
plot(model_Global_Sales$residuals)
# There is no  be no pattern in these residuals. They look like like white noise 

# Test the model with launch year 2000 data 
Global_Sales_forecast_2000 <- filter(Sales_by_year, Year==2000)

Global_Sales_forecast_test <- predict(model_Global_Sales, newdata = Global_Sales_forecast_2000)
Global_Sales_forecast_test
# the estimated Global Sales is 194.2972 where the Global Sales for launch year 2000 is 201.56.

# Predict the global sales (in millions) for the next financial year
# Assumption: 1- The input table Sales_by_year includes the sales details of 2022.
# Assumption: 2- EU Sales would increase 4% and NA Sales would increase 6% next year (2023) 

# Sum the columns of Sales_by_year. 
# This gives us the Total Global Sales,
# Total EU Sales and Total NA Sales figures of our current table.  
Global_Sales_forecast_2023 <- apply (Sales_by_year, 2, sum)
Global_Sales_forecast_2023
# Global_Sales     EU_Sales     NA_Sales 
# 8820.36          2409.12      4333.43 

#As per my assumptions, I am increasing EU_Sales by 4% and NA Sales by 6 %
Global_Sales_forecast_2023[['EU_Sales']] <- Global_Sales_forecast_2023[['EU_Sales']] * 1.04
Global_Sales_forecast_2023[['NA_Sales']] <- Global_Sales_forecast_2023[['NA_Sales']] * 1.06
Global_Sales_forecast_2023
# EU_Sales     NA_Sales 
# 2505.485     4593.436 

# Turn Global_Sales_forecast_2023 into a dataframe 
Global_Sales_forecast_2023 <- as.data.frame (t(Global_Sales_forecast_2023))
Global_Sales_forecast_2023


# Now run the prediction with the new figures to get an estimate of Global Years for the year 2023
Global_Sales_forecast <- predict(model_Global_Sales, newdata=Global_Sales_forecast_2023)
Global_Sales_forecast
# ın the event of EU Sales would increase 4% and NA Sales would increase 6% next year:
# Global Sales would be $8833.497 (m) 

# As a next step it would be best to revisit the assumption about EU_Sales and NA_Sales 2023 figures 
# and create regression models for these two variables in order to them predict with more confidence.