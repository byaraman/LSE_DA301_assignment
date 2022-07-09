# Prepare the workstation
install.packages("tidyverse")
library(tidyverse)

# Import CSV file into R (lego.csv)
lego_customer_data <- read.csv("lego.csv", header = TRUE)

# Sense check the data set
as_tibble(lego_customer_data)
summary(lego_customer_data)

# Understand and wrangle the data set
lego_customer_data[is.na(lego_customer_data)]
sum(is.na (lego_customer_data))

# 'Country' variable's minimum number shouldn't be zero - 
# Number of countries the product is sold in should be 
# equal to or greater than 1. 
# There are 575 such records, which is less than 5% of 12,261 records.
# I decided to keep them in the dataset. 
# I will not touch them in this excercise, since country variable is not used; 
# I will update by mean if/when necessary.

# Number of Reviews by Customer Age
qplot(ages, num_reviews, data = lego_customer_data, label = num_reviews, 
      geom = c("point", "text"), hjust = 0, vjust=-0.5, xlab = "Customer Age",
      ylab = "Number of Reviews", main = "Number of Reviews by Customer Age",
      check_overlap = TRUE)
# Customer Age corresponding to Number of Reviews
qplot(ages, num_reviews, data = lego_customer_data, label = ages, 
      geom = c("point", "text"), hjust = 0, vjust=-0.5, xlab = "Customer Age",
      ylab = "Number of Reviews", 
      main = "Customer Age corresponding to Number of Reviews",
      check_overlap = TRUE)

# The above two plots will give an idea on "the age group 
# that submits many reviews" (looks like 7-8 year olds). However 
# the plots might under-state the total number of reviews, 
# as scatter plot does not give a hint on the overlapping data points.
# Using "jitter" doesn't help much either.

# Which age group submits the most reviews?
# It makes more sense to use geom = col ( If you want the heights of the bars 
# to represent values in the data, use geom_col() instead. 
#geom_bar() uses stat_count() by default)
lego_customer_data2 <- mutate(lego_customer_data, 
                              ages = as.factor(ages))
qplot(ages, num_reviews, data = lego_customer_data2, 
     geom = "col", xlab = "Customer Age",
      ylab = "Number of Reviews", main = "Number of Reviews by Customer Age", 
      label=num_reviews)
#Here, we can specifically say that age year group 8 submits the most reviews.

# filter the customers aged >=25
lego_age_greater_or_equal_25 <-lego_customer_data[lego_customer_data$ages>=25,]
lego_customer_data3 <- mutate(lego_age_greater_or_equal_25, 
                              ages = as.factor(ages))
as_tibble(lego_customer_data3)
summary(lego_customer_data3)

# What is the most expensive Lego set purchased by customers >=25 years old?
qplot(ages, list_price, data = lego_customer_data3, label = list_price, 
      geom = c("boxplot", "text"), hjust = 0, vjust=1, 
      check_overlap = TRUE)

#Now that we have some additional insight on customer behaviour:
# •	We can analyse the comments made for products that are targeted for year 8’s 
# and understand what we do good good / what needs to be improved. 
# •	We know the target audience for expensive products, 
# and could consider introducing new products 
# that would attract 29 year olds' attention. 


