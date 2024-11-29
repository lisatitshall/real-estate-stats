#load the required packages
library(tidyverse)
library(readxl)
library(dplyr)
#skewness
library(moments)

#read in the real estate data
real_estate <- read_excel("data/GoodYearRealEstate.xls")

#view the dataset
View(real_estate)

#examine the datatypes
str(real_estate)

#change selected datatypes
real_estate$Bedrooms <- as.integer(real_estate$Bedrooms) 
real_estate$Twnship <- as.integer(real_estate$Twnship) 

#summarize the dataset
summary(real_estate)

#### Descriptive Statistics ####

#investigate the price column
summary(real_estate$Price)

#to create a frequency distribution decide on number of classes
#rule of thumb smallest n such that 2^n > number of observations: 7
#equal class intervals (max - min) / 7 ~ 31
(max(real_estate$Price) - min(real_estate$Price)) / 7 

#start from 120 and use intervals of $30k, 8 intervals
first_value <- 120
last_value <- 360
step <- 30
x_breaks <- seq(first_value, last_value, step)
x_mid <- seq(first_value + (step / 2 ), last_value - (step / 2), step)
x <- cut(real_estate$Price, breaks = x_breaks)
y <- table(x)
frequency_distribution <- data.frame(y)
View(frequency_distribution)

#houses generally selling between $150k and $270k

#look at the cumulative frequency distribution
cf <- cumsum(frequency_distribution$Freq) 
frequency_distribution$CumFreq <- cf

# plotting the data, lines and points
plot(x_breaks, 
     c(0,frequency_distribution$CumFreq),
     xlab="Price ($000k)",
     ylab="Cumulative Frequency",
     type = "b")


#approximately 40% of houses sold for less than $200k 
#approximately 30% of houses sold for more than $250k

#explore mean and median of price and whether one is more representative
mean_price <- mean(real_estate$Price)
median_price <- median(real_estate$Price)

#mean is slightly higher suggesting slight positive skew
#let's look at the distribution
plot(density(real_estate$Price),
     main = "Price Distribution",
     xlab = "Price")

#assess skew, 0.46, mild positive skew, use median
skewness(real_estate$Price)

#assess range and standard deviation
#range $220k
#standard deviation $47k
max(real_estate$Price) - min(real_estate$Price)
standard_deviation_price <- sd(real_estate$Price)

#Chebyshev's theorem: 95% of values lie within 4.5 standard deviations of mean
#9k to 433k
mean_price - (4.5 * standard_deviation_price)
mean_price + (4.5 * standard_deviation_price)

#Because skew < 1 we can use the Empirical Rule
#95% of values lie within 2 standard deviations of mean
#126k to 315k
mean_price - (2 * standard_deviation_price)
mean_price + (2 * standard_deviation_price)


#how many houses have been sold in each township?
town <- as.data.frame(table(real_estate$Twnship)) %>%
  rename("Township" = Var1 )

#plot as a bar chart
barplot(town$Freq,
        names = town$Township,
        xlab = "Township",
        ylab = "Frequency")

#township 4 is most popular

#### end ####

