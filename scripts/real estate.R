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

#are there any outliers? 
#look at boxplot, no outliers
boxplot(real_estate$Price,
        ylab = "Price (000k)")

#1st and 3rd quartiles, $187k and $251k
quantile(real_estate$Price, 
         prob = c(.25,.75))

#is there a relationship between price and size of home?
#plot scatter with line, weak positive relationship
plot(real_estate$Price,
     real_estate$Size,
     xlab = "Price (000k)",
     ylab = "Size (f2)"
)

abline(lm(real_estate$Size ~ real_estate$Price),
       col = 'blue',
       lty = 'dashed')

#what about price and distance from city centre
#stronger inverse relationship
plot(real_estate$Price,
     real_estate$Distance,
     xlab = "Price (000k)",
     ylab = "Distance (miles)"
)

abline(lm(real_estate$Distance ~ real_estate$Price),
       col = 'blue',
       lty = 'dashed')

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

#### Probability ####
#If a house is chosen at random what are the following probabilities:
#1. The home is in Township 1 or has a pool
#2. Given that it is in Township 3, that it has a pool
#3. The home has a pool and is in Township 3

#Firstly group the data to visualize and count
pool <- real_estate %>% 
  group_by(Twnship, Pool) %>% 
  summarise(homes = n())
View(pool)

#probability that home is in Township 1
town1 <- sum(pool[which(pool$Twnship=='1'), 3]) / nrow(real_estate)

#probability that home has a pool
has_pool <- sum(pool[which(pool$Pool=='1'), 3]) / nrow(real_estate)

#probability that home is in Township 1 and has a pool
town1_has_pool <- sum(pool[which(pool$Pool=='1' & pool$Twnship=='1'), 3]) / nrow(real_estate)

# We use General Rule of Addition because events aren't mutually exclusive 
probability_town1_or_pool <- town1 + has_pool - town1_has_pool 

#probability that home is in town 3
town3 <- sum(pool[which(pool$Twnship=='3'), 3]) / nrow(real_estate)

#probability that home is in town 3 and has a pool
town3_has_pool <- sum(pool[which(pool$Pool=='1' & pool$Twnship=='3'), 3]) / nrow(real_estate)

#using conditional probability
probability_has_pool_given_town_3 <- town3_has_pool / town3

#already have third probability
probability_has_pool_and_town3 <- town3_has_pool

#If a house is chosen at random what are the following probabilities:
#1. The home has a garage attached
#2. The home doesn't have a garage attached, given it is in Township 5
#3. The home has a garage attached and is in Township 3
#4. The home does not have a garage attached or is in Township 2

#group the data
garages <- real_estate %>% 
  group_by(Twnship, Garage) %>% 
  summarise(homes = n())
View(garages)

probability_has_garage <- sum(garages[which(garages$Garage=='1'), 3]) / nrow(real_estate)


#probability of being in Township 5
town5 <- sum(garages[which(garages$Twnship=='5'), 3]) / nrow(real_estate)

#probability of being in Township 5 and not having a garage
town5_no_garage <- sum(
  garages[which(garages$Twnship=='5' & garages$Garage=='0' ), 3]) / 
  nrow(real_estate)

#using conditional probability
probability_no_garage_given_town5 <- town5_no_garage / town5

#probability of having garage and being in town 3
probability_garage_and_town3 <- sum(
  garages[which(garages$Twnship=='3' & garages$Garage=='1' ), 3]) / 
  nrow(real_estate)

#probability of not having a garage attached using complement
no_garage <- 1 - probability_has_garage

#probability of being in town2
town2 <- sum(garages[which(garages$Twnship=='2'), 3]) / nrow(real_estate)

#probability of being in town 2 and not having a garage
town2_no_garage <- sum(
  garages[which(garages$Twnship=='2' & garages$Garage=='0'), 3]) / 
  nrow(real_estate)

#Events aren't mutually exclusive so use General Rule of Addition
probability_town2_or_no_garage <- no_garage + town2 - town2_no_garage

#### end #### 

