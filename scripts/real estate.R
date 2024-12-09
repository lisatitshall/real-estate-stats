#load the required packages
library(tidyverse)
library(readxl)
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

#assess skew, 0.46, mild positive skew
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

#For third probability, events are dependent
#use General Rule of Multiplication 
probability_has_pool_and_town3 <- town3 * probability_has_pool_given_town_3

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

#probability of being in town 3 and having a garage
town3_garage <- sum(
  garages[which(garages$Twnship=='3' & garages$Garage=='1' ), 3]) / 
  nrow(real_estate)

#probability of being in town 3, given it has a garage
probability_town3_given_garage <- town3_garage / probability_has_garage

#probability of having garage and being in town 3, using General Rule
probability_garage_and_town3 <- probability_has_garage * probability_town3_given_garage

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

#### probability distributions ####

#create a probability distribution for the number of bedrooms

#group the data
bedrooms <- real_estate %>% 
  group_by(Bedrooms) %>% 
  summarise(homes = n())
View(bedrooms)

#add probability column
bedrooms <- bedrooms %>% 
  mutate(probability = homes / nrow(real_estate))

#calculate the mean
mean_bedrooms <- sum(bedrooms$Bedrooms * bedrooms$probability)

#calculate the standard deviation
variance_bedrooms <- 
  sum(((bedrooms$Bedrooms - mean_bedrooms) ^ 2) * bedrooms$probability)

standard_deviation_bedrooms <- variance_bedrooms ^ 0.5
standard_deviation_bedrooms

#what is the probability of a house having a pool
pool_probability <- sum(real_estate$Pool) / nrow(real_estate)

#choose 10 homes, what's the probability that exactly one will have a pool
#use binomial distribution 

exactly_one_pool <- 
  choose(10,1) * 
  (pool_probability ^ 1) * 
  ((1-pool_probability) ^ 9)

#what's the probability that at least one will have a pool 
at_least_one_pool <- 
  1 - (choose(10,0) * 
         (pool_probability ^ 0) * 
         ((1-pool_probability) ^ 10)
  )

# is the normal distribution a good approximation for price?
#we calculated mean and standard deviation of price earlier
mean_price
standard_deviation_price

#use normal distribution to estimate % of homes selling for more than £280k
z <- (280 - mean_price) / standard_deviation_price
over_280_normal_approx <- pnorm(z, lower.tail =  FALSE)

# 11% 
# in reality 13% of houses were sold for more than 280k
over_280 <- sum(real_estate$Price > 280) / nrow(real_estate)

#assume the 105 homes are the population 

#choose a sample of 10 homes
sample_10 <- real_estate[sample(1:nrow(real_estate), 10),]
View(sample_10)

#calculate mean and standard deviation of sample
sample_10_mean <- mean(sample_10$Price)
sample_10_standard_deviation <- sd(sample_10$Price)

#what is the probability of the sample mean being at least this value
sample_z_value <- (sample_10_mean - mean_price) / 
  (standard_deviation_price / (10 ^ 0.5))

#29%
sample_probability <- pnorm(sample_z_value, lower.tail = FALSE)

#### end ####

#### confidence intervals and hypothesis testing####
#develop a 95% confidence interval for the mean selling price
#assume the data we have is a sample

#we've already calculated the mean and standard deviation
mean_price
standard_deviation_price

#we don't know the standard deviation of the population so use t distribution
#because our sample is > 30 we can assume normality
#manually using t-value from table
lower_bound <- mean_price - 
  ( 1.984  * (standard_deviation_price / (nrow(real_estate)^0.5)))
upper_bound <- mean_price + 
  ( 1.984  * (standard_deviation_price / (nrow(real_estate)^0.5)))

#using R to check
t.test(real_estate$Price, conf.level = 0.95)

#suggested mean price in the area is more than $220k
#can we conclude this from our sample?

#one sided hypothesis test where null hypothesis <= 220k
#don't know standard deviation of population
#test at the .01 significance level
#reject if t > ~2.364

t_score <- (mean_price - 220) / 
  (standard_deviation_price / (nrow(real_estate) ^ 0.5))

#t = 0.23, don't reject the null hypothesis. We can't say mean > 220k
#check using R
t.test(real_estate$Price, 
       alternative = c("greater"),
       mu = 220,
       conf.level = 0.99
)

#at .05 significance level can we conclude a difference in
#mean selling price of homes with and without a pool

#samples are independent
#how many houses do/don't have a pool - 38/67
#samples are bigger than 30 so normal approximation holds
houses_with_pool <- real_estate[which(real_estate$Pool=='1'),]
houses_without_pool <- real_estate[which(real_estate$Pool=='0'),]

#don't know population standard deviation 
#are the variances of both samples approximately equal, no
pool_variance <- var(houses_with_pool$Price)
without_pool_variance <- var(houses_without_pool$Price)

#test this statistically, reject null, variances are unequal
var.test(houses_with_pool$Price, houses_without_pool$Price)

#use t test with variances unequal(Welch)
#reject null
#there's a difference in mean price for houses with/without pool
t.test(houses_with_pool$Price,
       houses_without_pool$Price,
       var.equal = FALSE,
       conf.level = 0.95)

#at .05 significance level can we conclude a difference in
#mean selling price of homes with and without a garage

#samples are independent
#how many houses do/don't have a garage - 71/34
#samples are bigger than 30 so normal approximation holds
houses_with_garage <- real_estate[which(real_estate$Garage=='1'),]
houses_without_garage <- real_estate[which(real_estate$Garage=='0'),]

#population standard deviation is unknown
#are the variances of both samples approximately equal, no
garage_variance <- var(houses_with_garage$Price)
without_garage_variance <- var(houses_without_garage$Price)

#test this statistically, reject null, variances are unequal
var.test(houses_with_garage$Price, houses_without_garage$Price)

#use t test with variances unequal(Welch)
#reject null
#there's a difference in mean price for houses with/without garage
t.test(houses_with_garage$Price,
       houses_without_garage$Price,
       var.equal = FALSE,
       conf.level = 0.95)

#at 0.5 significance level can we conclude a difference in mean price
#between houses in Township 1 and Township 2

#samples are independent
#how many houses are in Township 1/2 - 15/20
#samples are small, we'd have to assume population is normally distributed
#with house prices this seems sensible
houses_township1 <- real_estate[which(real_estate$Twnship=='1'),]
houses_township2 <- real_estate[which(real_estate$Twnship=='2'),]

#population standard deviation is unknown
#are the variances of both samples approximately equal
township1_variance <- var(houses_township1$Price)
township2_variance <- var(houses_township2$Price)

#test statistically, don't reject null, variances are equal
var.test(houses_township1$Price, houses_township2$Price)

#use t-test
#reject null, difference in mean price of Township 1 and 2
t.test(houses_township1$Price,
       houses_township2$Price,
       var.equal = TRUE,
       conf.level = 0.95)

#### end ####



