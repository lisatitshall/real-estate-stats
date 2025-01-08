#load the required packages
library(tidyverse)
library(tidymodels)
library(readxl)
#skewness
library(moments)
#Bruesch-Pagan test for heteroscedasticity
library(lmtest)
library(tree)

#read in the real estate data
real_estate <- read_excel("data/GoodYearRealEstate.xls")

#view the dataset
View(real_estate) 

#examine the datatypes
str(real_estate)

#change selected datatypes
real_estate$Bedrooms <- as.integer(real_estate$Bedrooms) 
real_estate$Twnship <- as.factor(real_estate$Twnship) 
real_estate$Pool <- as.logical(real_estate$Pool)
real_estate$Garage <- as.logical(real_estate$Garage)

#plot to see all relationships
#stand outs:
#weak negative relationship between distance from town and price
#price generally higher if house has a garage
plot(real_estate)

#plot individual / pairs of variables
plot(real_estate$Price) # quantitative variable, plotted against index
plot(real_estate$Twnship) # categorical variable, bar chart
plot(real_estate$Price, real_estate$Distance) # two quant variables, scatter
plot(real_estate$Price, real_estate$Size) # two quant variables, scatter
#one quant, one category, boxplots
plot(real_estate$Twnship, 
     real_estate$Price,
     main = "Houses Prices by Township", 
     xlab = "Township",
     ylab = "Price (000k)")
#more bedrooms generally means higher price but only 4 homes with 7/8 bedrooms
plot(as.factor(real_estate$Bedrooms), 
     real_estate$Price,
     main = "Houses Prices by Number of Bedrooms", 
     xlab = "Number of Bedrooms",
     ylab = "Price (000k)")

#surprisingly houses without pool have a higher median
plot(as.factor(real_estate$Pool), 
     real_estate$Price,
     main = "Houses Prices by Pool", 
     xlab = "Has Pool",
     ylab = "Price (000k)")

#homes with a garage have a higher median
plot(as.factor(real_estate$Garage), 
     real_estate$Price,
     main = "Houses Prices by Garage", 
     xlab = "Has Garage",
     ylab = "Price (000k)")

#With 3 bathrooms the median price is higher, the rest are similar
plot(as.factor(real_estate$Baths), 
     real_estate$Price,
     main = "Houses Prices by Number of Bathrooms", 
     xlab = "Number of Bathrooms",
     ylab = "Price (000k)")


#barchart of homes with/without pool
pools_table <- table(real_estate$Pool)
barplot(pools_table,
        xlab = "Has pool",
        ylab = "Count of houses"
        )

#price distribution, frequency version
hist(real_estate$Price, 
     xlab = "Price (000k)", 
     main = "Price Distribution")

#price distribution, density version 
hist(real_estate$Price, 
     xlab = "Price (000k)", 
     main = "Price Distribution",
     freq = FALSE
     )
#add normal to see what it would look like
curve(dnorm(x, mean = mean(real_estate$Price), sd = sd(real_estate$Price)),
      add = TRUE, 
      col = "red")

#does log transform look more symmetric?
ggplot(real_estate, aes(x = Price)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 10) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(real_estate$Price), 
                  sd = sd(real_estate$Price)), 
                colour = "red")


ggplot(real_estate, aes(x = log10(Price))) + 
  geom_histogram(aes(y = after_stat(density)), bins = 10) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(log10(real_estate$Price)), 
                  sd = sd(log10(real_estate$Price))), 
                colour = "red") 

#### Descriptive Statistics ####
#summarize whole dataset
summary(real_estate)

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

#mean is slightly higher confirming slight positive skew observed earlier

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
has_pool <- sum(pool[which(pool$Pool==TRUE), 3]) / nrow(real_estate)

#probability that home is in Township 1 and has a pool
town1_has_pool <- sum(pool[which(pool$Pool==TRUE & pool$Twnship=='1'), 3]) / nrow(real_estate)

# We use General Rule of Addition because events aren't mutually exclusive 
probability_town1_or_pool <- town1 + has_pool - town1_has_pool 

#probability that home is in town 3
town3 <- sum(pool[which(pool$Twnship=='3'), 3]) / nrow(real_estate)

#probability that home is in town 3 and has a pool
town3_has_pool <- sum(pool[which(pool$Pool==TRUE & pool$Twnship=='3'), 3]) / nrow(real_estate)

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

probability_has_garage <- sum(garages[which(garages$Garage==TRUE), 3]) / nrow(real_estate)

#probability of being in Township 5
town5 <- sum(garages[which(garages$Twnship=='5'), 3]) / nrow(real_estate)

#probability of being in Township 5 and not having a garage
town5_no_garage <- sum(
  garages[which(garages$Twnship=='5' & garages$Garage==FALSE ), 3]) / 
  nrow(real_estate)

#using conditional probability
probability_no_garage_given_town5 <- town5_no_garage / town5

#probability of being in town 3 and having a garage
town3_garage <- sum(
  garages[which(garages$Twnship=='3' & garages$Garage==TRUE ), 3]) / 
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
  garages[which(garages$Twnship=='2' & garages$Garage==FALSE), 3]) / 
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

#check using r
dbinom(x=1, size = 10, prob = pool_probability)

#what's the probability that at least one will have a pool 
at_least_one_pool <- 
  1 - (choose(10,0) * 
         (pool_probability ^ 0) * 
         ((1-pool_probability) ^ 10)
  )

#check using r
sum(dbinom(x= 1:10, size = 10, prob = pool_probability))

# is the normal distribution a good approximation for price?
#we calculated mean and standard deviation of price earlier
mean_price
standard_deviation_price

#use normal distribution to estimate % of homes selling for more than £280k
z <- (280 - mean_price) / standard_deviation_price
over_280_normal_approx <- pnorm(z, lower.tail =  FALSE)

#same as above because default for pnorm is mean 0, sd 1
pnorm(280, mean = mean_price, sd = standard_deviation_price, lower.tail = F)

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

#using R to check (don't need the test part)
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
houses_with_pool <- real_estate[real_estate$Pool==TRUE,]
houses_without_pool <- real_estate[real_estate$Pool==FALSE,]

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
houses_with_garage <- real_estate[which(real_estate$Garage==TRUE),]
houses_without_garage <- real_estate[which(real_estate$Garage==FALSE),]

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

#at 0.02 significance level, is there a difference in the variability
#of house prices that do/don't have a pool (similar calculated before)

#to test we use the F statistic
F <- without_pool_variance / pool_variance

#f critical value
f_critical <- qf(0.01, 66, 37, lower.tail = FALSE)

#F is larger than f critical so reject the null, there's a variance difference
#test using R
var.test(houses_without_pool$Price, 
         houses_with_pool$Price, 
         conf.level = 0.98)

#at the 0.05 significance level is there a difference in the mean
#selling price of homes among five townships

#assumptions for ANOVA:
#independent populations - yes, different Townships
#populations follow normal - have to assume this because samples are too small
#populations have equal standard deviation - need to check
#we already have township 1 and 2 data, get the rest
houses_township3 <- real_estate[which(real_estate$Twnship=='3'),]
houses_township4 <- real_estate[which(real_estate$Twnship=='4'),]
houses_township5 <- real_estate[which(real_estate$Twnship=='5'),]

township3_variance <- var(houses_township3$Price)
township4_variance <- var(houses_township4$Price)

township5_variance <- var(houses_township5$Price)

#variances look different, test for the largest difference
#accept null, variances are equal
#change numbers to compare the rest (just in case)
var.test(houses_township4$Price, 
         houses_township5$Price, 
         conf.level = 0.95)

#can't reject the null in any case, variances are equal
#proceed with anova
#p-value is 0.2 so we can't reject the null, no difference in mean
one_way_anova <- aov(Price ~ Twnship, data = real_estate)
summary(one_way_anova)

#is there correlation between distance/price and size
#to use Pearson's correlation we assume normal
#plot histogram of distance/price (already seen size ~ normal)
ggplot(real_estate, aes(x = Distance)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun=dnorm, 
                args = list(mean = mean(real_estate$Distance),
                            sd = sd(real_estate$Distance)))
                                       
median(real_estate$Distance)                                       
mean(real_estate$Distance)

ggplot(real_estate, aes(x = Size)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun=dnorm, 
                args = list(mean = mean(real_estate$Size),
                            sd = sd(real_estate$Size)))

median(real_estate$Size)                                       
mean(real_estate$Size)

#both look approx normal, use Pearson's R
#moderate negative relationship between distance and price
#moderate positive between price and size
cor(real_estate %>% select(Price, Distance, Size))

#is there any significance?
#yes at 95%
cor.test(real_estate$Price, 
         real_estate$Distance)
#yes at 95%
cor.test(real_estate$Price, 
         real_estate$Size)


#### end ####

#### linear regression model #### 
#Aim to predict price based on combo of other variables
#From analysis so far we've seen the following with price
#moderate/strong relationship: distance, garage, size, pool
#no/weak relationship: baths, bedrooms, township

#is there any correlation between those 4 variables
#nothing immediately obvious, investigate garage/pool more
plot(real_estate %>% select(Distance, Garage, Pool, Size))

#no trend between garage and pool
ggplot(real_estate,
       aes(x = Garage, fill = Pool)) + 
  geom_bar() 

#are there any outliers in size and distance that could affect analysis
#2 for size but not overly concerned
ggplot(real_estate,
       aes(x = Size)) + 
  geom_boxplot() 

ggplot(real_estate,
       aes(x = Distance)) + 
  geom_boxplot() 

#normalize data because we have different scales
#split data into train and test
set.seed(1353)
split <- initial_split(real_estate)
train <- training(split)
test <- testing(split)

#standardize size and distance
train$Size <- scale(train$Size)
train$Distance <- scale(train$Distance)

#fit a linear model
linear_model <- lm(Price ~ Distance + Size + Garage + Pool,
                      data = train)

#review model, most p-values are low enough, distance borderline at .09
#Only 48% of variance in price explained by model
#33k RSE, roughly the average error, seems high
summary(linear_model)

#plot to examine residuals
par(mfrow = c(2,2)) 
plot(linear_model)

#first graph looks like heteroscedasticity 
#second graph is OK, approx normal
#no evidence of influential points

#test heteroscedasticity statistically, p 0.0595
bptest( Price ~ Distance + Size + Garage + Pool, data = train)

#heteroscedasticity is a problem, need to investigate solutions
#try log transformation of price
#what about quadratic terms?

#reset grid
par(mfrow = c(1,1))

#try log transform of price 
train <- train %>% mutate(log_price = log(Price))

linear_model_log_price <- lm(log_price ~ Distance + Size + Garage + Pool,
                   data = train)

#assess model results
#results look v.similar, less evidence of heteroscedasticity but p still 0.12
summary(linear_model_log_price)
bptest( log_price ~ Distance + Size + Garage + Pool, data = train)
par(mfrow = c(2,2)) 
plot(linear_model_log_price)

#look again at size / price and distance / price relationship 
ggplot(data = train, aes(x=log_price, y=Size)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()

ggplot(data = train, aes(x=log_price, y=Distance)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  theme_bw()

#could argue this is non-linear, try quadratic size term
#results almost same as first model and quadratic terms not significant
quadratic_model <- lm(Price ~ Distance + Size + Garage + Pool + 
                        I(Size^2) + I(Distance^2),
                      data = train)
summary(quadratic_model)
plot(quadratic_model)

#compare the two models using ANOVA for interest sake
#quadratic model isn't an improvement
anova(linear_model, quadratic_model)

#look at a tree to see which variables affect price
#a lot of different factors here
tree_model <- tree(Price~., data=train %>% select(!log_price))
plot(tree_model)
text(tree_model)

#try adding Bedrooms to the model
#adjusted R squared still 48%
linear_model_2 <- lm(log_price ~ Distance + Size + Garage + Pool + Bedrooms,
                     data = train)

summary(linear_model_2)
plot(linear_model_2)
bptest(log_price ~ Distance + Size + Garage + Pool + Bedrooms, data = train)

#how does this compare, model with bedrooms is an improvement
anova(linear_model_log_price, linear_model_2)

#try another model without distance (which wasn't significant)
#adjusted R squared about the same
#heteroscedascity no longer a problem, normal errors, no influential points
linear_model_3 <- update(linear_model_2, ~.-Distance)
summary(linear_model_3)
plot(linear_model_3)
plot(cooks.distance(linear_model_3), pch = 16, col = "blue")
bptest(log_price ~ Size + Garage + Pool + Bedrooms, data = train)

#how does this model compare, bit worse but not significantly so
#remove distance, linear model 3 is chosen 
anova(linear_model_2, linear_model_3)

#reset grid
par(mfrow = c(1,1)) 

#try chosen model on test dataset, need to scale first
test$Size <- scale(test$Size)
test$Distance <- scale(test$Distance)
test <- test %>% mutate(log_price = log(Price))
test$predictions <- predict(linear_model_3, test)

#convert the predictions back to $000k
test <- test %>% mutate(real_predictions = exp(test$predictions))
test$errors <- test$real_predictions - test$Price

#moderate correlation between predictions and actual, 0.6
cor(test$real_predictions, test$Price)

#what about correlation between errors and independent variables?
#nothing numerical
cor(test %>% select(Size, Bedrooms, errors))

#what about garage, lots of overlap but garage estimates are lower
plot(test$Garage, 
     test$errors,
     main = "Errors by Garage", 
     xlab = "Garage",
     ylab = "Errors (00k)")

#check distributions of errors
garage_test <- test %>% filter(Garage == T)  %>% select(errors)
no_garage_test <- test %>% filter(Garage == FALSE)  %>% select(errors)

#looks non-normal
qqnorm(garage_test$errors, pch = 1, frame = FALSE)
qqline(garage_test$errors, col = "blue", lwd = 2)
qqnorm(no_garage_test$errors, pch = 1, frame = FALSE)
qqline(no_garage_test$errors, col = "blue", lwd = 2)

#use non parametric t-test 
#some evidence to reject null
#model is underestimating houses with a garage more often
wilcox.test(errors ~ Garage, data = test)

#what about pool, lots of overlap
plot(test$Pool, 
     test$errors,
     main = "Errors by Pool", 
     xlab = "Pool",
     ylab = "Errors (00k)")

pool_test <- test %>% filter(Pool == T)  %>% select(errors)
no_pool_test <- test %>% filter(Pool == FALSE)  %>% select(errors)

#check distribution, looks non-normal
qqnorm(pool_test$errors, pch = 1, frame = FALSE)
qqline(pool_test$errors, col = "blue", lwd = 2)
qqnorm(no_pool_test$errors, pch = 1, frame = FALSE)
qqline(no_pool_test$errors, col = "blue", lwd = 2)

#use non parametric t-test 
#some evidence to reject null
wilcox.test(errors ~ Pool, data = test)

#calculate mean squared error, high
mse <- mean((test$Price - test$real_predictions)^2)
rmse <- mse ^ 0.5

#mape, 0.17, low is good
#on average predictions are 17% away from actuals
mape <- mean(abs((test$real_predictions - test$Price))/
               test$Price) 

#plot predictions agsinst actual, not great 
plot(test$real_predictions, 
     test$Price, 
     xlab = "Predictions (000k)",
     ylab = "Actual (000k)")
abline(a = 0, b = 1)

#no influential points
plot(cooks.distance(linear_model_3), pch = 16, col = "blue")

#### end ####

#### ANCOVA ####
#Would there be any benefit to having multiple regression lines 
#  for different categories?

#Garage and Pool were both significant in the linear model
#From visuals there was less price overlap for houses with/without garage

#plot garage, size and price
#slope more extreme for houses with Garage
ggplot(data = real_estate, 
       aes(x = Size, y = Price, color = Garage)) +
  geom_point(data = real_estate) +
  geom_smooth(method = "lm", se=FALSE) +
  theme_bw()

#plot pool, size and price
#slopes look roughly equal 
ggplot(data = real_estate, 
aes(x = Size, y = Price, color = Pool)) +
  geom_point(data = real_estate) +
  geom_smooth(method = "lm", se=FALSE) +
  theme_bw()

#try ancova (checked that train has enough T/F Garage/Pool)
ancova <- lm(log_price ~ Size * Garage * Bedrooms * Pool, data = train)
stats::step(ancova)

#step suggests an interaction between bedrooms and pool
#try manually
summary(ancova)
#remove four way interaction
ancova_2 <- update(ancova, ~. -Size:Garage:Bedrooms:Pool)
anova(ancova, ancova_2)
summary(ancova_2)
#remove garage/bedrooms/pool interaction
ancova_3 <- update(ancova_2, ~. -Garage:Bedrooms:Pool)
anova(ancova_2, ancova_3)
summary(ancova_3)
#remove size/bedrooms/pool interaction
ancova_4 <- update(ancova_3, ~. -Size:Bedrooms:Pool)
anova(ancova_3, ancova_4)
summary(ancova_4)
#remove size/garage/bedrooms interaction
ancova_5 <- update(ancova_4, ~. -Size:Garage:Bedrooms)
anova(ancova_4, ancova_5)
summary(ancova_5)
#remove size/garage/pool interaction
ancova_6 <- update(ancova_5, ~. -Size:Garage:Pool)
anova(ancova_5, ancova_6)
summary(ancova_6)
#remove size/pool
ancova_7 <- update(ancova_6, ~. -Size:Pool)
anova(ancova_6, ancova_7)
summary(ancova_7)
#remove size/bedrooms interaction
ancova_8 <- update(ancova_7, ~. -Size:Bedrooms)
anova(ancova_7, ancova_8)
summary(ancova_8)
#remove size/garage interaction
ancova_9 <- update(ancova_8, ~. -Size:Garage)
anova(ancova_8, ancova_9)
summary(ancova_9)
#remove garage/bedrooms interaction
ancova_10 <- update(ancova_9, ~. -Garage:Bedrooms)
anova(ancova_9, ancova_10)
summary(ancova_10)
#remove garage/pool interaction
ancova_11 <- update(ancova_10, ~. -Garage:Pool)
anova(ancova_10, ancova_11)
summary(ancova_11)
#remove pool 
ancova_12 <- update(ancova_11, ~. -Pool)
anova(ancova_11, ancova_12)
summary(ancova_12)
#remove size just to see what happens
#one removal too many
ancova_13 <- update(ancova_12, ~. -Size)
anova(ancova_12, ancova_13)

#final model
#adjusted R squared is up to 50% (up from 47%)
summary(ancova_12)
#no heteroscedasticity, normal errors, no influential points
plot(ancova_12)

#Try on test dataset
test$ancova_predictions <- predict(ancova_12, test)
test$ancova_real_predictions <- exp(test$ancova_predictions)
test$ancova_errors <- test$ancova_real_predictions - test$Price

#0.63 correlation (0.6 with linear model)
cor(test$ancova_real_predictions, test$Price)

#what about correlation between independent variables and errors
#nothing for numerical variables
cor(test %>% select(errors, Bedrooms, Size))

#what about garage? visually they overlap but with garage tends to underestimate
plot(test$Garage, 
     test$ancova_errors,
     main = "ANCOVA errors by Garage", 
     xlab = "Garage",
     ylab = "ANCOVA Errors (00k)")

garage_test_2 <- test %>% filter(Garage == T)  %>% select(ancova_errors)
no_garage_test_2 <- test %>% filter(Garage == FALSE)  %>% select(ancova_errors)

#distribution looks non-normal
qqnorm(garage_test_2$ancova_errors, pch = 1, frame = FALSE)
qqline(garage_test_2$ancova_errors, col = "blue", lwd = 2)
qqnorm(no_garage_test_2$ancova_errors, pch = 1, frame = FALSE)
qqline(no_garage_test_2$ancova_errors, col = "blue", lwd = 2)

#use non parametric t-test 
#some evidence to reject null
wilcox.test(ancova_errors ~ Garage, data = test)

#calculate mean squared error, slightly lower for ancova
mse_ancova <- mean((test$Price - test$ancova_real_predictions)^2)
rmse_ancova <- mse_ancova ^ 0.5

#mape, 0.17, same as linear regression
#on average predictions are 17% away from actuals
mape_ancova <- mean(abs((test$ancova_real_predictions - test$Price))/
               test$Price) 

#plot predictions against actual, not that different to linear model
plot(test$ancova_real_predictions, 
     test$Price, 
     xlab = "Predictions (000k)",
     ylab = "Actual (000k)",
     main = "Predictions vs actual (ANCOVA)")
abline(a = 0, b = 1)

#smaller AIC is better, ANCOVA better (only just)
AIC(ancova_12)
AIC(linear_model_3)
anova(linear_model_3, ancova_12)

#### end ####

