# Real Estate

## Summary
Explore a real estate dataset using EDA and statistics. The goal is to investigate which attributes have an impact on house prices.

### Steps taken

- Step 1: Import the data from Excel and explore datatypes
- Step 2: Plot data to look at price distribution and see which attributes affect price
- Step 3: Use descriptive statistics to add numerical insight to plots
- Step 4: Assess whether the normal distribution is a good approximation for price
- Step 5: Use two sample t-tests to see whether homes with a pool or garage have a higher price
- Step 6: Use ANOVA to see whether mean house price differs by township
- Step 7: Use correlation to assess the relationship between price, size and distance from town
- Step 8: Try fitting a linear regression model to predict house price from a combination of variables
- Step 9: See if ANCOVA performs better than linear regression 

## Findings

### [1] Price is approximately normally distributed
The mean house price is $221k and the median house price is $213k suggesting a mild positive skew. This can be seen visually in the plot below (the red line shows what the normal distribution would look like if it had the same mean and standard deviation as price). Calculating skewness gives a value of 0.46 so we can use the Empirical Rule to conclude 95% of prices will lie within two standard deviations of the mean ($126k to $315k).

![image](https://github.com/user-attachments/assets/8ebfea9e-3579-4f74-812f-b49b692a2607)

### [2] Distance from town and having a garage seem to impact price
Plotting all variables against price showed that homes closer to town and homes with a garage seemed to be worth more (see plots below). 

![image](https://github.com/user-attachments/assets/d1b17c77-ebb0-49fc-b68e-7ef9d56e372d)
![image](https://github.com/user-attachments/assets/c49c67d1-f36a-40e5-94f0-bb0069a73661)

The number of bedrooms and bathrooms only has a noticeable impact on price at the extreme end (see plots below). The bedroom data only includes 4 homes with 7 or 8 bedrooms so any analysis would need to take this into account.
![image](https://github.com/user-attachments/assets/93f80b06-bacb-453a-b742-5b707d5d4ac1)
![image](https://github.com/user-attachments/assets/4b376baf-9fc0-460d-8c28-dd94414bfc63)

Other relationships were more surprising. For example, homes with pools had a lower median and the size only seemed to have a weak positive relationship with price. There wasn't a noticeable relationship between price and township. 

### [3] Statistical tests provide evidence that having a garage or a pool affects house price
Performing a two sample t-test allowed us to reject the null hypothesis that the mean price between homes with and without a garage is the same (see results below). From the boxplot above we can see that homes with a garage have a higher mean. Note: we used Welch's test where variances were unequal. 

![image](https://github.com/user-attachments/assets/69d7fcd5-14af-4d88-a50e-709d5d63ddd6)

There was also a statistically significant result for homes with and without a pool (see results below). However, in this case the opposite was true. Homes with a pool had a smaller mean than homes without. 

![image](https://github.com/user-attachments/assets/04cb8275-d893-4ffd-87b5-270a2c0b177b)

An ANOVA test confirmed what we observed in the boxplot above. There wasn't enough evidence to reject the null hypothesis that the mean price isn't affected by township. 

![image](https://github.com/user-attachments/assets/284e3b4c-5886-4205-811f-000125a4053b)

Correlation tests showed statistically significant relationships between price/distance and price/size. However, both of the relationships are moderate rather than strong (see below).

![image](https://github.com/user-attachments/assets/2bf76d25-1689-4290-963b-766452affb3d)

![image](https://github.com/user-attachments/assets/4135a6b0-ff63-40f1-932e-8ef8af9a101c)


### [4] First attempt at a linear regression model doesn't explain enough price variation
Based on the statistical tests in part 3 a linear regression model was fitted to predict house prices based on distance, size, garage and pool. The results are shown below.

![image](https://github.com/user-attachments/assets/9adceda5-1090-43ba-9600-8c54e2596045)

Only 45% of the variation in price was explained by these variables, the Residual Standard Error was high and there were signs of heteroscedasticity (see below). Heteroscedasticity persisted after a log transform of price.

![image](https://github.com/user-attachments/assets/a1a330a6-bb3b-4afb-9c0b-89418fa64eef)

Adding quadratic terms to the model showed no improvement and no significance. 

The next step was to plot a tree to determine if any other variables were impacting price (see below).
![image](https://github.com/user-attachments/assets/4e2d680d-a5cc-45f7-8410-175b6ab92c91)

Bedrooms seemed to impact price and hadn't yet been included in the model. As mentioned above only a small number of houses have lots of bedrooms so we checked that high values were represented in both train and test datasets and there were no influential points skewing the model. An ANOVA test showed the model improvement and heteroscedasticity was no longer a problem. 

![image](https://github.com/user-attachments/assets/282342a5-d558-4fa0-ab07-27ca18614185)

There was a slight worsening after removing distance but it wasn't significant so this was our chosen model (predicting price based on size, garage, pool and bedrooms). 

![image](https://github.com/user-attachments/assets/2e8c273e-0d83-4d16-b2bf-528e3c2c3da3)

The model was tested on new data and the Mean Absolute Percentage Error was 0.17. This meant, on average, predictions differed from actuals by 17%. The graph below shows this graphically. Not a bad first attempt but room for improvement.

![image](https://github.com/user-attachments/assets/f8eb03eb-8dc5-454c-8f7b-f28afb4e6abe)

### [5] ANCOVA is a slight improvement on linear regression
An ANCOVA model was fitted on the same significant variables used in the linear regression model. Variables were removed one at a time until only significant variables remained. The final model results can be seen below. 

![image](https://github.com/user-attachments/assets/748b0fb0-ead3-4a41-8210-4b58444b3857)

This time 50% of the variation in price can be explained (up from 47% in linear regression). There was no problem with heteroscedasticity, non-normal errors or influential points skewing the model. 

The Mean Squared Error and AIC were slightly lower for the ANCOVA model but the Mean Absolute Percentage Error was the same at 0.17. The graph below plots the actual and predicted values. It isn't significantly different from the linear regression approach (confirmed in an ANOVA test). 

![image](https://github.com/user-attachments/assets/481a7366-56d3-4c3a-a542-2f4d9493e157)

![image](https://github.com/user-attachments/assets/c4ac1ecb-9275-407b-b46f-8d1090021f60)

Note that both the linear model and ANCOVA model showed some relationship between the errors and whether the house had a garage. More specifically, the models underestimated the price of houses with a garage. 

A Generalized Additive Model was also fitted to the data but it had the same explanatory power as the ANCOVA model so the simpler ANCOVA model was the chosen model.


