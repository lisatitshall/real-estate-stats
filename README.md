# Real Estate

## Summary
Explore a real estate dataset using EDA and statistics. The goal is to investigate which attributes have an impact on house prices.

### Steps taken

- Step 1: Import the data from Excel and explore datatypes
- Step 2: Plot data to look at price distibution and see which attributes seem to affect price
- Step 3: Use descriptive statistics to add numerical insight to plots
- Step 4: Assess whether the normal distribution is a good approximation for price
- Step 5: Construct a 95% confidence interval for price and use one sample hypothesis testing to test a statement about the mean price
- Step 6: Use two sample hypothesis tests to see whether homes with a pool or garage have a higher price
- Step 7: Use ANOVA to see whether mean house price differs by township

## Findings

### [1] Price is approximately normally distributed
The mean house price is $221k and the median house price is $213k suggesting a mild positive skew. This can be seen visually in the plot below (the red line shows what the normal distribution 
would look like if it had the same mean and standard deviation as price). Calculating skewness gives a value of 0.46 so we can use the Empirical Rule to conclude 95% of prices will lie within
two standard deviations of the mean ($126k to $315k).

![image](https://github.com/user-attachments/assets/8ebfea9e-3579-4f74-812f-b49b692a2607)
