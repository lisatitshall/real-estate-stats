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
The mean house price is $221k and the median house price is $213k suggesting a mild positive skew. This can be seen visually in the plot below (the red line shows what the normal distribution would look like if it had the same mean and standard deviation as price). Calculating skewness gives a value of 0.46 so we can use the Empirical Rule to conclude 95% of prices will lie within two standard deviations of the mean ($126k to $315k).

![image](https://github.com/user-attachments/assets/8ebfea9e-3579-4f74-812f-b49b692a2607)

### [2] Distance from town and having a garage seem to impact price
Plotting all variables against price showed that homes closer to town and homes with a garage seemed to be worth more (see plots below). 
![image](https://github.com/user-attachments/assets/d1b17c77-ebb0-49fc-b68e-7ef9d56e372d)
![image](https://github.com/user-attachments/assets/c49c67d1-f36a-40e5-94f0-bb0069a73661)

The number of bedrooms and bathrooms only has a noticeable impact on price at the extreme end (see plots below). The bedroom data only includes 4 homes with 7 or 8 bedrooms so any analysis would need to take this into account.
![image](https://github.com/user-attachments/assets/93f80b06-bacb-453a-b742-5b707d5d4ac1)
![image](https://github.com/user-attachments/assets/4b376baf-9fc0-460d-8c28-dd94414bfc63)

Other relationships were more surprising. For example, homes with pools had a lower median and the size only seemed to have a weak positive relationship with price.

![image](https://github.com/user-attachments/assets/86ccf4f1-2710-428e-a76f-f40ffe4195d4)
![image](https://github.com/user-attachments/assets/dc1b4205-732b-4139-a53b-fd1633a2b2ed)

### [3] 



