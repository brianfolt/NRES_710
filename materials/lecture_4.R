
#  NRES 710, Linear Regression 2 
#     University of Nevada, Reno
#     Presenting results of linear regression   

### Analyze data

# Read in the data from Tuesday: precipitation effects on biomass
datum <- read.csv("lecture_3_dataset1.csv")

# Fit the linear model
results <- lm(biomass ~ precip, data = datum)

# Examine the results
summary(results)

# Print the confidence intervals
confint(results)

# Calculating the 95% confidence intervals
(3.071 - 2.556) / 2
3.071 - 2.814

