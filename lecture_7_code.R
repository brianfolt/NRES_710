### Code for testing assumptions of regression in R

# There are five example datasets we can use:
# lecture_7_good_data.csv
# lecture_7_nonlinear_data.csv
# lecture_7_nonnormal_data.csv
# lecture_7_heteroscedastic_data.csv
# lecture_7_autocorrelated_data.csv

# Manually import the data
datum <- read.csv(file.choose())

# Examine the data
head(datum)
summary(datum)

# The procedures below require us to run a regression first
# Fit a linear regression model to the data and examine the results
results <- lm(Y ~ X, data = datum)
summary(results)


## Testing of assumptions

# Continuous variable - no need to test, should be obvious

# Scatterplot - good for testing all assumptions
plot(Y ~ X, data = datum) # Scatterplot
abline(results) # Add line of best fit to the plot
# if non-linear relationship, will have groups of points above and below line
# if heteroscedastic data, variance in error will not be constant across x values
# if non-normally distributed residuals, error will be skewed above or below the line
# if autocorrelated data, points will seem to follow each other in a path

## Residuals plot - good for testing all assumptions
residuals(results) # Extract residual error for each point
plot(residuals(results) ~ datum$X) # Plot residuals in order of X values
# regression line is now horizontal at Y = 0
# if non-linear relationship, will have groups of points above and below line
# if heteroscedastic data, variance in error will not be constant across x values
# if non-normally distributed residuals, error will be skewed above or below the line
# if autocorrelated data, points will seem to follow each other in a path

## Histogram of residuals - good for testing normality assumption
# Note that this is testing whether residuals are global normal (across all x); not whether residuals are locally normal
help(hist) # help file for histograms
hist(residuals(results)) # draw a histogram of residuals
hist(residuals(results), breaks=10) # the 'breaks' argument changes the number of bars
# if non-normally distributed residuals, histogram will appear non-normal

## Autocorrelation function (ACF)
help(acf) # help file for autocorrelation function
acf(residuals(results)) # runs autocorrelation function (ACF) on residuals from regression
# to test for autocorrelation, residuals must be in order that autocorrelation might exist (usually x-order)
acf(residuals(results)[order(datum$X)]) # sorts residuals into x-order
# if autocorrelated data, lines at x=1,2,3,.... will cross dotted horizontal line, indicated significant autocorrelation
# Note that violating assumption of non-linearity can also appear as autocorrelation.