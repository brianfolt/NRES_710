
#  NRES 710, Linear Regression (3)
#     University of Nevada, Reno
#     Assumptions of linear regression   

# Load the data
datum <- read.csv("lecture_5_good_data.csv")

# Plot the data
plot(y ~ x, data = datum)

# Load the data
nonlinear <- read.csv("lecture_5_nonlinear_data.csv")

# Plot the data
plot(y ~ x, data = nonlinear)

# Load the data
norm <- read.csv("lecture_5_nonnormal_data.csv")

# Plot the data
plot(y ~ x, data = norm)

# Load the data
hetero <- read.csv("lecture_5_heteroscedastic_data.csv")

# Plot the data
plot(y ~ x, data = hetero)

# Load the data
auto <- read.csv("lecture_5_autocorrelated_data.csv")

# Plot the data
plot(y ~ x, data = auto)

# Fit linear regression models to the five datasets
results <- lm(y ~ x, data = datum)
resultsNonlinear <- lm(y ~ x, data = nonlinear)
resultsNorm <- lm(y ~ x, data = norm)
resultsHetero <- lm(y ~ x, data = hetero)
resultsAuto <- lm(y ~ x, data = auto)

# Examine the residuals (just the first ~20)
residuals(results)[1:20]

# Simple way
plot(residuals(results))

# Correct way to examine residuals plot
plot(residuals(results) ~ datum$x)

# Add a horizontal line at y = 0
abline(a = 0, b = 0)

# Correct way to examine residuals plot
plot(residuals(resultsNonlinear) ~ nonlinear$x)

# Add a horizontal line at y = 0
abline(a = 0, b = 0)

# Correct way to examine residuals plot
plot(residuals(resultsNorm) ~ norm$x)

# Add a horizontal line at y = 0
abline(a = 0, b = 0)

# Correct way to examine residuals plot
plot(residuals(resultsHetero) ~ hetero$x)

# Add a horizontal line at y = 0
abline(a = 0, b = 0)

# Correct way to examine residuals plot
plot(residuals(resultsAuto) ~ auto$x)

# Add a horizontal line at y = 0
abline(a = 0, b = 0)

# Correct way to examine residuals plot
hist(residuals(results))

# Correct way to examine residuals plot
hist(residuals(resultsNorm))

# Correct way to examine residuals plot
hist(residuals(resultsHetero))

# Make two graphs side-by-side
par(mfrow = c(1, 2))

# Default setting
hist(residuals(resultsHetero))

# Revisualizing with more breaks
hist(residuals(resultsHetero), breaks = 10)

# Autocorrelation function for normal data
acf(residuals(results)[order(datum$x)])

# Autocorrelation function for normal data
acf(residuals(resultsAuto)[order(auto$x)])

# Two graphs
par(mfrow=c(1,2))

# Recall the scatterplot for the nonlinear data
plot(y ~ x, data = nonlinear)

# Autocorrelation function with correct order
acf(residuals(resultsNonlinear)[order(nonlinear$x)])




########################## 'Truth' ############################ 
### Code for simulating data that were analyzed in this lecture

# Set the seed for reproducibility
set.seed(123)

## Simulate data with no assumption violations
n <- 100
x1 <- rnorm(n, mean = 20, sd = 10)
y1 <- 5 + 4 * x1 + rnorm(n, mean = 0, sd = 8)

# Create dataframe
datum <- data.frame(x = x1, y = y1)

# Save the CSV file
write.csv(datum, "lecture_5_good_data.csv")


## Simulate data with a violation of normality
n <- 100
x <- rnorm(n, mean = 20, sd = 3)
y <- 10 + 25 * x + rnorm(n, mean = 0, sd = 8)^2

# Create dataframe
datum <- data.frame(x = x, y = y)

# Save the CSV file
write.csv(datum, "lecture_5_nonnormal_data.csv")


## Simulate data with a violation of linearity
n <- 100
x <- runif(n, 0, 10)
#x <- sort(x)
y <- 3 + 2 * x - 0.18 * x^2 + rnorm(n, mean = 0, sd = 1)

# Create dataframe
datum <- data.frame(x = x, y = y)

# Save the CSV file
write.csv(datum, "lecture_5_nonlinear_data.csv")


## Simulate data that are heteroscedastic
n <- 100
x1 <- runif(n, 0, 10)
y1 <- 5 + 4 * x1 + rnorm(n, mean = 0, sd = 1 * x1)

# Create dataframe
datum <- data.frame(x = x1, y = y1)

# Save the CSV file
write.csv(datum, "lecture_5_heteroscedastic_data.csv")


## Simulate data that are autocorrelated
n <- 100
x1 <- runif(n, 0, 10)

# Sort x1 from low to high
x1 <- sort(x1)

# Simulate error for each value using the mean of the previous value
error <- matrix(NA, length(x1), 1)
error[1,1] <- rnorm(1, mean = 0, sd = 1)
for (i in 2:length(x1)){
  error[i,1] <- rnorm(1, mean = error[i-1, 1], sd = 1)
}

# Create y values
y1 <- 3 + 2 * x1 + error

# Create dataframe
datum <- data.frame(x = x1, error = error, y = y1)

# Save the CSV file
write.csv(datum, "lecture_5_autocorrelated_data.csv")
