
#  NRES 710, Analysis of Categorical Data
#     University of Nevada, Reno
#     Testing between two groups

### Code for simulating data to be analyzed body size data for two sexes

# Set the seed for reproducibility
set.seed(123)

# Simulate the binomial X-variable (sex)
n <- 40
x <- c(rep("Female", n/2), rep("Male", n/2))
x <- factor(x)

# Simulate continuous y-variable data
y <- ifelse(x == "Female",
            rnorm(n/2, mean = 200, sd = 20), #females
            rnorm(n/2, mean = 250, sd = 20)) #males

# Create dataframe
datum <- data.frame(Sex = x, Size = y)

# Examine the data
head(datum)
tail(datum)

# Plot the data to examine it!
plot(Size ~ Sex, data=datum)
# Box and whiskers plot!
# Bold black line = median
# Edges of box: 75% and 25% quartiles
# Bars: 95% limits
# Points: outliers, or 5% of data outside of the 95% intervals

# Analyze this using the 't.test()' function in R:
help(t.test)
results <- t.test(Size ~ Sex, data = datum)

# Examine the summary
summary(results)
# Some functions in R don't have summary functions for them because they are so simple!

# Just ask for the object
results

251.2 - 198.9

(64.44 - 40.14)/2

# Plot
stripchart(Size ~ Sex, data = datum, vertical = TRUE, method = "jitter", 
           pch = 19, xlab = "Sex", ylab = "Body size (cm)")

### Code for simulating data to be analyzed body size data for two sexes

# Recall our 'datum' object
head(datum)
tail(datum)

# We need to 'dummy-code' our Sex variable, e.g., as 'Male'
Male <- c(rep(0, n/2), rep(1, n/2))

# Add 'Male' to the dataframe
datum <- cbind(datum, Male)

# Plot the data!
plot(Size ~ Male, data = datum)

# Examine the old t-test results
results
248.9749 - 202.8325 # effect of being male

# Use lm() to run regression with dummy-coded X-data
results2 <- lm(Size ~ Male, data = datum)
summary(results2)

# Confidence intervals
confint(results2)

# Regression with a categorical variable
results3 <- lm(Size ~ Sex, data = datum)
summary(results3)

