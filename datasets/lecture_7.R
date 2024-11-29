
#  NRES 710, Analysis of Categorical Data
#     University of Nevada, Reno
#     Testing between two groups

### Code for simulating data to be analyzed body size data for two sexes

# Set the seed for reproducibility
set.seed(123)

# Simulate the female data
n <- 20 # sample size
Sex <- rep("Female", n) # female category
Sigma <- 20 # standard deviation of the error (noise) in data
Error <- rnorm(n, mean = 0, sd = Sigma) # simulate the error (noise)
Size <- 200 + Error # make the response variable data
FemaleData <- cbind(Sex, Size)

# Simulate the male data
n <- 20 # sample size
Sex <- rep("Male", n) # female category
Sigma <- 20 # standard deviation of the error (noise) in data
Error <- rnorm(n, mean = 0, sd = Sigma) # simulate the error (noise)
Size <- 250 + Error # make the response variable data
MaleData <- cbind(Sex, Size)

# Bind the data together
datum <- data.frame(rbind(FemaleData, MaleData))

# Make the 'Sex' variable a factor
datum$Sex <- as.factor(datum$Sex)

# Force to 'Size' to numeric and round to 1 digit after the decimal
datum$Size <- round(as.numeric(datum$Size), 1)

# Save the data
write.csv(datum, "lecture_7_seal_data.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_7_seal_data.csv")

### Load in the data
datum <- read.csv("lecture_7_seal_data.csv")

# Examine the data
head(datum)
tail(datum)
str(datum)

# Force 'Sex' to be a factor
datum$Sex <- as.factor(datum$Sex)

# Plot the data to examine it!
plot(Size ~ Sex, data = datum)
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
# Nothing pops up. Some functions in R don't have summary functions for them because they are so simple!

# Just ask for the object
results

# Difference between groups
248.9 - 202.8

# Use confidence limits of difference between groups to calculate confidence intervals
(57.73 - 34.55)/2

# Males are 46.1 kg (+/-11.6; 95% CI) heavier in size than females (p = 1.101e-09).

### Code for simulating data to be analyzed body size data for two sexes

# Recall our 'datum' object
head(datum)
tail(datum)

# We need to 'dummy-code' our Sex variable, e.g., as 'Male'
Male <- c(rep(0, n), rep(1, n))

# Add 'Male' to the dataframe
datum <- cbind(datum, Male)

# Aside: we can use dummy-coded sex variable to simulate data much easier
Size <- 200 + 50 * Male + rnorm(n*2, 0, Sigma)
Size
# Plot the data!
plot(Size ~ Male, data = datum)

# Examine the old t-test results
results
248.9 - 202.8 # effect of being male

# Use lm() to run regression with dummy-coded X-data
results2 <- lm(Size ~ Male, data = datum)
summary(results2)

# Confidence intervals
confint(results2)

# Regression with a categorical variable
results3 <- lm(Size ~ Sex, data = datum)
summary(results3)

