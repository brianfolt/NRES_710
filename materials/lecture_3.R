
#  NRES 710, Linear Regression  
#     University of Nevada, Reno
#     Introduction to basic linear regression   

### To learn regression in R, let's simulate data so we know what 'Truth' is

# Set the seed for reproducibility
set.seed(123)

# Simulate a continuous predictor variable, precipitation
n <- 30
precip <- runif(n, min = 0, max = 10)

# Simulate the true, predicted response of biomass to precip (y-hat)
y_hat <- 2 + 3 * precip

# What does this look like?
plot(y_hat ~ precip)

# Simulate error for the response variable
# Let's say the standard deviation = 2, which means that around our line there
# is a 2 kg/ha standard deviation.
# Rule of thumb: 66% of samples within 1 SD, 95% of samples within 2 SD, 99% within SD
# With a SD of 2, 66% of samples will be within 2 units of the line, etc.
error <- rnorm(n, mean = 0, sd = 2)

# Create the response variable, biomass
# biomass = beta0 + beta1 * precip + epsilon
biomass <- y_hat + error

# Create a data frame
datum <- data.frame(precip = precip, y_hat = y_hat, error = error, biomass = biomass)

# Save this dataset
write.csv(datum, "lecture_3_dataset1.csv")

### Analyze the data

# Read in the dataset
datum <- read.csv("lecture_3_dataset1.csv")

# Observe the first few rows of our data
# head(datum)

# First thing we should do when starting an analysis is.. look at our data!
plot(biomass ~ precip, data = datum)

# Ask R for information about how 'lm()' works
help(lm)
?lm()

# Fit the linear model
results <- lm(biomass ~ precip, data = datum)

# Examine the results
summary(results)

# Plot the data with the line of best fit
plot(biomass ~ precip)
abline(results)

# Maybe you are used to seeing ANOVA tables...
anova(results)

# This should look similar to the bottom line from our regression output
