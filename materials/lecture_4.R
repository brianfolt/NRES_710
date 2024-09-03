
#  NRES 710, Linear Regression 2 
#     University of Nevada, Reno
#     Presenting results of linear regression   

### Simulate data

# Set the seed for reproducibility
set.seed(123)

# Simulate a continuous predictor variable, precipitation
n <- 30
precip <- runif(n, min = 0, max = 10)

# Simulate the true, predicted response of biomass to precip (y-hat)
y_hat <- 2 + 3 * precip

# Simulate error for the response variable
error <- rnorm(n, mean = 0, sd = 2)

# Create the response variable, biomass
# biomass = beta0 + beta1 * precip + epsilon
biomass <- y_hat + error

# Create a data frame
datum <- data.frame(precip = precip, y_hat = y_hat, error = error, biomass = biomass)

# Save the datafile
write.csv(datum, "lecture_4_dataset1.csv")


### Analyze data

# Read in the data
datum <- read.csv("lecture_4_dataset1.csv")

# Fit the linear model
results <- lm(biomass ~ precip, data = datum)

# Examine the results
summary(results)

# Print the confidence intervals
confint(results)
# These are actually the confidence limits! Annoying
# How do we calculate the confidence intervals

# Print the confidence intervals
confint(results)
# These are actually the confidence limits! Annoying

(3.071 - 2.556) / 2
3.071 - 2.814

