
#  NRES 710, Multi-variable Modeling - Collinearity
#     University of Nevada, Reno

par(mar=c(4,4,0,0))

# Read in the data
datum <- read.csv("lecture_13_dataset1.csv")

# Examine the data
head(datum, 10)

# Plot the data
plot(Size ~ Age, data = datum)

# Run lm() for different pairs of X-variables
results <- lm(Age ~ Sex, data = datum)
summary(results)

# Run lm() for different pairs of X-variables
results <- lm(Age ~ MotherSize, data = datum)
summary(results)

# Create new dataframe without categorical Sex
datum2 <- subset(datum, select = -Sex)
datum2 <- subset(datum2, select = -Size)

# Examine new subsetted data
head(datum2)

# Correlation matrix - runs a simple 'lm()' between each pair of X-variables
cor(datum2)

# Download 'car' by uncommenting this next line and running it through your console
# install.packages("car", dependencies=TRUE)

# Full linear model with all X-variables
results <- lm(Size ~ Age + Sex + MotherSize + FatherSize, data = datum)

# Use the VIF function from 'car'
# using 'car::vif()' will tell R to call the function 'vif()' from the package 'car' specifically
car::vif(results)

# Square-root this to get VIF in terms of Standard Error
car::vif(results)^0.5

# Simple linear model of Size ~ Age
results <- lm(Size ~ Age, data = datum)
summary(results)

# Simple linear model of Size ~ MotherSize
results <- lm(Size ~ MotherSize, data = datum)
summary(results)

# Simple linear model of Size ~ MotherSize
results <- lm(Size ~ Age + MotherSize, data = datum)
summary(results)

# Simple linear model of Size ~ MotherSize
results <- lm(Size ~ Age + Sex + MotherSize + FatherSize, data = datum)
summary(results)



################################ 'Truth' ################################### 
### Lecture 13: code to simulate collinear data and to demonstrate how
### failing to account for collinearity influences estimates and uncertainty

## Simulation Exercise 1

# Set the seed for reproducibility
set.seed(123)

# Number of simulations
s <- 1000

# Empty vectors to save results from each simulation
simple_beta1 <- numeric(s)
simple_se1 <- numeric(s)
simple_p1 <- numeric(s)
multi_beta1 <- numeric(s)
multi_se1 <- numeric(s)
multi_p1 <- numeric(s)
r2 <- numeric(s)
vif <- numeric(s)

# x2 = x1 + error(0, z), where
# z can range from 0.5 (highly correlated to x1) to 20 (not correlated at ~all)
z_values <- seq(0.5, 20, length.out = s)

# Loop through each simulation replicate to measure everything
for (i in 1:s){
  
  # Number of datapoints per simulation
  n <- 100

  # x1
  x1 <- runif(n, 0, 10) # Random, uniform variable; only simulated once

  # error for y; only simulated once
  error <- rnorm(n, 0, 2)
  
  # X2 for each simulation
  x2 <- x1 + rnorm(n, 0, z_values[i])
  
  # Y for each
  y <- 10 + 3 * x1 + 3 * x2 + error

  # Simple model
  results1 <- lm(y ~ x1)
  simple_beta1[i] <- summary(results1)$coefficients[2,1]
  simple_se1[i] <- summary(results1)$coefficients[2,2]
  simple_p1[i] <- summary(results1)$coefficients[2,4]

  # Multi-variable model
  results2 <- lm(y ~ x1 + x2)
  multi_beta1[i] <- summary(results2)$coefficients[2,1]
  multi_se1[i] <- summary(results2)$coefficients[2,2]
  multi_p1[i] <- summary(results2)$coefficients[2,4]

  # R^2 between X-variables
  model_x12 <- lm(x2 ~ x1)
  r2[i] <- summary(model_x12)$r.squared
  
  # VIF for multi-variable model
  vif[i] <- car::vif(results2)["x1"]
}

## Simulation Exercise 2

# Set seed
set.seed(123)

# Number of simulations
s <- 1000

# Empty vectors to save results from each simulation
simple_beta1 <- numeric(s)
simple_se1 <- numeric(s)
simple_p1 <- numeric(s)
simple_beta2 <- numeric(s)
multi_beta1 <- numeric(s)
multi_beta2 <- numeric(s)
multi_se1 <- numeric(s)
multi_p1 <- numeric(s)
r2 <- numeric(s)
vif <- numeric(s)

# x2 = x1 + error(0, z), where
# z can range from 0.5 (highly correlated to x1) to 20 (not correlated at ~all)
z_values <- seq(0.5, 20, length.out = s)

# Loop through each simulation replicate to measure everything
for (i in 1:s){
  
  # Number of datapoints per simulation
  n <- 100

  # x1
  x1 <- runif(n, 0, 10) # Random, uniform variable; only simulated once

  # error for y; only simulated once
  error <- rnorm(n, 0, 2)
  
  # X2 for each simulation
  x2 <- x1 + rnorm(n, 0, z_values[i])
  
  # Y for each
  y <- 10 + 3 * x1 + error

  # Simple model of X1
  results1 <- lm(y ~ x1)
  simple_beta1[i] <- summary(results1)$coefficients[2,1]
  simple_se1[i] <- summary(results1)$coefficients[2,2]
  simple_p1[i] <- summary(results1)$coefficients[2,4]
  
  # Simple model of X2
  results2 <- lm(y ~ x2)
  simple_beta2[i] <- summary(results2)$coefficients[2,1]

  # Multi-variable model
  results3 <- lm(y ~ x1 + x2)
  multi_beta1[i] <- summary(results3)$coefficients[2,1]
  multi_beta2[i] <- summary(results3)$coefficients[3,1]
  multi_se1[i] <- summary(results3)$coefficients[2,2]
  multi_p1[i] <- summary(results3)$coefficients[2,4]

  # R^2 between X-variables
  model_x12 <- lm(x2 ~ x1)
  r2[i] <- summary(model_x12)$r.squared
  
  # VIF for multi-variable model
  vif[i] <- car::vif(results3)["x1"]
}

## Data for analysis in class
# Set the seed for reproducibility & set graphing parameter
set.seed(123)

# This is similar to the Age, Sex, and Size data we simulated for last class.
# There is no collinearity between Age and Sex.

# X-variables
n <- 50
Sex <- c(rep("Female", n), rep("Male", n))
Age <- runif(n * 2, 1, 10)
dummy <- data.frame(model.matrix(~ Sex - 1))
colnames(dummy) <- c("Female", "Male")

# However, there are now two additional X-variables: MotherSize and FatherSize
# And both of those variables are a function of Age
MotherSize <- rnorm(n * 2, Age - 5, 1)
FatherSize <- rnorm(n * 2, Age, 1)

# All of these variables influence size, so they are all examples of confounding variables.

# Simulate error
Error <- rnorm(n * 2, 0, 1.2)

# Predict Y
Response <- 4 + 1.5 * Age + 2.5 * dummy$Male + 0.2 * MotherSize + 0.2 * FatherSize + Error

# Dataframe
datum <- data.frame(Age = Age, Sex = Sex, Male = dummy$Male, MotherSize = MotherSize, FatherSize = FatherSize, Size = Response)

# Save the data
write.csv(datum, "lecture_13_dataset1.csv", row.names = FALSE)
