### Exercise 2: code to simulate datasets 

# Set the seed for reproducibility
set.seed(123)

## Dataset 1

# Simulate X-variable
n <- 40
x <- runif(n, 0, 1) * 0.5

# Simulate error
error <- rnorm(n, mean = 0, sd = 0.4)

# Calculate Y-variable
y <- abs(0.1 + 2 * x + error) # abs() gets rid of negative numbers drawn by chance

# Create dataframe
datum <- data.frame(PreyFrogs = x, Spiders = y)

# Save the CSV file
write.csv(datum, "exercise2_dataset1.csv")

## Dataset 2

# Simulate X-variable
n <- 480
x <- round(runif(n, 50, 3000), 0)

# Simulate error
error <- rnorm(n, mean = 0, sd = 5)

# Calculate Y-variable
y <- abs(20 - 0.001 * x + error) # abs() gets rid of negative numbers drawn by chance

# Create dataframe
datum <- data.frame(Elevation = x, Size = y)

# Save the CSV file
write.csv(datum, "exercise2_dataset2.csv")

## Dataset 3

# Simulate the X-variable
n <- 140
Predators <- runif(n, 0, 1) * 20

# Simulate the error
error <- rnorm(n, mean = 0, sd = 120)

# Simulate Y-variable
Time <- round(rep(360, n) + error, 0)

# Create dataframe
datum <- data.frame(Predators = Predators, Time = Time)

# Save the CSV file
write.csv(datum, "exercise2_dataset3.csv")
