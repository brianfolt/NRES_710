### Exercise 3: code to simulate datasets 

# Set the seed for reproducibility
set.seed(123)

## Dataset 1
# Simulate X-variable
n <- 150
x <- runif(n, 0, 10)

# Simulate error
error <- rep(NA, n)
for (i in 1:n){
  error[i] <- rnorm(1, mean = 0, sd = 0.5 * x[i])
}

# Calculate Y-variable
y <- abs(2 + 0.5 * x + error) # abs() gets rid of negative numbers drawn by chance

# Create dataframe
datum <- data.frame(PreyDens = x, PredRate = y)

# Save the CSV file
write.csv(datum, "exercise3_dataset1.csv")


## Dataset 2
# Simulate x-variable
n <- 70
x <- seq(0, n, by = 1)

# Simulate error
error <- rep(NA, n + 1)
error[1] <- 0
for (i in 2:(n + 1)){
  error[i] <- rnorm(1, mean = error[i-1], 2)
}

# Calculate y
y <- 2 + 2 * x + error

# Create dataframe
datum <- data.frame(Age = x, BasalArea = y)

# Save the CSV file
write.csv(datum, "exercise3_dataset2.csv")


## Dataset 3
# Simulate x-variable
n <- 100
x <- runif(n, 0, 10)

# Error
error <- rnorm(n, mean = 0, 1)

# Y variable
y <- round(1 + 0.5 * x + error^2, 0)
  
# Dataframe
datum <- data.frame(PreyDensity = x, LitterSize = y)

# Save the CSV file
write.csv(datum, "exercise3_dataset3.csv")


## Dataset 4
# Simulate x-variable
n <- 100
x <- runif(n, 0, 10)

# Error
error <- rnorm(n, mean = 0, 0.05)

# Y variable
y <- 0.2 + 0.1 * x - 0.01 * x^2 + error

# Dataframe
datum <- data.frame(RoadDensity = x, ProbUse = y)

# Save the CSV file
write.csv(datum, "exercise3_dataset4.csv")


## Dataset 5
# Simulate x-variable
n <- 50

# X variable
x <- runif(n, 0, 5)

# error
error <- rnorm(n, 0, 2)

# Y variable
y <- abs(8 - 1*x + error)

# Create dataframe
datum <- data.frame(DeerDensity = x, CowsKilled = y)

# Save the CSV file
write.csv(datum, "exercise3_dataset5.csv")
