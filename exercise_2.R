# Set the seed for reproducibility
set.seed(123)

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
