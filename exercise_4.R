### Exercise 4: code to simulate datasets 

# Set the seed for reproducibility
set.seed(123)

## Dataset 1
# Simulate X-variable
n <- 50
Group <- factor(c(rep("Control", n/2), rep("Exclusion", n/2)))
Exclusion <- c(rep(0, n/2), rep(1, n/2))

# Simulate error
error <- rnorm(n, mean = 0, sd = 1)

# Calculate Y-variable
Density <- 5 + 2.5 * Exclusion + error

# Create dataframe
datum <- data.frame(Group, Exclusion, Density)

# Save the CSV file
write.csv(datum, "exercise_4_dataset1.csv")


## Dataset 2
# Simulate X-variable
Habitat <- c(rep("Agriculture", 10), rep("ClosedPine", 10), rep("OpenPine", 10), rep("Sagebrush", 10))
dummy <- data.frame(model.matrix(~ Habitat - 1))
colnames(dummy) <- c("Agriculture", "ClosedPine", "OpenPine", "Sagebrush")

# Simulate error
Error <- rnorm(length(Habitat), 0, 0.1)

# Predict Y
QuailDensity <- 0.85 - 0.25 * dummy$ClosedPine - 0.35 * dummy$OpenPine - 0.15 * dummy$Sagebrush + Error

# Dataframe
datum <- data.frame(Habitat, QuailDensity)

# Save the CSV file
write.csv(datum, "exercise_4_dataset2.csv")


## Dataset 3
# Simulate X-variable
Hunt <- as.factor(c(rep("Archery", 15), rep("Muzzleloader", 15), rep("Rifle", 15)))
dummy <- data.frame(model.matrix(~ Hunt - 1))
colnames(dummy) <- c("Archery", "Muzzleloader", "Rifle")

# Error
error <- rnorm(length(Hunt), 0, 0.1)

# Success
Success <- abs(0.12 + 0.45 * dummy$Muzzleloader + 0.6 * dummy$Rifle + error)

# Dataframe
datum <- data.frame(Hunt, Success)

# Save the CSV file
write.csv(datum, "exercise_4_dataset3.csv")


## Dataset 4
# x-variable
Fertilizer <- factor(sort(rep((0:5)*2, 5)))
dummy <- data.frame(model.matrix(~ Fertilizer - 1))
colnames(dummy) = c("Zero", "Two", "Four", "Six", "Eight", "Ten")

# Error
Error <- rnorm(length(Fertilizer), mean = 0, sd = 1)

# y-variable
Biomass <- 1.5 + 3.5 * dummy$Two + 5.25 * dummy$Four + 5.25 * dummy$Six + 4 * dummy$Eight + 0.25 * dummy$Ten + Error

# Dataframe
datum <- data.frame(Fertilizer, Biomass)

# Save only the first two columns in the CSV file
write.csv(datum, "exercise_4_dataset4.csv")
