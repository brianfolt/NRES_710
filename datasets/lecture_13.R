
#  NRES 710, Multi-Variable Modeling - Interactions
#     University of Nevada, Reno
#     Modeling interactions between X-variables
#  NOTE: to print as a DOCX, need to comment out the two 'plotly' 3D graphs

### Dataset 1: age + sex + age*sex
# This is similar to the Age, Sex, and Size data we simulated for in Lecture 12.
# There is no collinearity between Age and Sex, but now there is an interaction
# between Sex and Age.

# First dataset
# X variable
n <- 50
x1 <- c(rep("Female", n), rep("Male", n))
x2 <- runif(n * 2, 1, 10)
dummy <- data.frame(model.matrix(~ x1 - 1))
colnames(dummy) <- c("Female", "Male")

# Simulate error
Error <- rnorm(n * 2, 0, 0.8)

# Predict Y
Response <- 1 + 2 * x2 + 4 * dummy$Male + 1 * x2 * dummy$Male + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Size = Response)

# These data were saved as 'lecture_13_dataset.csv'

# Load the data
datum <- read.csv("lecture_13_dataset.csv")
head(datum)

# Plot the data
plot(Size ~ Age, data = datum)

# Fit a lm() without the interaction
results <- lm(Size ~ Age + Sex, data = datum)
summary(results)
# Fit a lm() with the interaction
results <- lm(Size ~ Age + Sex + Age:Sex, data = datum)
summary(results)
# Fit a lm() for males only
resultsMale <- lm(Size ~ Age, data = subset(datum, datum$Sex == "Male"))
summary(resultsMale)
# Confint for males
confint(resultsMale)
# Fit a lm() for females only
resultsFemale <- lm(Size ~ Age, data = subset(datum, datum$Sex == "Female"))
summary(resultsFemale)
confint(resultsFemale)
# Fit a lm() with interactions using a *
results <- lm(Size ~ Age*Sex, data = datum)
summary(results)
################### 'Truth' ####################
### Lecture 13: code to simulate data for class

# Set the seed for reproducibility
set.seed(123)

### Dataset 1: age + sex + age*sex
# This is similar to the Age, Sex, and Size data we simulated for in Lecture 12.
# There is no collinearity between Age and Sex, but now there is an interaction
# between Sex and Age.

# First dataset
# X variable
n <- 50
x1 <- c(rep("Female", n), rep("Male", n))
x2 <- runif(n * 2, 1, 10)
dummy <- data.frame(model.matrix(~ x1 - 1))
colnames(dummy) <- c("Female", "Male")

# Simulate error
Error <- rnorm(n * 2, 0, 0.8)

# Predict Y
Response <- 4 + 1.5 * x2 + 2.5 * dummy$Male + 1 * x2 * dummy$Male + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Size = Response)

# Save the data
write.csv(datum, "../datasets/lecture_13_dataset1.csv", row.names = FALSE)


### Dataset 2: age + sex + age*sex, with a negative interaction effect
# First dataset
# X variable
n <- 50
x1 <- c(rep("Female", n), rep("Male", n))
x2 <- runif(n * 2, 1, 10)
dummy <- data.frame(model.matrix(~ x1 - 1))
colnames(dummy) <- c("Female", "Male")

# Simulate error
Error <- rnorm(n * 2, 0, 0.8)

# Predict Y
Response <- 4 + 1.5 * x2 + 5 * dummy$Male + -1.5 * x2 * dummy$Male + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Size = Response)

# Save the data
write.csv(datum, "../datasets/lecture_13_dataset2.csv", row.names = FALSE)


### Dataset 3:
# Set the seed for reproducibility
set.seed(123)

# Sample size
n <- 90

# Simulate X-variables
# Continuous variables: Latitude and Elevation
Latitude <- runif(n, 0, 1) * 30
Elevation <- runif(n, 0, 1) * 30

# Response variable: Size
Size <- 3.33*Latitude + 1.6*Elevation

# Create dataframe
datum <- data.frame(Latitude=Latitude, Elevation=Elevation, Size=Size)

# Save the CSV file
write.csv(datum, "../datasets/lecture_13_dataset3.csv")


### Dataset 4: size ~ latitude + elevation + latitude*elevation
# Set the seed for reproducibility
set.seed(123)

## Dataset 4
# Sample size
n <- 90

# Simulate X-variables
# Continuous variables: Latitude and Elevation
Latitude <- runif(n, 0, 1) * 30
Elevation <- runif(n, 0, 1) * 30

# Response variable: Size
Size <- 3.33*Latitude + 1.6*Elevation - 0.08 * Latitude * Elevation

# Create dataframe
datum <- data.frame(Latitude=Latitude, Elevation=Elevation, Size=Size)

# Save the CSV file
write.csv(datum, "../datasets/lecture_13_dataset4.csv", row.names = FALSE)


### Dataset for class exercise: age + sex + age*sex
# This is similar to the Age, Sex, and Size data we simulated for in Lecture 12.
# There is no collinearity between Age and Sex, but now there is an interaction
# between Sex and Age.

# First dataset
# X variable
n <- 50
x1 <- c(rep("Female", n), rep("Male", n))
x2 <- runif(n * 2, 1, 10)
dummy <- data.frame(model.matrix(~ x1 - 1))
colnames(dummy) <- c("Female", "Male")

# Simulate error
Error <- rnorm(n * 2, 0, 0.8)

# Predict Y
Response <- 1 + 2 * x2 + 4 * dummy$Male + 1 * x2 * dummy$Male + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Size = Response)

# Save as CSV
write.csv(datum, "../datasets/lecture_13_dataset.csv")
