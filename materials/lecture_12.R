
#  NRES 710, Multi-variable Modeling
#     University of Nevada, Reno
#     Introduction to multi-variable modeling

# Load in the data and take a look at it
datum <- read.csv("lecture_12_dataset1.csv")

# Take a look at it
head(datum)
tail(datum)

# Plot the effect of sex on size
plot(Size ~ as.factor(Sex), data = datum)

# Plot the effect of sex on size
plot(Size ~ Age, data = datum)

# Simple regression between size and sex 
results1 <- lm(Size ~ Sex, data = datum)
summary(results1)

# Plot
plot(Size ~ Age, data = datum)

# Simple regression between size and age 
results2 <- lm(Size ~ Age, data = datum)
summary(results2)

# Simple regression between size and age 
results3 <- lm(Size ~ Age + Male, data = datum)
summary(results3)

# Load in the data and take a look at it
datum <- read.csv("lecture_12_dataset2.csv")

# Take a look at it
head(datum)
tail(datum)

# Make sure Sex is a factor
datum$Sex <- as.factor(datum$Sex)

# Plot the data
plot(Size ~ as.factor(Sex), data = datum)

# Plot again
plot(Size ~ Age, data = datum)

# Multi-variable analysis
results <- lm(Size ~ Age + Sex, data = datum)
summary(results)

# Use the relevel function to change order of sex
results2 <- lm(Size ~ Age + relevel(Sex, ref = "Male"), data = datum)
summary(results2)

# Back to original results
summary(results)

# ANOVA
anova(results)

# Switch order of effect
results3 <- lm(Size ~ Sex + Age, data = datum)
anova(results3)

# Simple model
results4 <- lm(Size ~ Age, data = datum)

# More complex model
results5 <- lm(Size ~ Age + Sex, data = datum)

# F-drop test
anova(results4, results5)

# Model
results6 <- lm(Size ~ Sex + Age, data = datum)
summary(results6)

# Tukey
results6 <- aov(results6)
TukeyHSD(results6, which = "Sex")



########################### 'Truth' ############################# 
### Lecture 12: code to simulate data for multi-variable analysis

# Set the seed for reproducibility & set graphing parameter
set.seed(123); par(mar=c(4,4,0,0)); par(mfrow=c(1,2))

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
Response <- 4 + 1.5 * x2 + 2.5 * dummy$Male + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Size = Response)

# Save the data
write.csv(datum, "lecture_12_dataset1.csv", row.names = FALSE)

# Second dataset
# X variable
n <- 50
groups <- 3
x1 <- c(rep("Female", n), rep("Male", n), rep("Hermaphrodite", n))
x2 <- runif(n * groups, 1, 10)
dummy <- data.frame(model.matrix(~ x1 - 1))
colnames(dummy) <- c("Female", "Hermaphrodite", "Male")

# Simulate error
Error <- rnorm(n * groups, 0, 0.8)

# Predict Y
Response <- 4 + 1.5 * x2 + 2.5 * dummy$Male + 5 * dummy$Hermaphrodite + Error

# Dataframe
datum <- data.frame(Age = x2, Sex = x1, Male = dummy$Male, Herma = dummy$Hermaphrodite, Size = Response)

# Save the data
write.csv(datum, "lecture_12_dataset2.csv", row.names = FALSE)
