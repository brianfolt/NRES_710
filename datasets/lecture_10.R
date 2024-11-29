
#  NRES 710, Analysis of Categorical Data (cont.)
#     University of Nevada, Reno
#     Continuous or Categorical Data?

### Code for F-Drop tests

# Read in the data
datum <- read.csv("lecture_10_dataset1.csv")
head(datum)

# Plot the data
plot(Biomass ~ Bison, data = datum)
# In this case, R saws numbers in the 'Bison' column and treated the variable as continuous

# Plot the data with X as a categorical variable
plot(Biomass ~ as.factor(Bison), data = datum)

# Run a regression, where we treat Bison as continuous
results <- lm(Biomass ~ Bison, data = datum)
summary(results)

# Run an ANOVA - treat Bison as categorical
results2 <- lm(Biomass ~ as.factor(Bison), data = datum)
# use the as.factor function to get R to treat 'Bison' as continuous
summary(results2)

# Conduct the F-drop test
anova(results2, results)
# The two models we have are the arguments
# Usually, you should list the more complicated model first (the one with more betas)
# Note: the two models can't have the same number of parameters
# A significant p-value means the more complex model is a significant improvement in fit
# A non-significant p-value means the simpler model is adequate
# Note: RSS (which is the same thing as SSE) is always lower in the more complex model

# Read in the data
datum <- read.csv("lecture_10_dataset2.csv")
head(datum)

# Plot the data
plot(Biomass ~ Bison, data = datum)

# BONUS: How to fit a quadratic curve to data when x is continuous

# Fit a linear model
results <- lm(Biomass ~ Bison, data = datum)

#Fit a quadratic curve to data when x is continuous
results3 <- lm(Biomass ~ Bison + I(Bison^2), data = datum)
anova(results3, results)

# Read in dataset 1
datum <- read.csv("lecture_10_dataset1.csv")

# Simple linear model
results <- lm(Biomass ~ Bison, data = datum)
summary(results)

# Build four models varying in complexity
results0 <- lm(Biomass ~ 1, data = datum) # no effects; 'model of mean'
summary(results0)
results1 <- lm(Biomass ~ Bison, data = datum) # simple linear model
summary(results1)
results1b <- lm(Biomass ~ I(Bison^2), data = datum) # model with quadratic term only
summary(results1b)
results2 <- lm(Biomass ~ Bison + I(Bison^2), data = datum) # linear + quadratic model
summary(results2)

# F-drop test comparing results0 to results1
anova(results1, results0) # p-value here is same as marginal p-val from summary(results1)
anova(results2, results1) # p-value here is same as marginal p-val from summary(results2)
anova(results1b, results0) # p-value here is same as marginal p-val from summary(results1b)
anova(results2, results1b) # p-value here is same as marginal p-val from summary(results2)

# So, the marginal p-values from our 'summary()' results are F-drop tests
# comparing a more complex model w/ the extra effect to more simple models
# without the extra effect. And, the order that the variables are included in
# the models doesn't matter!

# Another example: compare model with bison treated as categorical variable to a
# model with no effect of bison
results3 <- lm(Biomass ~ as.factor(Bison), data = datum)
summary(results3)
anova(results3, results0)
# p-value here is same as ANOVA p-value from bottom of summary(results3),
# so F-drop test can also evaluate significance of categorical variables with multiple
# betas


###################### 'Truth' ######################## 
### Lecture 10: code to simulate data for F-drop tests 

# Set the seed for reproducibility
set.seed(111)

## First dataset
# X variable
Bison <- c(rep(0, 8), rep(1, 8), rep(2, 8), rep(3, 8), rep(4, 8))

# Error
error <- rnorm(length(Bison), 0, 0.5)

# Y variable
Biomass <- 6 - 0.8*Bison + error

# Create dataframe
datum <- data.frame(Bison, Biomass)

# Save the CSV file
write.csv(datum, "lecture_10_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_10_dataset1.csv")


## Second dataset
# X variable
n <- 25
x <- c(rep(0, 5), rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5))

# Error
error <- rnorm(n, 0, 0.2)

# Dummy code x
dummy <- data.frame(x = as.factor(x))
dummy <- data.frame(model.matrix(~ dummy$x - 1, data=dummy))
colnames(dummy) <- c("Zero", "One", "Two", "Three", "Four")

# Simulate continuous y-variable data
y <- 2 + dummy$One * 1 + dummy$Two * 2 + dummy$Three * 1 + dummy$Four * 0 + error

# Create dataframe
datum <- data.frame(Bison = x, Biomass = y)

# Save the CSV file
write.csv(datum, "lecture_10_dataset2.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_10_dataset2.csv")


