
#  NRES 710, Analysis of Categorical Data (cont.)
#     University of Nevada, Reno
#     Presenting results and testing among multiple groups

# Load the data
datum <- read.csv("lecture_7_seal_data.csv")

# Examine it
head(datum)
tail(datum)

# Examine structure of data
str(datum)

# Force 'Sex' to be a factor
datum$Sex <- factor(datum$Sex)

# Scatterplot
plot(Size ~ Sex, data = datum)

# Fit a linear model
results <- lm(Size ~ Sex, data = datum)
summary(results)
# Confidence intervals
confint(results)

# Calculate confidence intervals
57.7 - 46.1
# simple way: upper limit minus mean
confint(results)["SexMale", 2] - results$coefficients["SexMale"]
# extracting estimates from objects to do math
# Examine the categorical data
str(datum$Sex)
# The 'levels' are the groups in the variable, and they are ordered alphabetically by default.

# Re-order the levels
datum$Sex <- factor(datum$Sex, levels = c("Male", "Female")) # switch the order

# Re-run model
results2 <- lm(Size ~ Sex, data = datum)
summary(results2)
### Code to test for differences among three groups

# Read in the data
datum <- read.csv("lecture_8_dataset1.csv")

# Examine the data
head(datum)

# Make datum a factor
datum$Age <- factor(datum$Age)

# Plot
plot(Size ~ Age, data = datum, xlab = "Age", ylab = "Body size (cm)")

# Plot w/ jitter
stripchart(Size ~ Age, data = datum, vertical = TRUE, method = "jitter",
           pch = 19, xlab = "Age", ylab = "Body size (cm)")

# Re-order categorical X-data
datum$Age <- factor(datum$Age, levels = c("Juvenile", "Subadult", "Adult"))

# Plot
stripchart(Size ~ Age, data = datum, vertical = TRUE, method = "jitter",
           pch = 19, xlab = "Age", ylab = "Body size (cm)")

# Add in dummy-coding
dummy <- model.matrix(~ Age -1, data = datum)
colnames(dummy) <- c("Juvenile", "Subadult", "Adult") # rename columns
# be careful; make sure name order reflects order of categorical variable 'Age'
datum <- cbind(datum, dummy) # bind to dataframe

# Re-examine data
head(datum)

## Analysis of variance; 'aov()'
help(aov)
# Fit an analysis of variance... by calling 'lm()'!!

# Run an ANOVA
results <- aov(Size ~ Age, data = datum)
summary(results)

## Multiple comparison using 'lm()'
# Run analysis
results2 <- lm(Size ~ Age, data = datum)
summary(results2)

# Probability of committing no errors in three t-tests
(1 - 0.05)^3

# Probability of committing >=1 Type I errors in three t-tests
1 - (1 - 0.05)^3

# ANOVA test
anova(results2)

summary(results2)
confint(results2)

# Re-run analysis with different reference
results3 <- lm(Size ~ relevel(Age, ref = "Subadult"), data = datum)
summary(results3)

# Using the dummy-coded variables to run this analysis
results4 <- lm(Size ~ Subadult + Adult, data = datum)
summary(results4)

# Changing the reference
results5 <- lm(Size ~ Juvenile + Adult, data = datum)
summary(results5)

# Using the dummy-coded variables to run this analysis
results6 <- lm(Size ~ Subadult + Adult + Juvenile, data = datum)
summary(results6)

# Using the dummy-coded variables to run this analysis
results7 <- lm(Size ~ Adult, data = datum)
summary(results7)

# Using the dummy-coded variables to run this analysis
results8 <- lm(Size ~ I(Juvenile + Subadult), data = datum)
# I() is a function to do whatever is inside the parentheses
summary(results8)



############################### 'Truth' ################################ 
### Code for simulating data to be analyzed body size data for two sexes

# Simulate the binomial X-variable (sex)
set.seed(123)
n <- 60
x <- c(rep("Juvenile", n/3), rep("Subadult", n/3), rep("Adult", n/3))
x <- factor(x, levels = c("Juvenile", "Subadult", "Adult"))

# Simulate continuous y-variable data
y <- ifelse(x == "Juvenile", rnorm(n/3, mean = 75, sd = 20), #juveniles
            ifelse(x == "Subadult", rnorm(n/3, mean = 200, sd = 20), #subad
              rnorm(n/2, mean = 250, sd = 20))) #adults

# Create dataframe
datum <- data.frame(Age = x, Size = y)

# Save these data for future use
write.csv(datum, "../datasets/lecture_8_dataset1.csv")
