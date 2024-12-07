
#  NRES 710, Analysis of Categorical Data (cont.)
#     University of Nevada, Reno
#     Post-hoc tests

# Load and examine the data
datum <- read.csv("lecture_9_seasons.csv")
head(datum)

# Plot the data
datum$Season <- factor(datum$Season)
plot(Mass ~ Season, data = datum)

# Fit a linear model
results <- lm(Mass ~ Season, data = datum)
summary(results)

# Fit a linear model
results2 <- lm(Mass ~ relevel(Season, ref="Summer"), data = datum)
summary(results2)

# Help file
help(TukeyHSD)

# Tukey requires an ANOVA output
results3 <- aov(Mass ~ Season, data = datum) # or
results3 <- aov(results)
summary(results3)

# This analysis is so simple that it doesn't even have a summary file. Run it directly.
TukeyHSD(results3)

# Compare original lm() to TukeyHSD() results
summary(results)

TukeyHSD(results3)
confint(results)

# Spring to Fall
(-0.63274618 - -0.5149456)/2 # Linear model
(-0.65152888 - -0.49616295)/2 #Tukey HSD

# Look at the second to last column
head(datum)

# Run a 'lm()' with that
results4 <- lm(Mass ~ SeasonN, data = datum)
summary(results4)

# Scatterplot
plot(Mass ~ SeasonN, data = datum)

# Scatterplot
#TukeyHSD(aov(results4)) # doesn't work!
# Error in `TukeyHSD.aov()`:
# ! no factors in the fitted model



####################### 'Truth' ######################## 
### Lecture 9: code to simulate data for post-hoc tests 

# Set the seed for reproducibility
set.seed(123)

# Simulate X-variable
n <- 20
Season <- factor(gl(4, n, labels = c("Fall", "Spring", "Summer", "Winter")))

# Season as a numeric
SeasonN <- as.numeric(factor(Season, levels = c("Fall", "Winter", "Spring", "Summer")))

# Simulate error
Error <- rnorm(length(Season), mean = 0, sd = 0.1)

# Create dummy-coded variables
Dummy <- model.matrix(~ Season - 1)
colnames(Dummy) <- c("Fall", "Spring", "Summer", "Winter")

# Create the dataframe
datum <- data.frame(Season = Season, Error = Error, Dummy, SeasonN)

# Calculate Y-variable
y <- 4.6 - (0.6 * datum$Spring) - (0.6 * datum$Summer) - (0.05 * datum$Winter) + Error

# Create dataframe
datum <- cbind(datum, Mass = y)

# Save the CSV file
write.csv(datum, "lecture_9_seasons.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_9_seasons.csv")

