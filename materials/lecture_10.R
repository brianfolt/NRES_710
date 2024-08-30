
#  NRES 710, Analysis of Categorical Data (cont.)
#     University of Nevada, Reno
#     Post-hoc tests

# Load and examine the data
datum <- read.csv("lecture_10_seasons.csv")
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

summary(results)

TukeyHSD(results3)
confint(results)

# Spring to Fall
(-0.63274618 - -0.5149456)/2 # LM
(-0.65152888 - -0.49616295)/2 #Tukey

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
### Lecture 10: code to simulate data for post-hoc tests 

# Set the seed for reproducibility
set.seed(123)

# Simulate X-variable
n <- 80
x <- factor(c(rep("Spring", n/4), rep("Summer", n/4), rep("Winter", n/4), rep("Fall", n/4)))

# Season as a numeric
SeasonN <- as.numeric(factor(x, levels = c("Fall", "Winter", "Spring", "Summer")))

# Simulate error
error <- rnorm(n, mean = 0, sd = 0.1)

# Create dummy-coded variables
dummy <- model.matrix(~ x - 1)
colnames(dummy) <- c("Fall", "Spring", "Summer", "Winter")

# Create the dataframe
datum <- data.frame(Season = x, error = error, dummy, SeasonN)

# Calculate Y-variable
y <- 4.6 - (0.6 * datum$Spring) - (0.6 * datum$Summer) - (0.05 * datum$Winter) + error

# Create dataframe
datum <- cbind(datum, Mass = y)

# Save the CSV file
write.csv(datum, "lecture_10_seasons.csv", row.names = FALSE)
