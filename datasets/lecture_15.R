
#  NRES 710, Random Effect Models
#     University of Nevada, Reno
#     Simple models with random effects

# Read in the data
datum <- read.csv("lecture_15_dataset1.csv")

# Examine it
head(datum, 15)

# Plot it!
plot(Abundance ~ Year, data = datum)

# Year as a categorical, fixed effect
results <- lm(Abundance ~ as.factor(Year), data = datum)
summary(results)

# Year as a categorical, fixed effect
results2 <- aov(results)
TukeyHSD(results2)

# We don't have to install this package - it now comes defaul in R!
library(nlme)

# Examine the help file

# Examine the help file
help(lme)
# Fit the model
results <- lme(Abundance ~ 1, data = datum, random = ~1|Year)
summary(results)
# Load the data
datum <- read.csv("lecture_15_dataset2.csv")

# Examine
head(datum)

# Plot
plot(Biomass ~ Treatment, data = datum)

# T-test
results <- lm(Biomass ~ Treatment, data = datum)
summary(results)

# LME; i.e., paired T-test
results2 <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field)
summary(results2)

################### 'Truth' #################### 
### Lecture 15: code to simulate data for class

# Set the seed for reproducibility
set.seed(123)

# Simulate data
Year <- rep(1:10, each = 10)

# Between-year error
MeanYear <- rep(rnorm(10, 0, 10), each = 10)

# Within-year error
Error <- rnorm(length(Year), 0, 1)

# Response variable
Abundance <- 50 + MeanYear + Error

# Save the data
datum <- data.frame(Year = Year, MeanYear = MeanYear, Error = Error, Abundance = Abundance)

# Save the data
write.csv(datum, "lecture_15_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_15_dataset1.csv", row.names = FALSE)


### Dataset 2
# Set the seed for reproducibility
set.seed(123)

# Simulate data
Field <- rep(1:15, each = 2)
Treatment <- rep(0:1, 15)

# Field error
FieldError <- rep(rnorm(15, 0, 5), each = 2)

# Residual error
Error <- rnorm(length(Field), 0, 1)

# Response variable
Biomass <- 25 + 10*Treatment + FieldError + Error

# Save the data
datum <- data.frame(Field = Field, Treatment = Treatment, FieldError = FieldError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "lecture_15_dataset2.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_15_dataset2.csv", row.names = FALSE)

