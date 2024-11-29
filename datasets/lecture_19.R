
#  NRES 710, Nested Designs (cont.)
#     University of Nevada, Reno
#     Split-plot designs

# Load the data
datum <- read.csv("lecture_19_dataset1.csv")

# Examine it
head(datum, 10)

# Linear-mixed effects model w/ random effect of field
library(nlme)
results <- lme(Biomass ~ Bison + Fertilizer + Water, data = datum, random = ~1|Field)
summary(results)
# 72 observations with 6 groups (fields)... does not account for pseudoreplication properly, we need better nesting

### Split-plot Design
# Linear-mixed effects model w/ random, nested effects
results2 <- lme(Biomass ~ Bison + Fertilizer + Water, data = datum, random = ~1|Field/Bison/Fertilizer)
summary(results2)
# The random effects are nested with field > bison > fertilizer. Water is not included, because it is our 'ultimate sample' and therefore is the residual error. We only have one sample for each water plot, so we can't disentangle error due to water measurements being different from residual error.

# Linear-mixed effects model w/ random, nested effects
results3 <- lme(Biomass ~ Bison + Fertilizer + Water, data = datum, random = ~1|Field/Bison/Fertilizer/Water)
summary(results3)
# Water is included when it was not needed to be included.

# Help file for lme()
help(lme)
# Notice the optimMethod -- an alternative optimization methods that allows you to change how it recognizes the correct degrees of freedom.

### Nested ANCOVA Design
# Read the data
datum <- read.csv("lecture_19_dataset2.csv")

# Examine
head(datum, 15)

# Plot it
plot(Size ~ Age, data = datum)
# You can almost see that there are ten individuals here....

# Plot with points colored by sex and shaped by individual... a bit messy, but:
plot(Size ~ Age, data = datum, pch = as.numeric(datum$Individual), col = as.numeric(datum$Male) + 1)
legend("topleft", legend = c("Female", "Male"), pch = c(21, 21), col = c(1, 2))

# LME with Age and Sex fixed effects and random effect of individual (repeated measures!)
library(nlme)
results <- lme(Size ~ Age + Male, data = datum, random = ~1|Individual)
summary(results)

# LME with Age, Sex, and Age*Sex fixed effects and random effect of individual (repeated measures!)
library(nlme)
results2 <- lme(Size ~ Age + Male + Age:Male, data = datum, random = ~1|Individual)
summary(results2)

################### 'Truth' #################### 
### Lecture 19: code to simulate data for class

### Dataset 1
# Simulate split-plot design data w/ three fixed effects: bison, fertilizer, water split across 6 fields
set.seed(123)

# Fields
n_fields <- 6
n_bison <- 2
n_fertilizer <- 2
n_water <- 3
Field <- sort(rep(rep(1:n_fields, n_bison*n_fertilizer*n_water)))

# Bison
Bison <- rep(sort(rep(c(0, 1), n_fertilizer*n_water)), n_fields)

# Fertilizer
Fertilizer <- rep(sort(rep(c(0, 1), n_water)), n_fields)

# Water
Water <- rep(c("Low", "Medium", "High"), n_fields * n_bison * n_fertilizer)
Water <- factor(Water, levels = c("Low", "Medium", "High"))
dummy <- data.frame(model.matrix(~ Water - 1))

# Field error
FieldError <- rep(rnorm(n_fields, 0, 1), each = n_bison * n_fertilizer * n_water)

# Bison error
BisonError <- rep(rnorm(n_bison * n_fields, 0, 2), each = n_fertilizer * n_water)

# Fertilizer error
FertilizerError <- rep(rnorm(n_fertilizer * n_bison * n_fields, 0, 2), each = n_water)

# Residual error
Error <- rnorm(length(Field), 0, 1)

# Response variable -- biomass
Biomass <- 20 + 10*Bison + 10*Fertilizer + 10*dummy$WaterMedium + 10*dummy$WaterHigh + FieldError + BisonError + FertilizerError + Error

# Save the data
datum <- data.frame(Field = Field, Bison = Bison, Fertilizer = Fertilizer, Water = Water, WaterMed = dummy$WaterMedium, WaterHigh = dummy$WaterHigh, FieldError = FieldError, BisonError = BisonError, FertError = FertilizerError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "lecture_19_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_19_dataset1.csv", row.names = FALSE)

### Dataset 2
# Body size as a function of age and sex in multiple individuals
# Set the seed for reproducibility
set.seed(123)

# Individuals
n_individuals <- 10
n_samples <- 10
Individual <- sort(rep(1:n_individuals, n_samples))

# Sex
Male <- c(rep(rep(0, n_samples), n_individuals/2), rep(rep(1, n_samples), n_individuals/2))

# Age
Age <- rep(seq(1, n_samples, 1), n_individuals)

# Error due to individual
IndError <- rep(rnorm(n_individuals, 0, 3), each = n_samples)

# Error within individual (residual error)
# Simulate autocorrelated error - each value using the mean of the previous value
Error <- matrix(NA, length(Age), 1)
Error[1,1] <- rnorm(1, mean = 0, sd = 1)
for (i in 2:length(Age)){
  Error[i,1] <- rnorm(1, mean = Error[i-1, 1], sd = 1)
}

# Response variable - Size
Size <- 10 + 1.5*Age + 5*Male + -1*Male*Age + IndError + Error

# Save the data
datum <- data.frame(Individual = Individual, Age = Age, Male = Male, IndError = IndError, Error = Error, Size = Size)

# Save the data
write.csv(datum, "lecture_19_dataset2.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_19_dataset2.csv", row.names = FALSE)


