
#  NRES 710, Mixed Effect Models
#     University of Nevada, Reno
#     Models with both fixed and random effects

# Read in the treatment field data from last class
datum <- read.csv("lecture_15_dataset2.csv")

# Examine the data
head(datum, 10)

# Fit a t-test
results <- lm(Biomass ~ Treatment, data = datum)
summary(results)

# Load the package
library(nlme)

# Fit the mixed-effects model
resultsMix <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field)
summary(resultsMix)

# Partial likelihood ratio test
anova(resultsMix, results)

# Re-examine the mixed-effects model
summary(resultsMix)

# Reduced model (basically, a random-effects model)
resultsRed <- lme(Biomass ~ 1, data = datum, random = ~1|Field)
summary(resultsRed)

# Now let's test whether the complex model is significantly better than the reduced model
anova(resultsMix, resultsRed)

# Notice at the bottom! This is a warning we should not ignore.

# Mixed model
resultsMix <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field, method = "ML")
summary(resultsMix)
# Didn't really change much. Effects still the same. 'Linear mixed-effects model fit by maximum likelihood'.

# Reduced model
resultsRed <- lme(Biomass ~ 1, data = datum, random = ~1|Field, method = "ML")
summary(resultsRed)
# One change here is that the standard deviation due to Field reduces to ~zero. This happens because Treatment explains a lot of variation in Biomass (P = 0), but we haven't accounted for that effect. This variance gets absorbed into the Residual Error, and then the random Field intercept variance becomes almost zero because the model fails to capture the between-field variation effectively.

# Now let's test whether the complex model is significantly better than the reduced model
anova(resultsMix, resultsRed)
# No longer a WARNING at the bottom.

# Mixed model
resultsMix <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field)
summary(resultsMix)

# Error for field: 3.81
################### 'Truth' #################### 
### Lecture 16: code to simulate data for class

### Dataset 1
# Set the seed for reproducibility
set.seed(123)

# Simulate data
n_fields <- 15
Field <- rep(1:n_fields, each = 4)
Treatment <- rep(c(0,0,1,1), n_fields)

# Field error
FieldError <- rep(rnorm(n_fields, 0, 5), each = 4)

# FieldXTreatment error
FieldXTreatError <- rep(rnorm(n_fields, 0, 3), each = 4)

# Within-field error
Error <- rnorm(length(Field), 0, 1)

# Response variable
Biomass <- 25 + 10*Treatment + Treatment*FieldXTreatError + FieldError + Error

# Save the data
datum <- data.frame(Field = Field, Treatment = Treatment, FieldXTreatError = FieldXTreatError, FieldError = FieldError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "../datasets/lecture_16_dataset1.csv", row.names = FALSE)


### Dataset 2
# Everything is the same as above in terms of truth, but now the sampling design
# is unbalanced. Field 1 has a lot of samples and a large effect of treatment.
# Set the seed for reproducibility
set.seed(123)

# Simulate data
n_fields <- 15
Field <- sort(c(rep(1, each = 50), rep(2:n_fields, 5)))

# Continuous X-variable
Sunlight <- runif(length(Field), 0, 1)

# Field error (matching the number of samples for each field)
FieldError <- c(rep(rnorm(1, 0, 5), 50), rep(rnorm(n_fields - 1, 0, 5), each = 5))

# Field X Sunlight interaction error (matching the number of samples for each field)
FieldXSunError <- c(rep(10, 50), rep(rnorm(n_fields - 1, 0, 3), each = 5))

# Within-field error
Error <- rnorm(length(Field), 0, 1)

# Response variable
Biomass <- 25 + 10*Sunlight + Sunlight*FieldXSunError + FieldError + Error

# Save the data
datum <- data.frame(Field = Field, Sunlight = Sunlight, FieldXSunError = FieldXSunError, FieldError = FieldError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "../datasets/lecture_16_dataset2.csv", row.names = FALSE)

