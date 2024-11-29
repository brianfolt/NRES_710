
#  NRES 710, Nested Designs
#     University of Nevada, Reno
#     Different applications of nested designs

### Nested design analysis in R

# Read the data
datum <- read.csv("lecture_18_dataset1.csv")

# Examine it
head(datum, 10)

# Make 'Treatment' a factor
datum$Treatment <- as.factor(datum$Treatment)

# Plot it
plot(Biomass ~ Treatment, data = datum)

# Simple linear model
results <- lm(Biomass ~ Treatment, data = datum)
summary(results)

# Linear mixed-effects model
library(nlme)
results2 <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field)
summary(results2)

### Another example analysis in R
# Read the data
datum <- read.csv("lecture_18_dataset2.csv")

# Examine it
head(datum, 15)

# Structure
str(datum)

# Plot it
plot(Biomass ~ Treatment, data = datum)

# Linear mixed-effects model: incorrect random-effect structure for this design
results <- lme(Biomass ~ Treatment, data = datum, random = ~1|Plot)
summary(results)
# This says: only 3 groups (plots). But we had more! This is not specified correctly; this is assuming a different structure than we really have.

# Linear mixed-effects model: okay, but non-ideal random-effect structure for this design
results2 <- lme(Biomass ~ Treatment, data = datum, random = ~1|PlotOrder)
summary(results2)
# This says: only 72 groups (1 for each plot). This is correct! But it fails to measure partition random error due to field. 

# Linear mixed-effects model: most correct random-effect structure with plots nested within fields
results3 <- lme(Biomass ~ Treatment, data = datum, random = ~1|Field/Plot)
summary(results3)

################### 'Truth' #################### 
### Lecture 18: code to simulate data for class

### Dataset 1
# Simulate nested-design data
set.seed(123)

# Fields
n_fields <- 6
n_samples <- 3
Field <- sort(rep(1:n_fields, n_samples))

# Treatment
Treatment <- c(rep("Control", n_samples*2), rep("T1", n_samples*2), rep("T2", n_samples*2))

# Dummy-code treatment
dummy <- data.frame(model.matrix(~ Treatment - 1))
colnames(dummy) <- c("Control", "T1", "T2")

# Field error
FieldError <- rep(rnorm(n_fields, 0, 0.5), each = n_samples)

# Error within fields (residual error)
Error <- rnorm(n_samples*n_fields, 0, 1)

# Response variable -- biomass
Biomass <- 20 + 10*dummy$T1 + 20*dummy$T2 + FieldError + Error

# Save the data
datum <- data.frame(Field = Field, Treatment = Treatment, Control = dummy$Control, T1 = dummy$T1, T2 = dummy$T2, FieldError = FieldError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "lecture_18_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_18_dataset1.csv", row.names = FALSE)


### Dataset 2
# Simulate nested-design data
set.seed(123)

# Fields
n_fields <- 24
n_plots <- 3
n_treatments <- 2
Field <- sort(rep(rep(1:n_fields, n_plots), n_treatments))

# Plots
Plot <- rep(c(rep(1, n_treatments), rep(2, n_treatments), rep(3, n_treatments)), n_fields)

# Treatments
Treatment <- rep(rep(c(0, 1), n_plots), n_fields)

# Field error
FieldError <- rep(rnorm(n_fields, 0, 1), each = n_plots * n_treatments)

# Plot error
PlotError <- rep(rnorm(n_plots * n_fields, 0, 2), each = n_treatments)

# Residual error
Error <- rnorm(length(Field), 0, 1)

# Response variable -- biomass
Biomass <- 20 + 10*Treatment + FieldError + PlotError + Error

# Plot order: plots numbered from 1 to 72
PlotOrder <- rep(1:(n_fields*n_plots), each = n_treatments)

# Save the data
datum <- data.frame(Field = Field, Plot = Plot, PlotOrder = PlotOrder, Treatment = Treatment, FieldError = FieldError, Error = Error, Biomass = Biomass)

# Save the data
write.csv(datum, "lecture_18_dataset2.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_18_dataset2.csv", row.names = FALSE)


