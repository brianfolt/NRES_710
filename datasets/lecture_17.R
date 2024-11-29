
#  NRES 710, Mixed Effect Models
#     University of Nevada, Reno
#     Models with both fixed and random effects

### Repeated measures model analysis in R

# Read the data
datum <- read.csv("lecture_17_dataset1.csv")

# Examine it
head(datum)

# Plot it
plot(Pollution ~ Distance, data = datum, pch = as.numeric(datum$River))

# Linear model
library(nlme)
results <- lme(Pollution ~ Distance, data = datum, random = ~1|River)
summary(results)

# Linear model
help(lme)

# Linear mixed-effects model that accounts for autocorrelation
results2 <- lme(Pollution ~ Distance, data = datum, random = ~1|River, correlation = corAR1())
summary(results2)

# Partial likelihood test
anova(results2, results)

# Linear mixed-effects model that accounts for autocorrelation with a moving-average autocorrelation model
results3 <- lme(Pollution ~ Distance, data = datum, random = ~1|River, correlation = corARMA(p = 0, q = 1))
summary(results3)

# Partial likelihood test comparing model with moving-average autocorrelation and no autocorrelation
anova(results3, results)

# Compare the two models with autocorrelation
anova(results3, results2)

################### 'Truth' #################### 
### Lecture 17: code to simulate data for class

### Dataset 1
# Pollution as a function of distance in three rivers
# Set the seed for reproducibility
set.seed(123)

# Rivers
n_rivers <- 10
n_samples <- 10
River <- sort(rep(1:n_rivers, n_samples))

# Distance from pollution source
Distance <- rep(seq(1, n_samples, 1), n_rivers)

# Error due to river
RiverError <- rep(rnorm(n_rivers, 0, 5), each = n_samples)

# Error within river (residual error)
# Simulate autocorrelated error - each value using the mean of the previous value
Error <- matrix(NA, length(Distance), 1)
Error[1,1] <- rnorm(1, mean = 0, sd = 1)
for (i in 2:length(Distance)){
  Error[i,1] <- rnorm(1, mean = Error[i-1, 1], sd = 1)
}

# Response variable - Pollution
Pollution <- 150 - 2*Distance + RiverError + Error

# Save the data
datum <- data.frame(River = River, Distance = Distance, RiverError = RiverError, Error = Error, Pollution = Pollution)

# Save the data
write.csv(datum, "lecture_17_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_17_dataset1.csv", row.names = FALSE)

