### Cormack-Jolly-Seber models
### Estimate apparent surival probability from mark-recapture data

# Install and load two useful packages & load them
install.packages("AHMbook", "wiqid") # install - only have to run once
library(AHMbook) # load - run at start of exercise
library(wiqid) # load - run at start of exercise

## Simulated data

# CJS data simulation function from package 'AHMbook'
?simCJS() # Query the help file
# Simulate data with phi = 0.65, p = 0.4; neither parameter varies by time
set.seed(1) # Set random number seed to ensure consistency among simulations
datum <- simCJS(n.occ = 6, n.marked = 20, phi = 0.8, p = 0.5, show.plot = FALSE)
str(datum) # Examine the data
head(datum$ch, 20) # Examine the capture histories
set.seed(1) # Set random number seed to ensure consistency among simulations
datum <- simCJS(n.occ = 6, n.marked = 20, phi = 0.8, p = 0.5, show.plot = TRUE) # Examine the data with plots

# Approaches to fit CJS models from the package 'wiqid'
help(survCJS)

# Frequentist CJS models
results <- survCJS(datum$ch)
results

results2 <- survCJS(datum$ch,phi ~ .time)
results2
# Note: we can compare the fit of these two models to our data with AIC!

# Bayesian CJS approaches
results3 <- BsurvCJS(datum$ch)
summary(results3)
#Rhat indicates convergence

results4 <- BsurvCJS(datum$ch, phi ~ .time)
summary(results4)
# No AIC for us with these models!
# Model fit harder to evaluate with Bayesian hierarchical models.
# We will learn how to use posterior-predictive checks for Goodness of Fit
# testing in lab.