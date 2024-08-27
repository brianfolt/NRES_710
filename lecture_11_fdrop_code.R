### Code for F-Drop tests

# Import data
datum <- read.csv(file.choose())
head(datum)

# Plotting the data
plot(Biomass ~ Bison, data = datum) # In this case, R saws numbers in the 'Bison' column and treated the variable as continuous
plot(Biomass ~ as.factor(Bison), data = datum) # use the as.factor function to get R to treat 'Bison' as a categorical variable

# Run a regression - treat Bison as continuous
results <- lm(Biomass~Bison, data = datum) #Bison is continuous because R saw numbers for that variable
summary(results)
plot(residuals(results) ~ datum$Bison)

# Run an ANOVA - treat Bison as categorical
results2 <- lm(Biomass ~ as.factor(Bison), data = datum) #use the as.factor function to get R to treat 'Bison' as continuous
summary(results)

# Conduct the f-drop test
anova(results, results3) #the two models you want to compare are included as arguments
# Usually, you should list the more complicated model first, but in this case it doesn't matter
# Note that the two models can't have the same number of parameters
# A significant p-value means the more complex model is a significant improvement in fit
# A non-significant p-value means the simpler model is adequate
# Note that RSS (which is the same thing as SSE) is always lower in the more complex model

# BONUS: How to fit a quadratic curve to data when x is continuous
results3 <- lm(Biomass ~ Bison + I(Bison^2), data = datum)
anova(results3, results)