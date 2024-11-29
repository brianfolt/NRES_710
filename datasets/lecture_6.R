
#  NRES 710, Linear Regression (4)
#     University of Nevada, Reno
#     Linear regression - prediction  

# Read in the biomass data again, if necessary
datum <- read.csv("lecture_6_biomass_data.csv")

# Plot
plot(biomass ~ rainfall, data = datum, xlab = "Rainfall (cm)", ylab = "Biomass (kg/ha)")

# Regression
results <- lm(biomass ~ rainfall, data = datum)
abline(results)

# Examine the results 
summary(results)

# Make a prediction for when rainfall = 3 cm
1.69 + 3.34 * 3

# Examine the help file
help(predict.lm)

# Create predict data
datumPredict <- data.frame(rainfall = 3)
datumPredict

# Create a new object called predictions
predictions <- predict(results, datumPredict, interval = "prediction")
predictions

predictions[1,"upr"] - predictions[1,"fit"]

# Let's look at information about our data
summary(datum)
# Minimum rainfall was closer to 0, maximum was closer to 10. 
# Let's make predictions between 0 -- 10 cm of rainfall

# And let's try to make 20 predictions across this interval:
# 10 cm / 20 predictions = a prediction every 0.5 cm
xSequence <- seq(from = 0, to = 10, by = 0.5)
xSequence

# Turn this into a dataframe with a column called 'rainfall'
datumPredict <- data.frame(rainfall = xSequence)
datumPredict

# Make the predictions
predictions <- predict(results, datumPredict, interval = "prediction")

# Examine predictions
head(predictions)

# Make it a dataframe to make it easier to add data to it.
predictions <- as.data.frame(predictions)

# Add a new column called 'rainfall'
predictions$rainfall <- xSequence

# Examine
head(predictions)

# Very simple way to plot the prediction intervals
matplot(predictions$rainfall, predictions[,1:3], type = "l")

# Plot the data, but this time specify what the x- and y-limits are.
# This is important to make sure everything will fit on here.
plot(biomass ~ rainfall, data = datum, xlim = c(0, 10), ylim = c(0, 40), xlab = "Rainfall (cm)", ylab = "Biomass (kg/ha)")

# But now we have to add other data onto here, without erasing the original data.
# Specify the parameter: new = TRUE
par(new = TRUE)
plot(fit ~ rainfall, data = predictions, type = "l", xlim = c(0, 10), ylim = c(0, 40), ylab = "", xlab = "")
par(new = TRUE)
plot(lwr ~ rainfall, data = predictions, type = "l", xlim = c(0, 10), ylim = c(0, 40), ylab = "", xlab = "", lty=2)
par(new = TRUE)
plot(upr ~ rainfall, data = predictions, type = "l", xlim = c(0, 10), ylim = c(0, 40), ylab = "", xlab = "", lty=2)

