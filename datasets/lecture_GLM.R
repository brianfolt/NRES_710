
#  NRES 710, Generalized Linear Models
#     University of Nevada, Reno
#     Revising the LM to handle non-normal data

# Load the data
datum <- read.csv("lecture_GLM_dataset1.csv")
head(datum, 20)

# Plot it
plot(ClutchSize ~ Age, data = datum, pch = as.numeric(datum$Wild))

# Examine the 'glm()' function
help(glm)

# Run the GLM
results <- glm(ClutchSize ~ Age + Wild, data = datum)
summary(results)
# Did I do something wrong??

# Yes, I forgot to specify the 'family' statement to use a Poisson distribution! 
# Re-run it correctly while specifying the 'family' statement
results2 <- glm(ClutchSize ~ Age + Wild, data = datum, family = "poisson")
summary(results2)
# Examine the confidence limits
confint(results2)

################### 'Truth' #################### 
### Lecture ##: code to simulate data for class

### Dataset 1
# Simulate split-plot design data w/ three fixed effects: bison, fertilizer, water split across 6 fields
set.seed(123)

# Sample size
n_snakes <- 45 # Sample size

# X1 data - age
age <- round(runif(n_snakes, 3, 10), 0) # Age - uniformly distributed between 3 and 10 years old

# X2 data - clutch laid in wild or captivity
wild <- rbinom(size = 1, n = n_snakes, prob = 0.5) # 1 = laid in wild; 0 = laid in captivity

# Response variable
mean_clutch <- exp(0 + 0.1*age + 1.5*wild) # y_hat; the mean clutch size for each snake, given effects
clutch_size <- rep(NA, n_snakes)
# Response variable
for (i in 1:n_snakes){
  clutch_size[i] <- rpois(1, mean_clutch[i])
  # y_i - simulated each observation of y_i using a random process but given the y_hat mean for each individual snake
}

# Save the data
datum <- data.frame(Age = age, Wild = wild, MeanClutchSize = mean_clutch, ClutchSize = clutch_size)

# Save the data
write.csv(datum, "lecture_GLM_dataset1.csv", row.names = FALSE)
write.csv(datum, "../datasets/lecture_GLM_dataset1.csv", row.names = FALSE)

