
#  NRES 710, Random Effect Models
#     University of Nevada, Reno
#     Multi-variable models with random effects

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

