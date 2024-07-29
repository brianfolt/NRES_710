
#  NRES 710, P-values and an Intro to R  
#     University of Nevada, Reno
#     P-values, a discussion of readings, and an intro to R   


# Getting started with R ----------------- 

# Use R as a calculator. Run the following code from your script (top-left panel in RStudio)
# through your Console (bottom-left panel in RStudio)

2 + 2                 # use R as a calculator
four <- 2 + 2         # define your first variable! This creates an 'object' called 'four'
four
five <- 2 + 2         # BEWARE: you can make mistakes and define misleading labels - so be careful!
three <- four + five
three


# R has many built in datasets 

# data()    # 'uncomment' this command and run it to explore built-in datasets
#           # code can be uncommented with CTRL SHIFT C (PC) or COMMAND SHIFT C (Mac)


#iris                 # this is a data frame -- the basic data storage type in R
head(iris)            # [add your own comment here!]
# tail(iris)

#  ?iris              # uncomment this to learn more about the iris dataset
# str(iris)


len <- iris$Petal.Length
hist(len)             # what does this do? How could you learn more about this 'hist' function?

# Q: what kind of data are petal lengths?


#install.packages("titanic")    # uncomment this command to install the package. you only need to install once! so comment the code after installing.
 
library(titanic)              # this 'loads' the package and needs to be done every time you run this script

data("titanic_train")
head(titanic_train)
# ?titanic_train              # uncomment and run to learn more about the data

# Q: What kind of data are those in the "Embarked" column? How might you learn more 
# Q: What kind of data are those in "Pclass?"
# Q: How might you learn more about data types for all variables?


# Make our own data -------------------

# lets pull 15 numbers from the standard normal distribution

a <- rnorm(15)
a <- rnorm(15, mean = 2, sd = 0.5)

# let's pull 15 numbers from the binomial distribution
b <- rbinom(15, size = 1, prob = 0.2) # we could "weight the coin"


# we can create categories: 
unit <- rep(c("Control", "+N", "+P", "+NP"), each = 20)


# we can even create a whole dataframe
plant.data <- data.frame(
  Obs.Id = 1:100, # 100 plants
  Treatment = rep(c("A", "B", "C", "D", "E"), each = 20), # 5 treatments
  Block = rep(1:20, times = 5), # 5 randomized blocks
  Germination = rpois(100, lambda = rep(c(1, 5, 4, 7, 1), each = 20)), # Germination status: 1 (yes), 0 (no)
  AvgHeight = rnorm(100, mean = rep(c(10, 30, 31, 25, 35, 7), each = 20)) # Final plant height
)
head(plant.data)


# import data from file ----------------------

# Don't forget to set your working directory (or just make sure you're using an Rstudio Project). 

# setwd("~/Desktop")       # uncomment and run if you want to set the desktop as your working directory.

#Read in the data. Note that the file needs to be in CSV format, the name must be in quotes, and the name must include the csv extension.

# Pleach <- read.csv("PbyTime_Bio.csv", header=T) # this won't work for you because you don't have this file. 

# Use your own file to try it out. Or, download a file by entering this link into your web browser: https://brianfolt.github.io/NRES_710/CourseSchedule.csv


