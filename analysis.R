library(ggplot2)
library(randomForest)
library(caret)

set.seed(100)

setwd("/home/thanh-thao/Projects/Dataiku/")
# Read the datasets
data.learn <- read.csv("census_income_learn.csv", header = FALSE)
data.test <- read.csv("census_income_test.csv", header = FALSE)

# Processing of data
source("data_preprocessing.R")

# Univariate stats and visualization
source("univariate_audit.R")

# Building classification datasets
source("data_improved.R")

# Random forests
# source("random_forest.R")
# Learning error rate : 1.49%
# Test error rate : 4.68%
# After running random forest, I chose to run the other algorithms only on the improved dataset

# Logistic regression
# source("log_regression.R")
# Learning error rate : 5.3%
# Test error rate : 5.28%

# The random forest seem to have better results than the logistic regression based on error rates
# Most important variables seem to be : sex, major indus code, age, capital gains and education
