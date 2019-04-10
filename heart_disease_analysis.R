# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

#############################################################
# HarvardX: PH125.9 - Data Science: Capstone
#############################################################
#
# The following script uses a subset of the Cleveland Heart 
# Disease dataset and tests different models in order to predict
# instances of heart disease in patients from a number of parameters.
# This code was run on Windows 8 OS with RStudio Version 1.1.447.
#
# Find this project online at: https://github.com/HeloiseA/Capstone_Heart_Disease
# Resource page for the dataset: https://archive.ics.uci.edu/ml/datasets/Heart+Disease
#
#############################################################
# Retrieve and Tidy Dataset
#############################################################

library(dplyr)
library(tidyverse)
library(caret)

# Automatically download data to working directory (NOTE: Comment lines if project was cloned from GitHub)
# IN CASE OF FAILURE FOR THE AUTOMATIC DOWNLOAD:
# 1. Go to: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/
# 2. Download processed.cleveland.data to your working directory
# OR
# 1. Go to: https://github.com/HeloiseA/Capstone_Heart_Disease.
# 2. Click "Clone or Download" button, then select "Download ZIP".
# 3. Unzip the downloaded folder and copy the file "processed.cleveland.data" to your current working directory.
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
              "processed.cleveland.data")

# Load the dataset with named columns
data_columns <- c("age", "sex", "chestPain", "BPrest", "chol", "fastBloodSugar", "ECGrest", "maxHR",
                  "exerciseAngina", "SToldPeak", "STslope", "nFluoVessels", "defect", "disease")
data_full <- read.table("processed.cleveland.data", sep=",", col.names = data_columns)

# Eliminate rows with missing values, indicated by "?" in this dataset
data <- data_full %>% filter_all(all_vars(.!="?"))

# Create a new column "diseaseBin" with values of 0 and 1 indicating Heart Disease. The existing column
# "disease" contains numeric values of 0-4, where 0 means no heart disease and 4, severe heart disease.
# In this project, the aim is to predict instances of disease regardless of severity. Thus, values 1-4
# are converted to 1 in "disease_bin". Remove old disease column.
data <- data %>% mutate("diseaseBin" = ifelse(disease==0, 0, 1))
data$disease <- NULL

# Convert categorical variables from numeric to factor.
cols <- c("sex", "chestPain", "fastBloodSugar", "ECGrest", "exerciseAngina", "STslope", "defect","diseaseBin")
data[cols] <- lapply(data[cols], factor)
# The nFluoVessels was loaded as factors due to the presence of "?" values initially.
# We convert it back to numeric.
data$nFluoVessels <- as.numeric(data$nFluoVessels)


#############################################################
# Create Training and Testing Sets
#############################################################

# The testing set will be 20% of the orignal dataset.
set.seed(1)
index <- createDataPartition(y = data$diseaseBin, times = 1, p = 0.1, list = FALSE)
trainingSet <- data[-index,]
testingSet <- data[index,]


#############################################################
# Data Exploration and Selection of Meaningful Parameters
#############################################################

library(ggplot2)











dim(data)
head(data)












