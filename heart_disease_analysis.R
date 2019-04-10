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
#
#############################################################
# Retrieve and Tidy Dataset
#############################################################

library(dplyr)
library(tidyverse)
library(caret)

# This project uses a subset of the Cleveland Heart Disease Dataset.
# The original dataset contained 76 parameters; this subset has 14.
# AUTOMATICALLY LOAD DATA TO WORKING DIRECTORY:
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
              "processed.cleveland.data")
# 
data_columns <- c("age", "sex", "chestPain", "BPrest", "chol", "fastBloodSugar", "ECGrest", "maxHR",
                  "exerciseAngina", "SToldPeak", "STslope", "nFluoVessels", "defect", "disease")
data <- read.table("processed.cleveland.data", sep=",", col.names = data_columns)









