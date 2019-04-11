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
disease_data <- data_full %>% filter_all(all_vars(.!="?"))

# Create a new column "diseaseBin" with values of 0 and 1 indicating Heart Disease. The existing column
# "disease" contains numeric values of 0-4, where 0 means no heart disease and 4, severe heart disease.
# In this project, the aim is to predict instances of disease regardless of severity. Thus, values 1-4
# are converted to 1 in "disease_bin". Remove old disease column.
disease_data <- disease_data %>% mutate("diseaseBin" = ifelse(disease==0, 0, 1))
disease_data$disease <- NULL

# Convert categorical variables from numeric to factor.
cols <- c("sex", "chestPain", "fastBloodSugar", "ECGrest", "exerciseAngina", "STslope", "defect","diseaseBin")
disease_data[cols] <- lapply(disease_data[cols], factor)
# The nFluoVessels was loaded as factors due to the presence of "?" values initially.
# We convert it back to numeric.
disease_data$nFluoVessels <- as.numeric(disease_data$nFluoVessels)


#############################################################
# Data Exploration and Selection of Meaningful Parameters
#############################################################

library(ggplot2)

# Vizualize the density distributions for Heart Disease conditions in function of
# the available continuous parameters

HeartDisease <- disease_data$diseaseBin

density_plot <- function(column, param_name){
  ggplot(disease_data, aes(x=column, fill=HeartDisease, color=HeartDisease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name=param_name) +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes"))
}

plotAge <- density_plot(disease_data$age, "Age")
plotAge

plotBPrest <- density_plot(disease_data$BPrest, "Blood Pressure at Rest (mmHg)")
plotBPrest

plotchol <- density_plot(disease_data$chol, "Cholesterol (mg/dl)")
plotchol

plotmaxHR <- density_plot(disease_data$maxHR, "Maximum Heart Rate during Exercise (BPM)")
plotmaxHR

plotSTpeak <- density_plot(disease_data$SToldPeak, "ECG Wave-ST Depression Induced by Exercise Relative to Rest")
plotSTpeak

plotFluo <- density_plot(disease_data$nFluoVessels, "Number of Major Vessels Colored by Fluoroscopy")
plotFluo

# Visualize the categorical parameters with histograms

format_barplot <- function(gc, columngroup, param_name, labelling){
  ggplot(gc, aes(x=columngroup, y=n, fill=diseaseBin))+ 
    geom_bar( stat="identity") +
    scale_x_discrete(name=param_name, labels=labelling) +
    scale_fill_discrete(name='Heart Disease', labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease', labels=c("No", "Yes")) +
    theme(legend.position="bottom")
}

groupby_sex <- disease_data %>% group_by(diseaseBin) %>% count(sex) %>% as.data.frame()
plotSex <- format_barplot(groupby_sex, groupby_sex$sex, "Sex", c("Female", "Male"))
plotSex

groupby_cp <- disease_data %>% group_by(diseaseBin) %>% count(chestPain) %>% as.data.frame()
plotCP <- format_barplot(groupby_cp, groupby_cp$chestPain, "Chest Pain Type",
                         c("Typical Angina", "Atypical Angina", "Non-Anginal Pain", "Asymptomatic")) 
plotCP

groupby_fbs <- disease_data %>% group_by(diseaseBin) %>% count(fastBloodSugar) %>% as.data.frame()
plotFBS <- format_barplot(groupby_fbs, groupby_fbs$fastBloodSugar, "Fasting Blood Sugar",
                          c("< 120 mg/dl", ">= 120 mg/dl"))
plotFBS
  
groupby_ECGrest <- disease_data %>% group_by(diseaseBin) %>% count(ECGrest) %>% as.data.frame()
plotECGrest <- format_barplot(groupby_ECGrest, groupby_ECGrest$ECGrest, "ECG at Rest Status",
                          c("Normal", "ST-T wave abnormality", "left ventricular hypertrophy"))
plotECGrest

groupby_exAng <- disease_data %>% group_by(diseaseBin) %>% count(exerciseAngina) %>% as.data.frame()
plotexAng <- format_barplot(groupby_exAng, groupby_exAng$exerciseAngina, "Exercise-Induced Angina",
                            c("No Angina", "Angina"))
plotexAng

groupby_STslope <- disease_data %>% group_by(diseaseBin) %>% count(STslope) %>% as.data.frame()
plotSTslope <- format_barplot(groupby_STslope, groupby_STslope$STslope, "Slope of Peak Exercise ECG ST Segment",
                              c("Upsloping", "Flat", "Downsloping"))
plotSTslope

groupby_defect <- disease_data %>% group_by(diseaseBin) %>% count(defect) %>% as.data.frame()
plotdefect <- format_barplot(groupby_defect, groupby_defect$defect, "Thalium Stress Test Result",
                              c("Normal", "Fixed Defect", "Reversable Defect"))
plotdefect

# todo Here eliminate useless columns from disease_data

#############################################################
# Create Training and Testing Sets
#############################################################

dim(disease_data)
head(disease_data)

# todo review method to make multiple training and testing sets...

# The testing set will be 20% of the orignal dataset.
set.seed(1)
index <- createDataPartition(y = disease_data$diseaseBin, times = 1, p = 0.1, list = FALSE)
trainingSet <- data[-index,]
testingSet <- data[index,]

group_count$n




