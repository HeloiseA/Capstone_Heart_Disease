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
# Finally: Comment the download.file lines and launch the code again
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
disease_data$nFluoVessels <- as.numeric(disease_data$nFluoVessels) - 2

#############################################################
# Data Exploration and Selection of Meaningful Parameters
#############################################################

library(ggplot2)

# Explore dimensions
dim(disease_data)
head(disease_data)

# Vizualize the density distributions for Heart Disease conditions in function of
# the available continuous features
HeartDisease <- disease_data$diseaseBin

# Create a function for the density plots
density_plot <- function(column, param_name){
  ggplot(disease_data, aes(x=column, fill=HeartDisease, color=HeartDisease)) +
    geom_density(alpha=0.2) +
    theme(legend.position="bottom") +
    scale_x_continuous(name=param_name) +
    scale_fill_discrete(name='Heart Disease',labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease',labels=c("No", "Yes"))
}

# Plot for all continuous features
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

# Visualize the categorical parameters with stacked barplots
# Create a function for the barplots
format_barplot <- function(gc, columngroup, param_name, labelling){
  ggplot(gc, aes(x=columngroup, y=n, fill=diseaseBin))+ 
    geom_bar( stat="identity") +
    scale_x_discrete(name=param_name, labels=labelling) +
    scale_fill_discrete(name='Heart Disease', labels=c("No", "Yes")) +
    scale_color_discrete(name='Heart Disease', labels=c("No", "Yes")) +
    theme(legend.position="bottom")
}

# Plot for all categorical features
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

# Based on observation, keep only features that vary based on disease status
keep_columns <- c(2, 3, 8, 9, 11, 12, 13, 14)
disease_clean <- disease_data[, keep_columns]
head(disease_clean)

#############################################################
# Create Training and Testing Sets
#############################################################

# The testing set will be 20% of the orignal dataset.
set.seed(1)
index <- createDataPartition(y = disease_clean$diseaseBin, times = 1, p = 0.2, list = FALSE)
trainingSet <- disease_clean[-index,]
testingSet <- disease_clean[index,]

#############################################################
# Create a first model with K-Nearest Neighbors
#############################################################

# We train a k-nearest neighbor algorithm with a tunegrid parameter to optimize for k
set.seed(1989)
train_knn <- train(diseaseBin ~ ., method = "knn",
                   data = trainingSet,
                   tuneGrid = data.frame(k = seq(2, 30, 2)))

# Visualize and save the optimal value for k
k_plot <- ggplot(train_knn, highlight = TRUE)
k_plot
optim_k <- train_knn$bestTune[1, 1]

# Train and predict using k-nn with optimized k value
knn_fit <- knn3(diseaseBin ~ ., data = trainingSet, k = optim_k)
y_hat_knn <- predict(knn_fit, testingSet, type = "class")
cm_knn <- confusionMatrix(data = y_hat_knn, reference = testingSet$diseaseBin, positive = "1")

# Return optimized k value, Accuracy, Sensitivity and Specificity
Accuracy_knn <- cm_knn$overall["Accuracy"]
Sensitivity_knn <- cm_knn$byClass["Sensitivity"]
Specificity_knn <- cm_knn$byClass["Specificity"]
print("K-Nearest Neighbors Results")
cat("Optimized k: ", optim_k)
cat("Accuracy: ", Accuracy_knn)
cat("Sensitivity: ", Sensitivity_knn)
cat("Specificity: ", Specificity_knn)

#############################################################
# Create a second model with Adaptive Boosting
#############################################################

# Train and predict outcomes with the adaboost algorithm
# Warning, this may take a few minutes to run
train_ada <- train(diseaseBin ~ ., method = "adaboost", data = trainingSet)
y_hat_ada <- predict(train_ada, testingSet)
cm_ada <- confusionMatrix(data = y_hat_ada, reference = testingSet$diseaseBin, positive = "1")

# Return Accuracy, Sensitivity and Specificity
Accuracy_ada <- cm_ada$overall["Accuracy"]
Sensitivity_ada <- cm_ada$byClass["Sensitivity"]
Specificity_ada <- cm_ada$byClass["Specificity"]
print("Adaptive Boosting Results")
cat("Accuracy: ", Accuracy_ada)
cat("Sensitivity: ", Sensitivity_ada)
cat("Specificity: ", Specificity_ada)

#############################################################
# Create a third model with Naive Bayes
#############################################################

# Look at correlation between features to verify independance
matrix_data <- matrix(as.numeric(unlist(disease_clean)),nrow=nrow(disease_clean))
correlations <- cor(matrix_data)
correlations

# Train and predict using Naive Bayes
train_nb <- train(diseaseBin ~ ., method = "nb", data = trainingSet)
y_hat_nb <- predict(train_nb, testingSet)
cm_nb <- confusionMatrix(data = y_hat_nb, reference = testingSet$diseaseBin, positive = "1")

# Return Accuracy, Sensitivity and Specificity
Accuracy_nb <- cm_nb$overall["Accuracy"]
Sensitivity_nb <- cm_nb$byClass["Sensitivity"]
Specificity_nb <- cm_nb$byClass["Specificity"]
print("Naive Bayes Results")
cat("Accuracy: ", Accuracy_nb)
cat("Sensitivity: ", Sensitivity_nb)
cat("Specificity: ", Specificity_nb)

########################################################################
# Use Weighted Subspace Random Forest (WSRF) and k-fold Cross-Validation
########################################################################

# Define train control for k-fold (10-fold here) cross validation
set.seed(1001)
train_control <- trainControl(method="cv", number=10)

# Train and predict using WSRF
set.seed(1002)
train_wsrf <- train(diseaseBin ~ ., data = trainingSet, 
                 method = "wsrf", 
                 trControl = train_control)
y_hat_wsrf <- predict(train_wsrf, testingSet)
cm_wsrf <- confusionMatrix(data = y_hat_wsrf, reference = testingSet$diseaseBin, positive = "1")

# Return Accuracy, Sensitivity and Specificity
Accuracy_wsrf <- cm_wsrf$overall["Accuracy"]
Sensitivity_wsrf <- cm_wsrf$byClass["Sensitivity"]
Specificity_wsrf <- cm_wsrf$byClass["Specificity"]
print("K-Fold Cross-Validation and WSRF Results")
cat("Accuracy: ", Accuracy_wsrf)
cat("Sensitivity: ", Sensitivity_wsrf)
cat("Specificity: ", Specificity_wsrf)

#############################################################
# Display final results
#############################################################

results <- tribble(
  ~Method, ~Accuracy, ~Sensitivity,  ~Specificity,
  "K-nn", Accuracy_knn,  Sensitivity_knn, Specificity_knn,
  "Adaboost", Accuracy_ada,  Sensitivity_ada, Specificity_ada,
  "Naive Bayes", Accuracy_nb,  Sensitivity_nb, Specificity_nb,
  "WSRF + K-fold c.v.", Accuracy_wsrf,  Sensitivity_wsrf, Specificity_wsrf
)
results
