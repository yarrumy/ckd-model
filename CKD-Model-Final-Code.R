---
title:"ISM 645 Group 4 Final Project"
author(s):"Nylah Murray and Zohra Tayebali"
output:html_document
---
  
#Import Libraries
  library(tidyverse)
  library(rpart)
  library(rpart.plot)
  library(dplyr)
  library(rsample)
  library(rpart)
  library(rpart.plot)
  library(caret)
  library(yardstick)
  library(randomForest)

#Import Data Set
  KD_in_India <- read.csv("KD in India.csv")
  # Convert values into binary
  KD_in_India$classification <- ifelse(KD_in_India$classification != "ckd", 1, 0)
  #Albumin Creatinine Ratio
  KD_in_India$al_sc_ratio <- (KD_in_India$al/KD_in_India$sc)
  summary(KD_in_India)
  
  selected_vars <- KD_in_India[, c("bu","age","bp","sod","bgr","hemo","sg","al_sc_ratio")]
  cor(selected_vars, use="complete.obs")
  

#Create training and test data set
  # Split the data 70/30
  set.seed(10)
  KD_split <- initial_split(KD_in_India, prop = 0.7, strata = "classification")
  
  KD_train <- training(KD_split)
  KD_test <- testing(KD_split)
  
#Logistic Regression modeling
  # Perform logistic regression
  #Original model
  cor(KD_in_India$sc, KD_in_India$classification, use = "complete.obs")
  cor(KD_in_India$al, KD_in_India$classification, use = "complete.obs")
  cor(KD_in_India$al_sc_ratio, KD_in_India$classification, use = "complete.obs")
  
  #research Question 1
  logistic_model_1 <- glm(classification ~  al_sc_ratio, data = KD_train, family = binomial)
  summary(logistic_model_1)
  
  predictions <- predict(logistic_model_1, newdata = KD_test, type = "response")
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- confusionMatrix(factor(predicted_class), factor(KD_test$classification))
  conf_matrix
  # Result 
  accuracy <- conf_matrix$overall['Accuracy']
  print(paste("Accuracy:", accuracy))

  #Research Question 2
  cor(KD_in_India$age, KD_in_India$classification, use = "complete.obs")
  logistic_model_2 <- glm(classification ~  age, data = KD_train, family = binomial)
  summary(logistic_model_2)
  predictions <- predict(logistic_model_2, newdata = KD_test, type = "response")
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- confusionMatrix(factor(predicted_class), factor(KD_test$classification))
  conf_matrix
  # Result 
  accuracy <- conf_matrix$overall['Accuracy']
  print(paste("Accuracy:", accuracy))

  #Research Question 3
  logistic_model_3<-glm(classification ~ bu+age+bp+sod+bgr, data = KD_train, family = binomial)
  summary(logistic_model_3)

  # Find predictions on the train set
  KD_train <- KD_train %>% 
    mutate(predictions = predict(logistic_model_3, newdata = KD_train, type = "response")) %>% 
    mutate(binary_predictions = if_else(predictions>0.5, 1, 0))
  rmse(KD_train, classification, binary_predictions)
  
  table(KD_train$binary_predictions, KD_train$classification)
  
  #Find predictions on the test data set
  KD_test <- KD_test %>% 
    mutate(predictions = predict(logistic_model_3, newdata = KD_test, type = "response")) %>% 
    mutate(binary_predictions = if_else(predictions>0.5, 1, 0))
  rmse(KD_test, classification, binary_predictions)
  
  table(KD_test$binary_predictions, KD_test$classification)
  
  predictions <- predict(logistic_model_3, newdata = KD_test, type = "response")
  predicted_class <- ifelse(predictions > 0.5, 1, 0)
  conf_matrix <- confusionMatrix(factor(predicted_class), factor(KD_test$classification))
  conf_matrix
  # Result 
  accuracy <- conf_matrix$overall['Accuracy']
  print(paste("Accuracy:", accuracy))

# Test the model against a random patient 
  random_patient <- data.frame(
    age = 64,
    bp = 130,
    bgr = 150,
    sod = 150
  )
  
  # Predict the probability for this patient 
  random_patient_prediction <- predict(logistic_model_3, newdata = random_patient, type = "response")
  
  # View the prediction 
  random_patient_prediction
  
#Decision tree modeling
  
  #Create decision tree with specific gravity and hemoglobin
  KD_dtree <- rpart(classification ~ bu+age+bp+sod+bgr, data = KD_train, method = "class")
  rpart.plot(KD_dtree, cex=0.7)
  
  # Test predictions on decision tree
  predictions_dtree <- predict(KD_dtree, newdata = KD_test, type = "class", na.action = na.pass)
  
  # Seeing which patients have CKD or NotCKD
  predict_class <- predict(KD_dtree, KD_test, type = "class")
  
  # Create a table to see how many are classified as having CKD and how many don't 
  table_mat <- table(KD_test$classification, predict_class, useNA = "no")
  table_mat

  # Create confusion matrix
  conf_matrix <- table(KD_test$classification, predict_class)
  print(conf_matrix)
  
  # Test confusion matrix to measure performance
  accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
  
  # Print confusion matrix
  print(paste('Accuracy of Confusion Matrix', accuracy_test))