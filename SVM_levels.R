# Load libraries
library(tidyverse)
library(e1071)
library(caret)

# Load dataset
data <- read.csv("cleaned_tmdb_data_final.csv", stringsAsFactors = FALSE)

# Classify popularity into High, Medium, and Low
data$popularity_level <- cut(data$popularity, breaks = c(-Inf, quantile(data$popularity, c(1/3, 2/3)), Inf), labels = c("Low", "Medium", "High"))

# Remove popularity column
data <- data[, !(names(data) %in% c("popularity"))]

# Split data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$popularity_level, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train models
svm_model <- svm(popularity_level ~ .,
                 data = train_data,
                 kernel = "radial",
                 cost = 2,
                 epsilon = 0.1,
                 gamma = 0.1)

# Make predictions
svm_pred <- predict(svm_model, test_data)

# Confusion matrix
confusionMatrix(svm_pred, test_data$popularity_level)

precision_low <-0.8248     
recall_low <-   	0.9317 

precision_medium <-0.7962    
recall_medium <-  	0.7201 

precision_high <-0.9046	
recall_high <-0.8737

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)