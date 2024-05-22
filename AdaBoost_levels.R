# Load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(adabag)


# Load dataset
data <- read.csv("cleaned_tmdb_data_final.csv", stringsAsFactors = FALSE)

# Classify popularity into High, Medium, and Low
data$popularity_level <- cut(data$popularity, breaks = c(-Inf, quantile(data$popularity, c(1/3, 2/3)), Inf), labels = c("Low", "Medium", "High"))

# Remove popularity column
data <- data[, !(names(data) %in% c("popularity"))]

# Split data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$popularity_level, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train AdaBoost classifier
boost_model <- boosting(popularity_level ~ ., 
                        data = train_data, 
                        mfinal = 200,
                        boos = TRUE)

# Predict on test data
predictions <- predict(boost_model, newdata = test_data)

# Evaluate the model
confusionMatrix(factor(predictions$class), test_data$popularity_level)


precision_low <-0.8742     
recall_low <-   	0.9010

precision_medium <-0.8107   
recall_medium <-   	0.7747

precision_high <-0.8956	
recall_high <-0.9078

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)