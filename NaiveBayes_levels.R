# Load required libraries
library(tidyverse)
library(caret)
library(e1071)

# Load dataset
data <- read.csv("cleaned_tmdb_data_final.csv", stringsAsFactors = FALSE)

# Classify popularity into High, Medium, and Low
data$popularity_level <- cut(data$popularity, breaks = c(-Inf, quantile(data$popularity, c(1/3, 2/3)), Inf), labels = c("Low", "Medium", "High"))

# Remove popularity column
data <- data[, !(names(data) %in% c("popularity"))]

# Split data into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$popularity_level, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train Naive Bayes classifier
model <- naiveBayes(popularity_level ~ .,
                    data = trainData,
                    laplace = 0)

# Predict using the trained model
predictions <- predict(model, testData)

# Evaluate the model
confusionMatrix(predictions, testData$popularity_level)

precision_low <-0.6178       
recall_low <- 	0.9215

precision_medium <-0.5000      	
recall_medium <-0.3720

precision_high <-0.8929	
recall_high <-	0.6826

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)