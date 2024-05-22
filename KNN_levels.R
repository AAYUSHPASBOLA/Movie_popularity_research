# Load libraries
library(tidyverse)
library(caret)

# Load dataset
data <- read.csv("cleaned_tmdb_data_final.csv", stringsAsFactors = FALSE)

# Classify popularity into High, Medium, and Low on basis of quantile
data$popularity_level <- cut(data$popularity, breaks = c(-Inf, quantile(data$popularity, c(1/3, 2/3)), Inf), labels = c("Low", "Medium", "High"))

# Remove popularity column
data <- data[, !(names(data) %in% c("popularity"))]

# Split data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$popularity_level, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train model
model <- train(popularity_level ~ .,
               data = train_data,
               method = "knn")

# Make predictions on test set
predictions <- predict(model, test_data)

# Confusion matrix
confusionMatrix(predictions, test_data$popularity_level)


precision_low <- 0.6828    
recall_low <- 	0.7201   

precision_medium <-0.4870    
recall_medium <-  	0.4471  

precision_high <-0.6877
recall_high <-		0.7065

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)