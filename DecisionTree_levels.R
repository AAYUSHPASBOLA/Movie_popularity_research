# Load required libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# Load dataset
data <- read.csv("cleaned_tmdb_data_final.csv", stringsAsFactors = FALSE)

# Classify popularity into High, Medium, and Low
data$popularity_level <- cut(data$popularity, breaks = c(-Inf, quantile(data$popularity, c(1/3, 2/3)), Inf), labels = c("Low", "Medium", "High"))

#quantiles <- quantile(data$popularity, c(1/3, 2/3))

# Remove popularity column
data <- data[, !(names(data) %in% c("popularity"))]

# Split data into training and testing sets
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$popularity_level, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Decision tree model
model <- rpart(popularity_level ~ .,
               data = train_data,
               method = "class",
               cp = 0.001,
               minsplit = 25,
               minbucket = 5,
               maxdepth = 10,
               maxcompete = 4,
               xval = 8)

# Plot the decision tree
#rpart.plot(model, main = "Decision Tree")
#prp(model)

#plotcp(model)

# Predictions on test data
predictions <- predict(model, test_data, type = "class")

# Confusion matrix
confusionMatrix(predictions, test_data$popularity_level)


precision_low <- 0.8585        
recall_low <- 0.9113        

precision_medium <- 0.8238      
recall_medium <- 0.7338      

precision_high <- 0.8795
recall_high <- 0.9215

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)
