# Load libraries
library(tidyverse)
library(caret)
library(randomForest)

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

# Train model

rf_model <- randomForest(popularity_level ~ ., 
                         data = train_data, 
                         ntree = 600, 
                         mtry = 1, 
                         nodesize = 12, 
                         maxnodes = 20, 
                         maxdepth = 25)

#   - ntree: Number of trees in the forest
#   - mtry: Number of variables randomly sampled as candidates at each split
#   - nodesize: Minimum size of terminal nodes
#   - maxnodes: Maximum number of terminal nodes
#   - maxdepth: Maximum depth of the tree

# Make predictions
rf_pred <- predict(rf_model, test_data)

# Confusion matrix
confusionMatrix(rf_pred, test_data$popularity_level)

precision_low <-0.8377              
recall_low <-	0.8635  

precision_medium <-0.7361      
recall_medium <-	0.7235  

precision_high <-0.8754
recall_high <-	0.8635

f1_score_low <- 2 * (precision_low * recall_low) / (precision_low + recall_low)
f1_score_medium <- 2 * (precision_medium * recall_medium) / (precision_medium + recall_medium)
f1_score_high <- 2 * (precision_medium * precision_high) / (precision_medium + precision_high)

cat("f1_score_low : ",f1_score_low)
cat("f1_score_medium : ",f1_score_medium)
cat("f1_score_high : ",f1_score_high)