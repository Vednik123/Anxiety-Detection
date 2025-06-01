library(smotefamily)
library(caret)
library(randomForest)

#Sampling by SMOTE
data <- read.csv("smote_full.csv")

# #SAMPLING by ROSE
# data <- read.csv("ROSE_full.csv")

#Features selected by LASSO
selected_features_df <- read.csv("selected_features_LASSO.csv")

# #Features selected by RF
# selected_features_df <- read.csv("selected_features_RF4.csv")

all_features <- as.character(selected_features_df$Feature)

top_n_features <- 10

selected_features <- all_features[1:top_n_features]

selected_features <- c(selected_features, "Anxiety")

data <- data[, selected_features]

# Partition into 80% Train - 20% Test
set.seed(123)
trainIndex <- createDataPartition(data$Anxiety, p = 0.75, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]



# Ensure target variable is a factor
trainData$Anxiety <- as.factor(trainData$Anxiety)
testData$Anxiety <- as.factor(testData$Anxiety)

cat("Using", top_n_features, "top features:", paste(selected_features[1:top_n_features], collapse = ", "), "\n")








### RANDOM FOREST ###
# set.seed(123)
# # Train Random Forest Model using only the selected features
# rf_model <- randomForest(Anxiety ~ .,
#                          data = trainData,
#                          importance = TRUE)
# 
# # Predict on test data
# predictions <- predict(rf_model, testData)
# 
# predictions <- factor(predictions, levels = levels(testData$Anxiety))
# 
# # Print confusion matrix
# conf_matrix <- confusionMatrix(predictions, testData$Anxiety)
# print(conf_matrix)
# 
# # Extract values from confusion matrix
# conf_table <- conf_matrix$table
# 
# TP <- conf_table["1", "1"]  # True Positives
# FP <- conf_table["1", "0"]  # False Positives
# 
# # Calculate precision
# precision <- TP / (TP + FP)
# 
# # Display precision
# cat("\nPrecision:", round(precision, 4), "\n")












### XGBOOST ###
library(xgboost)
library(caret)

set.seed(123)

# Prepare data for XGBoost (needs matrix input)
train_matrix <- model.matrix(Anxiety ~ . - 1, data = trainData)
test_matrix <- model.matrix(Anxiety ~ . - 1, data = testData)

# Convert outcome to numeric (starting from 0)
train_labels <- as.numeric(trainData$Anxiety) - 1
test_labels <- as.numeric(testData$Anxiety) - 1

# Set parameters for XGBoost
params <- list(
  objective = "binary:logistic",  # Changed to binary since you have 2 classes
  eval_metric = "logloss",       # Changed to binary metric
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train XGBoost model
xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  params = params,
  nrounds = 100,
  verbose = 0
)

# Predict on test data
xgb_pred_prob <- predict(xgb_model, test_matrix)
xgb_pred_class <- ifelse(xgb_pred_prob > 0.5, 1, 0)

# Convert to factor with same levels as original data
xgb_pred_class <- factor(xgb_pred_class, levels = c(0, 1))
testData$Anxiety <- factor(testData$Anxiety, levels = c(0, 1))

# Confusion matrix
xgb_conf_matrix <- confusionMatrix(xgb_pred_class, testData$Anxiety)
print(xgb_conf_matrix)

# Extract TP and FP from confusion matrix table
conf_table <- xgb_conf_matrix$table
TP <- conf_table["1", "1"]
FP <- conf_table["1", "0"]
precision <- TP / (TP + FP)

# Print Precision
cat("\nPrecision (TP / (TP + FP)):", precision, "\n")
saveRDS(xgb_model,"final_model.rds")















# ### LOGISTIC REGRESSION ###
# set.seed(123)
# 
# logit_model <- glm(Anxiety ~ .,
#                    data = trainData,
#                    family = binomial(link = "logit"))
# 
# probabilities <- predict(logit_model, testData, type = "response")
# 
# # threshold = 0.5
# predictions <- ifelse(probabilities > 0.5,
#                       levels(testData$Anxiety)[2],
#                       levels(testData$Anxiety)[1])
# 
# predictions <- factor(predictions, levels = levels(testData$Anxiety))
# 
# conf_matrix <- confusionMatrix(predictions, testData$Anxiety)
# print(conf_matrix)
# conf_table <- conf_matrix$table
# TP <- conf_table[2, 2]  # True Positives (class "1")
# FP <- conf_table[2, 1]  # False Positives (class "1" predicted when actual "0")
# precision <- TP / (TP + FP)
# 
# # Print precision
# cat(sprintf("\nPrecision (TP/(TP+FP)): %.4f (%.1f%%)\n",
#             precision,
#             precision*100))
# 


# ### DECISION TREE ###
# library(rpart)
# 
# # Train Decision Tree Model
# dt_model <- rpart(Anxiety ~ .,
#                   data = trainData,
#                   method = "class",  # For classification
#                   control = rpart.control(
#                     minsplit = 20,    # Minimum number of observations in a node before splitting
#                     cp = 0.01,        # Complexity parameter
#                     maxdepth = 15      # Maximum depth of tree
#                   ))

# # Visualize the tree (optional)
# library(rpart.plot)
# rpart.plot(dt_model, main = paste("Decision Tree for Anxiety Prediction (Top", top_n_features, "Features)"))
# 
# # Predict on test data
# predictions <- predict(dt_model, testData, type = "class")
# 
# # Ensure predictions and test labels have the same factor levels
# predictions <- factor(predictions, levels = levels(testData$Anxiety))
# 
# # Print confusion matrix
# conf_matrix <- confusionMatrix(predictions, testData$Anxiety)
# print(conf_matrix)
# 
# conf_table <- conf_matrix$table
# TP <- conf_table["1", "1"]  # True Positives
# FP <- conf_table["1", "0"]  # False Positives
# precision <- TP / (TP + FP)
# 
# cat("\nPrecision (for class '1'):", round(precision, 4), "\n")
