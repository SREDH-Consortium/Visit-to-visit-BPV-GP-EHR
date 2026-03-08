# Load necessary libraries
library(dplyr)
library(caret)
library(pROC)

# Ensure reproducibility
set.seed(123)

# Step 1: Split the data 75% train, 25% test
train_index <- createDataPartition(BPV_55$outcome, p = 0.75, list = FALSE)
train_data <- BPV_55[train_index, ]
test_data  <- BPV_55[-train_index, ]

# Step 2: Train a logistic regression model on the training set
# Using cv_S to predict outcome
model <- glm(outcome ~ cv_S, data = train_data, family = "binomial")
summary(model)  # Look at model estimates

# Step 3: Predict probabilities in the test set
test_data <- test_data %>%
  mutate(pred_prob = predict(model, newdata = test_data, type = "response"))

# Step 4: Create an ROC curve and identify the optimal cutoff 
roc_curve <- roc(test_data$outcome, test_data$cv_S)
# Using Youden's index to identify the best threshold
# Extract threshold value from the list
opt_thresh_val <- opt_thresh[[1]]  # or use opt_thresh$threshold if it's a named list

# Now you can safely print it
cat("Optimal cutoff for cv_S is:", opt_thresh_val, "\n")


# Optional: Evaluate classifier performance using the chosen cutoff
test_data <- test_data %>%
  mutate(pred_class = if_else(cv_S >= opt_thresh, 1, 0))

# Generate a confusion matrix
conf_matrix <- table(Observed = test_data$outcome, Predicted = test_data$pred_class)
print(conf_matrix)

# Calculate AUC as an overall performance measure
auc_val <- auc(roc_curve)
cat("AUC for the model is:", auc_val, "\n")




