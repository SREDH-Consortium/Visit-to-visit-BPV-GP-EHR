# Load libraries
library(pROC)
library(dplyr)
library(caret)

# Ensure reproducibility
set.seed(123)

# Split data
train_index <- createDataPartition(BPV_55_cph$outcome, p = 0.75, list = FALSE)
train_data <- BPV_55_cph[train_index, ]
test_data  <- BPV_55_cph[-train_index, ]

# Custom F1 score function
F1_score <- function(pred, actual) {
  TP <- sum(pred == 1 & actual == 1)
  FP <- sum(pred == 1 & actual == 0)
  FN <- sum(pred == 0 & actual == 1)
  if ((2 * TP + FP + FN) == 0) return(0)
  return((2 * TP) / (2 * TP + FP + FN))
}

# Generalized evaluation function
evaluate_model_with_bpv_cutoff <- function(formula, train_data, test_data, label) {
  # Fit model
  model <- glm(formula, data = train_data, family = "binomial")
  
  # Predict probabilities
  test_data <- test_data %>%
    mutate(pred_prob = predict(model, newdata = ., type = "response"))
  
  # ROC and best threshold (Youden)
  roc_obj <- roc(test_data$outcome, test_data$pred_prob)
  auc_val <- auc(roc_obj)
  opt_thresh_prob <- coords(roc_obj, "best", best.method = "youden", ret = "threshold")[[1]]
  
  # Classify
  test_data <- test_data %>%
    mutate(pred_class = if_else(pred_prob >= opt_thresh_prob, 1, 0))
  
  # Confusion matrix
  cm <- confusionMatrix(factor(test_data$pred_class), factor(test_data$outcome), positive = "1")
  
  # Get model coefficients
  coefs <- coef(model)
  beta_0 <- coefs[1]
  beta_1 <- coefs[2]
  
  # Calculate raw cutoff for BPV
  bpv_cutoff <- (log(opt_thresh_prob / (1 - opt_thresh_prob)) - beta_0) / beta_1
  
  # Custom F1 Score
  f1 <- F1_score(test_data$pred_class, test_data$outcome)
  
  # Report
  cat("\n---", label, "---\n")
  cat("Optimal predicted probability threshold:", round(opt_thresh_prob, 4), "\n")
  cat("Corresponding", names(coefs)[2], "cutoff (raw value):", round(bpv_cutoff, 2), "\n")
  cat("AUC:", round(auc_val, 4), "\n")
  cat("Accuracy:", round(cm$overall["Accuracy"], 4), "\n")
  cat("Sensitivity:", round(cm$byClass["Sensitivity"], 4), "\n")
  cat("Specificity:", round(cm$byClass["Specificity"], 4), "\n")
  cat("F1 Score:", round(f1, 4), "\n")
}

# Run models
evaluate_model_with_bpv_cutoff(outcome ~ sd_S, train_data, test_data, "Model: sd_S")
evaluate_model_with_bpv_cutoff(outcome ~ sd_D, train_data, test_data, "Model: sd_D")
evaluate_model_with_bpv_cutoff(outcome ~ cv_S, train_data, test_data, "Model: cv_S")
evaluate_model_with_bpv_cutoff(outcome ~ cv_D, train_data, test_data, "Model: cv_D")



